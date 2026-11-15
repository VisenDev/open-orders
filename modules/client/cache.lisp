(defpackage #:open-orders.client.cache
  (:use #:cl)
  (:export #:cached-fetch #:invalidate-cache #:clear-cache
           #:*max-age* #:*retry-after* #:*timeout*))
(in-package #:open-orders.client.cache)

;; Seconds. Separate from request keywords to avoid name collisions.
(defparameter *max-age* 30)
(defparameter *retry-after* 5)
(defparameter *timeout* 30)

(defstruct entry
  value
  ready-p
  error
  request
  (deadline 0)
  (next-fetch 0))

(defvar *cache* (make-hash-table :test #'equal))

(defun now ()
  (/ (get-internal-real-time)
     internal-time-units-per-second))

(defun clear-cache ()
  "Forget cached data and ignore outstanding responses."
  (clrhash *cache*))

(defun invalidate-cache ()
  "Refresh cached reads on their next lookup, retaining old data."
  (maphash
   (lambda (key entry)
     (declare (ignore key))
     (setf (entry-next-fetch entry) 0
           (entry-request entry) nil
           (entry-error entry) nil))
   *cache*))

(defun cached-fetch (endpoint &rest parameters)
  "Return DATA, STATUS (:LOADING, :READY, :ERROR), and ERROR."
  (let* ((encoded
           (open-orders.client.fetch:encode-parameters parameters))
         (key (cons (copy-seq endpoint) encoded))
         (entry (or (gethash key *cache*)
                    (setf (gethash key *cache*) (make-entry))))
         (time (now))
         (max-age *max-age*)
         (retry-after *retry-after*))

    (when (and (entry-request entry)
               (>= time (entry-deadline entry)))
      (setf (entry-request entry) nil
            (entry-error entry) "Request timed out."
            (entry-next-fetch entry) (+ time retry-after)))

    (when (and (not (entry-request entry))
               (>= time (entry-next-fetch entry)))
      (let ((request (gensym "REQUEST")))
        (setf (entry-request entry) request
              (entry-deadline entry) (+ time *timeout*))

        (labels
            ((current-p ()
               (and (eq entry (gethash key *cache*))
                    (eq request (entry-request entry))))

             (fail (error)
               (when (current-p)
                 (setf (entry-request entry) nil
                       (entry-error entry) error
                       (entry-next-fetch entry)
                       (+ (now) retry-after))))

             (complete (body code)
               (when (current-p)
                 (if (and (integerp code) (<= 200 code 299))
                     (handler-case
                         (let* ((*read-eval* nil)
                                (value
                                  (if (stringp body)
                                      (read-from-string body nil nil)
                                      body)))
                           (setf (entry-value entry) value
                                 (entry-ready-p entry) t
                                 (entry-error entry) nil
                                 (entry-request entry) nil
                                 (entry-next-fetch entry)
                                 (+ (now) max-age)))
                       (error (condition)
                         (fail condition)))
                     (fail (list :http-status code
                                 :response body))))))

          (handler-case
              (open-orders.client.fetch::%fetch
               endpoint encoded #'complete)
            (error (condition)
              (fail condition))))))

    (values (entry-value entry)
            (cond
              ((entry-ready-p entry) :ready)
              ((entry-request entry) (lui:request-frame) :loading)
              ((entry-error entry) :error)
              (t :loading))
            (entry-error entry))))
