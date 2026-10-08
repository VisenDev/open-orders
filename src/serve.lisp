(defpackage #:open-orders.serve
  (:use #:cl
        #:open-orders.fn)
  (:export
   #:register-page
   #:static-serve-directory))
(in-package #:open-orders.serve)

(defvar *hunchentoot-dispatchers* (make-hash-table :test 'equal))
(fn (register-page t) ((url string) (callback (function () t)))
  "Registers a hunchentoot dispatcher match url"
  (let ((existing-dispatcher (gethash url *hunchentoot-dispatchers*)))
    (when (not (null existing-dispatcher))
      (setf hunchentoot:*dispatch-table*
            (delete existing-dispatcher hunchentoot:*dispatch-table*))))

  (let ((dispatcher (hunchentoot:create-prefix-dispatcher url callback)))
    (setf (gethash url *hunchentoot-dispatchers*) dispatcher)
    (push dispatcher hunchentoot:*dispatch-table*)))


(defmacro static-serve-directory (url-prefix dir-path
                                  &optional
                                    (content-type "text/plain"))
  "Statically serves the files in directory under the url-prefix"
  (assert (stringp url-prefix))
  (assert (stringp dir-path))
  (assert (char= #\/ (aref url-prefix (1- (length url-prefix)))))
  (assert (char= #\/ (aref url-prefix 0)))
  (let ((files (uiop:directory-files (asdf:system-relative-pathname
                                      "open-orders"
                                      dir-path))))
    (cons 'progn
          (loop
            :for file :in files
            :collect
            `(register-page
              ,(concatenate 'string url-prefix
                            (pathname-name file)
                            "."
                            (pathname-type file))
              (lambda ()
                (setf (hunchentoot:content-type*) ,content-type)
                ,(with-open-file (stream file :element-type '(unsigned-byte 8))
                   (let ((bytes (make-array (file-length stream)
                                            :element-type '(unsigned-byte 8))))
                     (read-sequence bytes stream) bytes))))))))


;; STATIC SERVE JAVASCRIPT
(static-serve-directory "/js/" "src/js" "text/javascript")
