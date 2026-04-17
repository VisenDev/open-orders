(uiop:define-package #:cl-db.database
  (:use #:cl)
  (:import-from #:defclass-std
                #:defclass/std)
  (:export #:database
           #:dbget
           #:new-id
           #:insert
           #:serialize
           #:deserialize))
(in-package cl-db.database)

(defmethod marshal:class-persistent-slots ((serializable standard-object))
  (mapcar #'closer-mop:slot-definition-name
          (closer-mop:class-slots (class-of serializable))))

(defclass/std database ()
  ((id-counter :std 0 :type integer)
   (elements :std (make-hash-table))))

(defun dbget (database id)
  (gethash id (elements database)))
(defun (setf dbget) (value database id)
  (setf (gethash id (elements database)) value))
(defun new-id (database)
  (let ((id (id-counter database)))
    (incf (id-counter database))
    id))
(defun insert (database item)
  (let ((id (new-id database)))
    (setf (dbget database id) item)
    id))

(progn
  (cffi:defcfun fsync :int (fd :int))
  (cffi:defcfun fileno :int (stream :pointer))
  (cffi:defcfun fopen :pointer (path :string) (modes :string))
  (cffi:defcfun fputc :int (ch :int) (stream :pointer))
  (cffi:defcfun fclose :int (stream :pointer))
  (cffi:defcfun ("rename" frename) :int (oldpath :string) (newpath :string)))

(defun safe-overwrite-file (filepath string)
  "Overwrite file fully or not at all (atomically),
   in order to prevent corruption during a crash."
  (let* ((tmppath (concatenate 'string filepath ".temp"))
         (fp (fopen tmppath "wb")))
    (loop :for ch :across string
          :do (fputc (char-code ch) fp))
    (fsync (fileno fp))
    (frename tmppath filepath)
    (fclose fp)))

(defun serialize (database filepath)
  (safe-overwrite-file
   filepath
   (format nil "~S" (marshal:marshal database))))

(defun deserialize (filepath)
  (marshal:unmarshal
   (uiop:read-file-form filepath)))

#+nil
(defun test ()
  (let ((db (make-instance 'database)))
    (insert db "foo")
    (insert db "bar")
    (insert db "as;ldkfj;asldkjf")
    (serialize db "campro.database")
    ))



