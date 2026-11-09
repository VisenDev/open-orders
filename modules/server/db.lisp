(defpackage #:open-orders.server.db
  (:use #:cl #:open-orders.common.fn)
  (:shadow #:set #:get #:delete)
  (:export
   #:get
   #:set
   #:new
   #:connect
   #:disconnect
   #:*db*
   #:*default-database-path*
   #:null-entity
   #:delete
   #:get-all
   #:entity
   #:make-entity
   #:entity-p
   #:copy-entity
   #:entity-id))
(in-package #:open-orders.server.db)

(defvar *default-database-path* "open-orders.sqlite3")
(defvar *db* nil)

(fn (disconnect (values)) ()
  (when *db* (sqlite:disconnect *db*))
  (setf *db* nil)
  (values))

(fn (connect (values)) (&optional ((path (or pathname string))
                                   *default-database-path*))

  (when *db* (disconnect))
  (setf *db* (sqlite:connect path))

  (dolist (sql '("PRAGMA foreign_keys = ON;"
                 "create table if not exists entity(
                    id integer primary key
                 );"
                 "create table if not exists component(
                    name string not null,
                    value string,
                    entity integer references entity(id) on delete cascade,
                    primary key(name, entity)
                  );"))
    (sqlite:execute-non-query *db* sql))
  (values))

(defstruct entity id)

(fn (new entity) ()
  (let ((id (sqlite:execute-single
             *db* "insert into entity default values returning id;")))
    (make-entity :id id)))

(fn (delete (values)) ((entity entity))
  (sqlite:execute-non-query
   *db*
   "delete from entity where id = ?;"
   (entity-id entity))
  (setf (entity-id entity) nil)
  (values))

(define-condition null-entity (error) ())

(fn (set (values)) ((entity entity) (component keyword) (value t))
  (when (null (entity-id entity))
    (error (make-condition 'null-entity)))
  (sqlite:execute-non-query
   *db*
   "insert into component(name, value, entity) values(?, ?, ?);"
   (symbol-name component) (format nil "~S" value) (entity-id entity))
  (values))

(fn (get t) ((entity entity) (component keyword))
  (when (null (entity-id entity))
    (error (make-condition 'null-entity)))
  (let ((result
          (sqlite:execute-single
           *db*
           "select value from component where entity = ? and name = ?;"
           (entity-id entity) (symbol-name component))))
    (if (stringp result)
        (nth-value 0 (read-from-string result))
        result)))

(fn (get-all trivial-types:property-list) ((entity entity))
  (when (null (entity-id entity))
    (error (make-condition 'null-entity)))
  (let ((result
          (sqlite:execute-to-list
           *db*
           "select name,value from component where entity = ?;" (entity-id entity))))
    (loop :for (key value) :in result
          :appending (list (find-symbol key '#:keyword)
                           (if (stringp value)
                               (nth-value 0 (read-from-string value))
                               value)))))
