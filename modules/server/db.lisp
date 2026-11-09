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
   #:entity-id
   #:match
   #:unset))
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
                    entity integer not null references entity(id) on delete cascade,
                    primary key(entity, name)
                  );"
                 "create index if not exists component_name_idx
                  on component(name, entity);"))
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

(fn (set (values)) ((entity entity) (component string) (value t))
  (when (null (entity-id entity))
    (error (make-condition 'null-entity)))
  (sqlite:execute-non-query
   *db*
   "insert into component(name, value, entity) values(?, ?, ?)
    on conflict(entity, name) do update set value = excluded.value;"
   component
   (let ((*print-readably* t)
         (*print-circle* t)
         (*package* (find-package :keyword)))
     (write-to-string value))
   (entity-id entity))
  (values))

(fn (unset (values)) ((entity entity) (component string))
  (when (null (entity-id entity))
    (error (make-condition 'null-entity)))
  (sqlite:execute-non-query
   *db*
   "delete from component where entity = ? and name = ?;"
   (entity-id entity) component)
  (values))

(fn (get t) ((entity entity) (component string))
  (when (null (entity-id entity))
    (error (make-condition 'null-entity)))
  (let ((result
          (sqlite:execute-single
           *db*
           "select value from component where entity = ? and name = ?;"
           (entity-id entity) component)))
    (if (stringp result)
        (nth-value 0 (let ((*read-eval* nil)) (read-from-string result)))
        result)))

(fn (match (vector entity)) ((components (vector string)))
  (let ((query-results
          (if (zerop (length components))
              (sqlite:execute-to-list *db* "select id from entity;")
              (apply
               #'sqlite:execute-to-list
               *db*
               (format nil "select entity from component where name in (~{~a~^,~}) 
                            group by entity having count(*) = ~a;"
                       (loop :repeat (length components)
                             :collect "?")
                       (length components))
               (remove-duplicates (coerce components 'list) :test #'string=))))
        (result (make-array 0 :element-type 'entity :fill-pointer 0 :adjustable t)))
    (declare (dynamic-extent query-results))
    (dolist (id query-results)
      (vector-push-extend (make-entity :id (first id)) result))
    result))

(fn (get-all list) ((entity entity))
  (when (null (entity-id entity))
    (error (make-condition 'null-entity)))
  (let ((result
          (sqlite:execute-to-list
           *db*
           "select name,value from component where entity = ?;" (entity-id entity))))
    (loop :for (key value) :in result
          :appending (list key
                           (if (stringp value)
                               (nth-value 0 (let ((*read-eval* nil))
                                              (read-from-string value)))
                               value)))))

(fn (match-get-all (vector list)) ((components (vector string)))
  (loop :with entities = (match components)
        :with result = (make-array (length entities)
                                   :element-type 'trivial-types:property-list
                                   :fill-pointer 0
                                   :initial-element nil)
        :for entity :across entities
        :do (vector-push-extend (get-all entity) result)
        :finally (return result)))
