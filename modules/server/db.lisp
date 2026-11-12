(uiop:define-package #:open-orders.server.db
  (:use #:cl #:open-orders.common.fn)
  (:shadow #:set #:get #:delete #:log)
  (:export
   #:connect
   #:disconnect
   #:*db*
   #:*default-database-path*
   #:invalid-entity
   #:create
   #:delete
   #:query
   #:query-components
   #:get
   #:set
   #:get-components
   #:set-components
   #:entity
   #:make-entity
   #:entity-p
   #:copy-entity
   #:entity-id
   #:unset
   #:invalid-component
   #:user-password-correct-p
   #:user-create
   #:user-get-auth-token
   #:user-auth-token-valid-p
   #:user-update-password
   #:user-delete
   #:log
   #:user-lookup-auth-token-owner
   #:auth-token->username))
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
                  on component(name, entity);"
                 "create table if not exists user(
                    username string primary key not null,
                    hash string,
                    auth_token string
                  );"
                 "create table audit_log (
                    id integer primary key,
                    timestamp text not null
                    default (strftime('%Y-%m-%dT%H:%M:%fZ', 'now')),
                    username text,
                    ip_address text,
                    category text not null,
                    entity_id integer,
                    details text
                 );"
                 ))
    (sqlite:execute-non-query *db* sql))
  (values))

;;; ==== AUDIT ====
(fn (log (values)) (&key username (ip_address (hunchentoot:real-remote-addr))
                         category entity_id details)
  (sqlite:execute-non-query
   *db* "insert into audit_log(username, ip_address, category, entity_id, details)
         values(?, ?, ?, ?, ?);"
   username ip_address category entity_id details)
  (values))


;;; ==== USER ====

(fn (user-password-correct-p boolean) ((username string) (password string))
  (let ((hash (sqlite:execute-single
               *db* "select hash from user where username = ?;"
               username)))
    (unless hash (return-from user-password-correct-p nil))
    (cl-pass:check-password password hash)))

(fn (user-create (values)) ((username string) (password string))
  (sqlite:execute-non-query
               *db* "insert into user(username, hash) values(?, ?);"
               username (cl-pass:hash password))
  (values))

(fn (user-delete (values)) ((username string))
  (sqlite:execute-non-query
   *db* "delete from user where username = ?;" username)
  (values))

(fn (user-update-password (values)) ((username string) (new-password string))
  (sqlite:execute-non-query
   *db* "update user set hash = ? where username = ?;"
   (cl-pass:hash new-password)
   username)

  ;; update auth_token
  (sqlite:execute-non-query
   *db* "update user set auth_token = ? where username = ?;"
   (base64:usb8-array-to-base64-string
    (ironclad:random-data 32))
   username)
  (values))

(fn (user-get-auth-token (or string null)) ((username string) (password string))
  (when (user-password-correct-p username password)
    (let ((auth-token (base64:usb8-array-to-base64-string
                       (ironclad:random-data 32)) ))
      (sqlite:execute-non-query
       *db* "update user set auth_token = ? where username = ?;"
       auth-token username)
      auth-token)))

(fn (auth-token->username string) ((auth-token string))
  (sqlite:execute-single
   *db* "select username from user where auth_token = ?;" auth-token))

(fn (user-auth-token-valid-p boolean) ((auth-token string))
  (if (user-lookup-auth-token-owner auth-token) t nil))


;;; ==== ECS ====

(defstruct entity id)

(fn (create entity) ()
  (let ((id (sqlite:execute-single
             *db* "insert into entity default values returning id;")))
    (make-entity :id id)))

(fn (delete (values)) ((entity entity))
  (when (null (entity-id entity))
    (error 'invalid-entity))
  (sqlite:execute-non-query
   *db*
   "delete from entity where id = ?;"
   (entity-id entity))
  (setf (entity-id entity) nil)
  (values))

(define-condition invalid-entity (error) ())
(define-condition invalid-component (error) ())

(fn (set (values)) ((entity entity) (component keyword) (value t))
  (when (null (entity-id entity))
    (error (make-condition 'invalid-entity)))
  (when (eq component :id)
    (error (make-condition 'invalid-component)))
  (sqlite:execute-non-query
   *db*
   "insert into component(name, value, entity) values(?, ?, ?)
    on conflict(entity, name) do update set value = excluded.value;"
   (symbol-name component)
   (let ((*print-readably* t)
         (*print-circle* t)
         (*package* (find-package :keyword)))
     (write-to-string value))
   (entity-id entity))
  (values))

(fn (unset (values)) ((entity entity) (component keyword))
  (when (null (entity-id entity))
    (error (make-condition 'invalid-entity)))
  (sqlite:execute-non-query
   *db*
   "delete from component where entity = ? and name = ?;"
   (entity-id entity) (symbol-name component))
  (values))

(fn (get t) ((entity entity) (component keyword))
  (when (null (entity-id entity))
    (error (make-condition 'invalid-entity)))
  (let ((result
          (sqlite:execute-single
           *db*
           "select value from component where entity = ? and name = ?;"
           (entity-id entity) (symbol-name component))))
    (if (stringp result)
        (nth-value 0 (let ((*read-eval* nil)) (read-from-string result)))
        result)))

(fn (query (vector entity)) ((components (vector keyword)))
  (let* ((component-names-list
           (mapcar #'symbol-name (remove-duplicates (coerce components 'list))))
         (query-results
           (if (zerop (length component-names-list))
               (sqlite:execute-to-list *db* "select id from entity;")
               (apply
                #'sqlite:execute-to-list
                *db*
                (format nil "select entity from component where name in (~{~a~^,~}) 
                            group by entity having count(*) = ~a;"
                        (loop :repeat (length component-names-list)
                              :collect "?")
                        (length component-names-list))
                component-names-list)))
        (result (make-array 0 :element-type 'entity :fill-pointer 0 :adjustable t)))
    (declare (dynamic-extent query-results))
    (dolist (id query-results)
      (vector-push-extend (make-entity :id (first id)) result))
    result))

(fn (get-components trivial-types:property-list) ((entity entity))
  (when (null (entity-id entity))
    (error (make-condition 'invalid-entity)))
  (let* ((result
           (sqlite:execute-to-list
            *db*
            "select name,value from component where entity = ?;"
            (entity-id entity)))
         (plist
           (loop :for (key value) :in result
                 :appending (list (find-symbol (string-upcase key) :keyword)
                                  (if (stringp value)
                                      (nth-value 0 (let ((*read-eval* nil))
                                                     (read-from-string value)))
                                      value)))))
    (setf (getf plist :id) (entity-id entity))
    plist))

(fn (set-components (values)) ((components trivial-types:property-list))
  (let* ((id (getf components :id)))
    (unless id
      (error 'invalid-entity))
    (sqlite:with-transaction *db*
      (loop :with entity = (make-entity :id id)
            :for (key value) :on components :by #'cddr
            :unless (eq key :id)
              :do (set entity key value))))
  (values))

(fn (query-components (vector trivial-types:property-list))
    ((components (vector keyword)))
  (loop :with entities = (query components)
        :with result = (make-array (length entities)
                                   :element-type 'trivial-types:property-list
                                   :fill-pointer 0
                                   :initial-element nil)
        :for entity :across entities
        :do (vector-push-extend (get-components entity) result)
        :finally (return result)))
