(in-package :shepherdb)

;;; utils
(defun strcat (string &rest more-strings)
  (apply 'concatenate 'string string more-strings))

;;;
;;; Status codes
;;;
(defparameter *status-codes*
  '((200 . :ok)
    (201 . :created)
    (202 . :accepted)
    (404 . :not-found)
    (409 . :conflict)
    (412 . :precondition-failed)
    (500 . :internal-server-error)))

;;;
;;; Conditions
;;;
(define-condition couchdb-error () ())

(define-condition unexpected-response (couchdb-error)
  ((status-code :initarg :status-code :reader error-status-code)
   (response :initarg :response :reader error-response))
  (:report (lambda (condition stream)
             (format stream "Unexpected response with status code: ~A~@
                             HTTP Response: ~A"
                     (error-status-code condition)
                     (error-response condition)))))

;;;
;;; Database errors
;;;
(define-condition database-error (couchdb-error)
  ((uri :initarg :uri :reader database-error-uri)))

(define-condition db-not-found (database-error)
  ()
  (:report (lambda (condition stream)
             (format stream "Database ~A not found." (database-error-uri condition)))))

(define-condition db-already-exists (database-error)
  ()
  (:report (lambda (condition stream)
             (format stream "Database ~A already exists." (database-error-uri condition)))))

;;;
;;; Document errors
;;;
(define-condition document-error (couchdb-error) ())

(define-condition document-not-found (document-error)
  ((id :initarg :id :reader document-404-id)
   (db :initarg :db :reader document-404-db))
  (:report (lambda (e s)
             (format s "No document with id ~S was found in ~A"
                     (document-404-id e)
                     (document-404-db e)))))

(define-condition document-conflict (document-error)
  ((conflicting-doc :initarg :doc :reader conflicting-document)
   (conflicting-doc-id :initarg :id :reader conflicting-document-id))
  (:report (lambda (e s)
             (format s "Revision for ~A conflicts with latest revision for~@
                        document with ID ~S"
                     (conflicting-document e)
                     (conflicting-document-id e)))))

;;;
;;; Server API
;;;
(defstruct server
  (host "127.0.0.1")
  (port 5984))

(defun server->url (server)
  (format nil "http://~A:~A/" (server-host server) (server-port server)))

(defun couch-request (server &key (uri "") (method :get) content parameters additional-headers
                      (external-format-out *drakma-default-external-format*))
  (multiple-value-bind (response status-code)
      (http-request (strcat (server->url server) uri)
                    :method method :content content
                    :external-format-out external-format-out
                    :content-type "application/json"
                    :parameters parameters
                    :additional-headers additional-headers)
    (values response (or (cdr (assoc status-code *status-codes* :test #'=))
                         ;; The code should never get here once we know all the
                         ;; status codes CouchDB might return.
                         (error "Unknown status code: ~A. HTTP Response: ~A"
                                status-code response)))))

(defun all-dbs (server)
  (couch-request server :uri "_all_dbs"))

(defun config-info (server)
  (couch-request server :uri "_config"))

(defun replicate (server source target)
  (couch-request server :uri (format nil "_replicate?source=~A&target=~A" source target) :method :post))

(defun stats (server)
  (couch-request server :uri "_stats"))

(defun active-tasks (server)
  (couch-request server :uri "_active_tasks"))

(defun get-uuids (server &key (number 10))
  (couch-request server :uri (format nil "_uuids?count=~A" number)))

;;;
;;; Basic database API
;;;
(defparameter +utf-8+ (make-external-format :utf-8 :eol-style :lf))

(defproto =database= ()
  ((server (make-server)) name db-namestring)
  :documentation
  "Base database prototype. These objects represent the information required in order to communicate
with a particular CouchDB database.")
(defmessage host (db)
  (:reply ((db =database=))
    (server-host (server db))))
(defmessage port (db)
  (:reply ((db =database=))
    (server-port (server db))))
;; These extra replies handle automatic caching of the db-namestring used by db-request.
(defreply db-namestring :around ((db =database=))
  (or (call-next-reply)
      (setf (db-namestring db) (db->url db))))
(defreply (setf server) :after (new-val (db =database=))
  (declare (ignore new-val))
  (setf (db-namestring db) nil))

(defmessage db->url (db)
  (:documentation "Converts the connection information in DB into a URL string.")
  (:reply ((db =database=))
    (strcat (server->url (server db)) (name db))))

;; TODO - CouchDB places restrictions on what sort of URLs are accepted, such as everything having
;;        to be downcase, and only certain characters being accepted. There is also special meaning
;;        behing the use of /, so a mechanism to escape it in certain situations would be good.
(defmessage db-request (db &key)
  (:documentation "Sends a CouchDB request to DB.")
  (:reply ((db =database=) &key (uri "") (method :get) content
           (external-format-out *drakma-default-external-format*)
           parameters additional-headers)
    (multiple-value-bind (response status-code)
        (couch-request (format nil "~A/~A" (db-namestring db) uri) :method method :content content
                       :external-format-out external-format-out
                       :parameters parameters
                       :additional-headers additional-headers)
      (values response status-code))))

(defmacro handle-request (result-var request &body expected-responses)
  (let ((status-code (gensym "STATUS-CODE-")))
    `(multiple-value-bind (,result-var ,status-code)
         ,request
       (case ,status-code
         ,@expected-responses
         (otherwise (error 'unexpected-response :status-code ,status-code :response ,result-var))))))

(defmessage db-info (db)
  (:documentation "Fetches info about a given database from the CouchDB server.")
  (:reply ((db =database=))
    (handle-request response (db-request db)
      (:ok response)
      (:internal-server-error (error "Illegal database name: ~A" (name db)))
      (:not-found (error 'db-not-found :uri (db-namestring db))))))

(defun connect-to-db (name &key (host "127.0.0.1") (port 5984) (prototype =database=))
  "Confirms that a particular CouchDB database exists. If so, returns a new database object
that can be used to perform operations on it."
  (let ((db (create prototype 'host host 'port port 'name name)))
    (when (db-info db)
      db)))

(defun create-db (name &key (host "127.0.0.1") (port 5984) (prototype =database=))
  "Creates a new CouchDB database. Returns a database object that can be used to operate on it."
  (let ((db (create prototype 'host host 'port port 'name name)))
    (handle-request response (db-request db :method :put)
      (:created db)
      (:internal-server-error (error "Illegal database name: ~A" name))
      (:precondition-failed (error 'db-already-exists :uri (db-namestring db))))))

(defun ensure-db (name &rest all-keys)
  "Either connects to an existing database, or creates a new one.
 Returns two values: If a new database was created, (DB-OBJECT T) is returned. Otherwise, (DB-OBJECT NIL)"
  (handler-case (values (apply #'create-db name all-keys) t)
    (db-already-exists () (values (apply #'connect-to-db name all-keys) nil))))

(defmessage delete-db (db &key)
  (:documentation "Deletes a CouchDB database.")
  (:reply ((db =database=) &key)
    (handle-request response (db-request db :method :delete)
      (:ok response)
      (:not-found (error 'db-not-found :uri (db-namestring db))))))

(defmessage compact-db (db)
  (:documentation "Triggers a database compaction.")
  (:reply ((db =database=))
    (handle-request response (db-request db :uri "_compact" :method :post)
      (:accepted response))))

(defmessage changes (db)
  (:documentation "Returns the changes feed for DB")
  (:reply ((db =database=))
    (handle-request response (db-request db :uri "_changes")
      (:ok response))))

;;;
;;; Documents
;;;
(defmessage get-document (db id)
  (:documentation "Returns an CouchDB document from DB as an alist.")
  (:reply ((db =database=) id)
    (handle-request response (db-request db :uri id)
      (:ok response)
      (:not-found (error 'document-not-found :db db :id id)))))

(defmessage all-documents (db &key)
  (:documentation "Returns all CouchDB documents in DB, in alist form.")
  (:reply ((db =database=) &key startkey endkey limit include-docs &aux params)
    (when startkey (push `("startkey" . ,(prin1-to-string startkey)) params))
    (when endkey (push `("endkey" . ,(prin1-to-string endkey)) params))
    (when limit (push `("limit" . ,(prin1-to-string limit)) params))
    (when include-docs (push `("include_docs" . "true") params))
    (handle-request response (db-request db :uri "_all_docs" :parameters params)
      (:ok response))))

(defmessage batch-get-documents (db &rest doc-ids)
  (:documentation "Uses _all_docs to quickly fetch the given DOC-IDs in a single request.")
  (:reply ((db =database=) &rest doc-ids)
    (handle-request response
        (db-request db :uri "_all_docs"
                    :parameters '(("include_docs" . "true"))
                    :method :post
                    :content (format nil "{\"foo\":[~{~S~^,~}]}" doc-ids))
      (:ok response))))

(defmessage put-document (db id doc &key)
  (:documentation "Puts a new document into DB, using ID.")
  (:reply ((db =database=) id doc &key batch-ok-p)
    (handle-request response
        (db-request db :uri id :method :put
                    :external-format-out +utf-8+
                    :content doc
                    :parameters (when batch-ok-p '(("batch" . "ok"))))
      ((:created :accepted) response)
      (:conflict (error 'document-conflict :id id :doc doc)))))

(defmessage delete-document (db id revision)
  (:documentation "Deletes an existing document.")
  (:reply ((db =database=) id revision)
    (handle-request response (db-request db :uri (format nil "~A?rev=~A" id revision) :method :delete)
      (:ok response))))

(defmessage copy-document (db from-id to-id &key)
  (:documentation "Copies a document's content in-database.")
  (:reply ((db =database=) from-id to-id &key revision)
    (handle-request response
        (db-request db :uri from-id :method :copy
                    :additional-headers `(("Destination" . ,to-id))
                    :parameters `(,(when revision `("rev" . ,revision))))
      (:created response))))
