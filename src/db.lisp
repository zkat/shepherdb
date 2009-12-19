(in-package :shepherdb)

;;;
;;; Status codes
;;;
(defparameter *status-codes* '((200 . :ok)
                               (201 . :created)
                               (202 . :accepted)
                               (404 . :not-found)
                               (409 . :conflict)
                               (412 . :precondition-failed)))

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

;;; Database errors
(define-condition database-error (couchdb-error)
  ((uri :initarg :uri :reader :database-error-uri)))

(define-condition db-not-found (database-error)
  ()
  (:report (lambda (condition stream)
             (format stream "Database ~A not found." (database-error-uri condition)))))

(define-condition db-already-exists (database-error)
  ()
  (:report (lambda (condition stream)
             (format stream "Database ~A already exists." (database-error-uri condition)))))

;;;
;;; Basic database API
;;;
(defmacro define-constant (name value &optional doc)
  "A version of DEFCONSTANT for /strict/ CL implementations."
  ;; See <http://www.sbcl.org/manual/Defining-Constants.html>
  `(defconstant ,name (if (boundp ',name) (symbol-value ',name) ,value)
     ,@(when doc (list doc))))
(define-constant +utf-8+ (make-external-format :utf-8 :eol-style :lf)
  "Default external format for document content.")

(defproto =database= ()
  ((host "127.0.0.1")
   (port 5984)
   (name nil))
  (:documentation
   "Base database prototype. These objects represent the information required in order to communicate
with a particular CouchDB database."))

(defmessage db->url (db)
  (:documentation "Converts the connection information in DB into a URL string.")
  (:reply ((db =database=))
    (with-properties (host port name) db
      (format nil "http://~A:~A/~A" host port name))))

(defmessage db-request (db &key)
  (:documentation "Sends a CouchDB request to DB.")
  (:reply ((db =database=) &key (uri "") (method :get) content)
  (let ((db-reply (json:decode-json-from-string
                     (http-request (format nil "~A/~A" (db->url db) uri)
                                   :method method
                                   :content content))))
      ;; TODO - This should use the status code instead of searching for :error.
      (if (assoc :error db-reply)
          (signal-couchdb-error db-reply)
          db-reply))))

(defun connect-to-db (name &key (host "127.0.0.1") (port 5984) (prototype =database=))
  "Confirms that a particular CouchDB database exists. If so, returns a new database object
that can be used to perform operations on it."
  (let ((db (create prototype 'host host 'port port 'name name)))
    (db-request db)
    db))

(defun create-db (name &key (host "127.0.0.1") (port 5984) (prototype =database=))
  "Creates a new CouchDB database. Returns a database object that can be used to operate on it."
  (let ((db (create prototype 'host host 'port port 'name name)))
    (db-request db :method :put)
    db))

(defmessage delete-db (db &key)
  (:documentation "Deletes a CouchDB database.")
  (:reply ((db =database=) &key)
    (db-request db :method :delete)))

;;;
;;; Documents
;;;
(defmessage get-document (db id)
  (:documentation "Returns an CouchDB document from DB as an alist.")
  (:reply ((db =database=) id)
    ;; TODO - because of the way ids and revisions can be inside the JSON object's body,
    ;;        the interface for this message should probably be reworked.
    (db-request db :uri id)))

(defmessage all-documents (db)
  (:documentation "Returns all CouchDB documents in DB, in alist form.")
  (:reply ((db =database=))
    (db-request db :uri "_all_docs")))

(defmessage put-document (db id doc)
  (:documentation "Puts a new document into DB, using ID.")
  (:reply ((db =database=) id doc)
    ;; TODO - because of the way ids and revisions can be inside the JSON object's body,
    ;;        the interface for this message should probably be reworked.
    (cdr (assoc :rev (db-request db :uri id :method :put
                                 :external-format-out +utf-8+
                                 :content (json:encode-json-alist-to-string doc))))))

(defmessage update-document (db id revision doc)
  (:documentation "Updates an existing document.")
  (:reply ((db =database=) id revision doc)
    ;; TODO - because of the way ids and revisions can be inside the JSON object's body,
    ;;        the interface for this message should probably be reworked.
    (cdr (assoc :rev (db-request db :uri (format nil "~A?rev=~A" id revision)
                                 :method :put :external-format-out +utf-8+
                                 :content (json:encode-json-alist-to-string doc))))))

(defmessage delete-document (db id revision)
  (:documentation "Deletes an existing document.")
  (:reply ((db =database=) id revision)
    ;; TODO - because of the way ids and revisions can be inside the JSON object's body,
    ;;        the interface for this message should probably be reworked.
    (cdr (assoc :rev (db-request db :uri (format nil "~A?rev=~A" id revision) :method :delete)))))
