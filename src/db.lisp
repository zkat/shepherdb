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
  ((uri :initarg :uri :reader database-error-uri)))

(define-condition db-not-found (database-error)
  ()
  (:report (lambda (condition stream)
             (format stream "Database ~A not found." (database-error-uri condition)))))

(define-condition db-already-exists (database-error)
  ()
  (:report (lambda (condition stream)
             (format stream "Database ~A already exists." (database-error-uri condition)))))

;;; Document errors
(define-condition document-error (couchdb-error) ())

(define-condition document-not-found (document-error) ())

(define-condition document-conflict (document-error) ())

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
  :documentation
  "Base database prototype. These objects represent the information required in order to communicate
with a particular CouchDB database.")

(defmessage db->url (db)
  (:documentation "Converts the connection information in DB into a URL string.")
  (:reply ((db =database=))
    (with-properties (host port name) db
      (format nil "http://~A:~A/~A" host port name))))

;; TODO - CouchDB places restrictions on what sort of URLs are accepted, such as everything having
;;        to be downcase, and only certain characters being accepted. There is also special meaning
;;        behing the use of /, so a mechanism to escape it in certain situations would be good.
(defmessage db-request (db &key)
  (:documentation "Sends a CouchDB request to DB.")
  (:reply ((db =database=) &key (uri "") (method :get) content
           (external-format-out *drakma-default-external-format*)
           parameters additional-headers)
    (multiple-value-bind (response status-code)
        (http-request (format nil "~A/~A" (db->url db) uri) :method method :content content
                      :external-format-out external-format-out
                      :content-type "application/json"
                      :parameters parameters
                      :additional-headers additional-headers)
      (values response (or (cdr (assoc status-code *status-codes* :test #'=))
                           ;; The code should never get here once we know all the
                           ;; status codes CouchDB might return.
                           (error "Unknown status code: ~A. HTTP Response: ~A"
                                  status-code response))))))

(defmessage db-info (db)
  (:reply ((db =database=))
    (multiple-value-bind (response status-code) (db-request db)
      (case status-code
        (:ok db)
        (:not-found (error 'db-not-found :uri (db->url db)))
        (otherwise (error 'unexpected-response :status-code status-code :response response))))))

(defun connect-to-db (name &key (host "127.0.0.1") (port 5984) (prototype =database=))
  "Confirms that a particular CouchDB database exists. If so, returns a new database object
that can be used to perform operations on it."
  (let ((db (create prototype 'host host 'port port 'name name)))
    (when (db-info db)
      db)))

(defun create-db (name &key (host "127.0.0.1") (port 5984) (prototype =database=))
  "Creates a new CouchDB database. Returns a database object that can be used to operate on it."
  (let ((db (create prototype 'host host 'port port 'name name)))
    (multiple-value-bind (response status-code) (db-request db :method :put)
      (case status-code
        (:created db)
        (:precondition-failed (error 'db-already-exists :uri (db->url db)))
        (otherwise (error 'unexpected-response :status-code status-code :response response))))))

(defmessage delete-db (db &key)
  (:documentation "Deletes a CouchDB database.")
  (:reply ((db =database=) &key)
    (multiple-value-bind (response status-code) (db-request db :method :delete)
      (case status-code
        (:ok response)
        (:not-found (error 'db-not-found :uri (db->url db)))
        (otherwise (error 'unexpected-response :status-code status-code :response response))))))

(defmessage compact-db (db)
  (:reply ((db =database=))
    (multiple-value-bind (response status-code)
        (db-request db :uri "_compact" :method :post)
      (case status-code
        (:accepted response)
        (otherwise (error 'unexpected-response :status-code status-code :response response))))))

;;;
;;; Documents
;;;
(defmessage get-document (db id)
  (:documentation "Returns an CouchDB document from DB as an alist.")
  (:reply ((db =database=) id)
    (multiple-value-bind (response status-code) (db-request db :uri id)
      (case status-code
        (:ok response)
        (:not-found (error 'document-not-found))
        (otherwise (error 'unexpected-response :status-code status-code :response response))))))

(defmessage all-documents (db &key)
  (:documentation "Returns all CouchDB documents in DB, in alist form.")
  (:reply ((db =database=) &key startkey endkey limit)
    (let (params)
      (when startkey
        (push `("startkey" . ,(prin1-to-string startkey)) params))
      (when endkey
        (push `("endkey" . ,(prin1-to-string endkey)) params))
      (when limit
        (push `("limit" . ,(prin1-to-string limit)) params))
      (multiple-value-bind (response status-code)
          (db-request db :uri "_all_docs"
                      :parameters params)
        (case status-code
          (:ok response)
          (otherwise (error 'unexpected-response :status-code status-code :response response)))))))

(defmessage put-document (db id doc)
  (:documentation "Puts a new document into DB, using ID.")
  (:reply ((db =database=) id doc)
    (multiple-value-bind (response status-code)
        (db-request db :uri id :method :put
                    :external-format-out +utf-8+
                    :content doc)
      (case status-code
        (:created response)
        (:conflict (error 'document-conflict))
        (otherwise (error 'unexpected-response :status-code status-code :response response))))))

(defmessage delete-document (db id revision)
  (:documentation "Deletes an existing document.")
  (:reply ((db =database=) id revision)
    (multiple-value-bind (response status-code)
        (db-request db :uri (format nil "~A?rev=~A" id revision) :method :delete)
      (case status-code
        (:ok response)
        (otherwise (error 'unexpected-response :status-code status-code :response response))))))

(defmessage copy-document (db from-id to-id &key)
  (:reply ((db =database=) from-id to-id &key revision)
    (multiple-value-bind (response status-code)
        (db-request db :uri from-id :method :copy
                    :additional-headers `(("Destination" . ,to-id))
                    :parameters `(,(when revision `(("rev" . ,revision)))))
      (case status-code
        (:created response)
        (otherwise (error 'unexpected-response :status-code status-code :response response))))))
