;;;; Basic database API
;;;;
(in-package :shepherdb)

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
   (name nil)))

(defmessage db->url (db)
  (:reply ((db =database=))
    (with-properties (host port name) db
      (format nil "http://~A:~A/~A" host port name))))

(define-condition couchdb-error ()
  ((name :initarg :name :reader error-name)
   (reason :initarg :reason :reader error-reason))
  (:report (lambda (condition stream)
             (format stream "Error: ~A, Reason: ~A"
                     (error-name condition)
                     (error-reason condition)))))

(defun signal-couchdb-error (error-msg)
  (error 'couchdb-error
         :name (cdr (assoc :error error-msg))
         :reason (cdr (assoc :reason error-msg))))

(defmessage db-request (db &key)
  (:reply ((db =database=) &key (uri "") (method :get) content)
  (let ((db-reply (json:decode-json-from-string
                     (http-request (format nil "~A/~A" (db->url db) uri)
                                   :method method
                                   :content content))))
      (if (assoc :error db-reply)
          (signal-couchdb-error db-reply)
          db-reply))))

(defun connect-to-db (name &key (host "127.0.0.1") (port 5984) (prototype =database=))
  (let ((db (create prototype 'host host 'port port 'name name)))
    (db-request db)
    db))

(defun create-db (name &key (host "127.0.0.1") (port 5984) (prototype =database=))
  (let ((db (create prototype 'host host 'port port 'name name)))
    (db-request db :method :put)
    db))

;;;
;;; Documents
;;;
(defun id (doc)
  (or (cdr (assoc :id doc))
      (cdr (assoc :_id doc))))

(defun revision (doc)
  (or (cdr (assoc :_rev doc))
      (cdr (assoc :rev doc))))

(defmessage get-document (db id)
  (:reply ((db =database=) id)
    (db-request db :uri id)))

(defmessage all-documents (db)
  (:reply ((db =database=))
    (db-request db :uri "_all_docs")))

(defmessage put-document (db id doc)
  (:reply ((db =database=) id doc)
    (db-request db :uri id
                :method :put
                :content (json:encode-json-alist-to-string doc)
                :external-format-out +utf-8+)))

(defmessage update-document (db id revision doc)
  (:reply ((db =database=) id revision doc)
    (db-request db :uri (format nil "~A?rev=~A" id revision)
                :method :put
                :content (json:encode-json-alist-to-string doc)
                :external-format-out +utf-8+)))
