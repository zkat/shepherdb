;;;; Basic database API
;;;;
(in-package :shepherdb)

(defproto =database= ()
  ((host "127.0.0.1")
   (port 5984)
   (name nil)))

(defun connect-to-db (name &key (host "127.0.0.1") (port 5984) (prototype =database=))
  (let ((db-reply (json:decode-json-from-string
                   (http-request (format nil "http://~A:~A/~A" host port name)))))
    (if (assoc :error db-reply)
        (error "Error: ~A, Reason: ~A" (cdr (assoc :error db-reply)) (cdr (assoc :reason db-reply)))
        (create prototype 'host host 'port port 'name name))))

(defun create-db (name &key (host "127.0.0.1") (port 5984) (prototype =database=))
  (let ((db-reply (json:decode-json-from-string
                   (http-request (format nil "http://~A:~A/~A" host port name)
                                 :method :put))))
    (if (assoc :error db-reply)
        (error "Error: ~A, Reason: ~A" (cdr (assoc :error db-reply)) (cdr (assoc :reason db-reply)))
        (create prototype 'host host 'port port 'name name))))

(defmessage get-document (db id)
  (:reply ((db =database=) id)
    (let ((db-reply (json:decode-json-from-string
                     (http-request (format nil "http://~A:~A/~A/~A"
                                           (host db) (port db) (name db) id)))))
      (if (assoc :error db-reply)
          (error "Error: ~A, Reason: ~A" (cdr (assoc :error db-reply)) (cdr (assoc :reason db-reply)))
          db-reply))))

(defmessage all-documents (db)
  (:reply ((db =database=))
    (let ((db-reply (json:decode-json-from-string
                     (http-request (format nil "http://~A:~A/~A/_all_docs"
                                           (host db) (port db) (name db))))))
      (if (assoc :error db-reply)
          (error "Error: ~A, Reason: ~A" (cdr (assoc :error db-reply)) (cdr (assoc :reason db-reply)))
          db-reply))))

(defmessage put-document (db doc)
  (:reply ((db =database=) doc)
    (let ((db-reply (json:decode-json-from-string
                     (http-request (format nil "http://~A:~A/~A/~A"
                                           (host db) (port db) (name db)
                                           (id doc)
                                           (revision doc))
                                   :method :put
                                   :content (json:encode-json-alist-to-string
                                             (remove :_id (remove :id doc :key 'car) :key 'car))))))
      (if (assoc :error db-reply)
          (error "Error: ~A, Reason: ~A" (cdr (assoc :error db-reply)) (cdr (assoc :reason db-reply)))
          db-reply))))

(defun id (doc)
  (or (cdr (assoc :id doc))
      (cdr (assoc :_id doc))))

(defun revision (doc)
  (or (cdr (assoc :_rev doc))
      (cdr (assoc :rev doc))))
