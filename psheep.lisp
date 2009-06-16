;;;; This file is part of persistent-sheeple.

;;;; persistent-sheeple is free software: you can redistribute it and/or modify
;;;; it under the terms of the GNU Lesser General Public License as published by
;;;; the Free Software Foundation, either version 3 of the License, or
;;;; (at your option) any later version, with Franz's Preamble.

;;;; persistent-sheeple is distributed in the hope that it will be useful,
;;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;;; GNU Lesser General Public License for more details.

;;;; You should have received a copy of the GNU Lesser General Public License
;;;; along with persistent-sheeple.  If not, see <http://www.gnu.org/licenses/>.
;;;; The license, with the Franz preamble, LGPL additional permissions, and full GPL text,
;;;; should be in a file named COPYING in the root directory of these sources.
;;;;
;;;; TODO
;;;; - Try getting persistent-sheep objects to automatically load to the DB. Check the dumped data.
;;;; - Once that's done, use the dumped data to reliably reload the objects on startup.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package :persistent-sheeple)

;;;
;;; Database management
;;;
(defvar *sheep-db*)
(defmacro with-db (database &body body)
  `(with-connection (:db-name (db-name ,database) :host (host ,database) :port (port ,database))
     ,@body))
(defclass database ()
  ((host :accessor host :initform "localhost" :initarg :host)
   (port :accessor port :initform "5984" :initarg :port)
   (db-name :accessor db-name :initarg :db)))

(defun load-sheep-db (name &key (hostname "localhost") (port "5984"))
  (let ((db (make-database name :host hostname :port port)))
    (use-database db)
    (with-db db
      (unless (get-document "sheep-id-info" :if-missing nil)
        (create-sheep-id-document db))
      (load-sheeple-from-database db))
    (setf *sheep-db* db)
    db))

(defun make-database (db-name &key (host "localhost") (port "5984"))
  (let ((db (make-instance 'database :db db-name :host host :port port)))
    (create-db :db-name (db-name db)
               :if-exists :ignore)
    db))

(defun use-database (database)
  (set-connection :host (host database)
                  :db-name (db-name database)
                  :port (port database)))

(defun create-sheep-id-document (database)
  (with-db database
    (unless (get-document "sheep-id-info" :if-missing nil)
      (put-document (list (cons :max-sheep-id 0))
                    :id "sheep-id-info"))))

(defun max-sheep-id ()
  (with-db *sheep-db*
   (document-property :max-sheep-id (get-document "sheep-id-info"))))
(defun (setf max-sheep-id) (new-value)
  (create-sheep-id-document *sheep-db*)
  (with-db *sheep-db*
    (put-document 
     (set-document-property (get-document "sheep-id-info")
                            :max-sheep-id new-value)))
  new-value)

;;;
;;; Persistent-Sheeple
;;;
(defclass persistent-sheep (standard-sheep)
  ((db-id :accessor db-id :initarg :db-id :initform nil)))
(defmethod print-object ((sheep persistent-sheep) stream)
  (print-unreadable-object (sheep stream :identity t)
    (format stream "Persistent Sheep ID: ~A~@[ AKA: ~A~]" (db-id sheep) (sheep-nickname sheep))))

(defvar *all-sheep* nil
  "all the sheep objects that are currently loaded.")
(defun all-sheep ()
  *all-sheep*)

(defmethod initialize-instance :after ((sheep persistent-sheep))
  (allocate-sheep-in-database sheep *sheep-db*)
  (pushnew sheep *all-sheep*))

(defmacro pclone (sheeple properties &rest options)
  `(clone ,sheeple ,properties ,@options (:metaclass 'persistent-sheep)))
(defgeneric allocate-sheep-in-database (sheep database))
(defmethod allocate-sheep-in-database ((sheep persistent-sheep) (db database))
  (with-db db
    (assign-new-db-id sheep)
    (let* ((sheep-alist (sheep->alist sheep)))
      (put-document sheep-alist :id (db-id sheep)))))

(defun assign-new-db-id (sheep)
  (unless (db-id sheep)
    (setf (db-id sheep)
          (incf (max-sheep-id)))))

(defun find-sheep-with-id (db-id)
  (find-if (lambda (sheep)
             (= db-id (db-id sheep)))
           (all-sheep)))

;; How to rebuild a sheep object:
;;
;; Grab this stuff:
;; 1. direct-parents
;; 2. direct slot definitions
;; 3. metaclass
;; 4. nickname
;; 5. documentation
;; Then:
;; (spawn-sheep (list parent1 parent2)
;;              :metaclass metaclass
;;              :properties (list (list :name 'propname :value "value" :readers ...))
;;              :nickname nickname
;;              :documentation documentation)
;;
;; That should rebuild the whole thing.
;;
;; How to rebuild a sheep from a JSON entry:
;; 1. read in the JSON into a bunch of alists
;; 2. figure out all the sheeple that have no direct-parents (in which case they only have =dolly=)
;; 3. build those sheeple
;; 4. find all the sheeple that depend on those sheeple, and figure out the dependency graph from there
;; 5. sequentially rebuild all sheeple by just calling spawn-sheep

;;;
;;; Serialization (to alist, then JSON)
;;;
(defgeneric sheep->alist (sheep))
(defmethod sheep->alist ((sheep standard-sheep))
  (let ((parents (sheep-direct-parents sheep))
        (properties (loop for property in (sheep-direct-properties sheep)
                       collect (list (cons :name (property-spec-name property))
                                     (cons :value (property-spec-value property))
                                     (cons :readers (property-spec-readers property))
                                     (cons :writers (property-spec-writers property)))))
        (metaclass (class-name (class-of sheep)))
        (nickname (sheep-nickname sheep))
        (documentation (sheep-documentation sheep)))
    (list
     (cons :parents parents)
     (cons :properties properties)
     (cons :metaclass metaclass)
     (cons :nickname nickname)
     (cons :documentation documentation))))

(defmethod sheep->alist ((sheep persistent-sheep))
  (let ((parents (sheep-direct-parents sheep))
        ;; build an alist of alists by scanning the property-spec objects
        (properties (loop for property in (sheep-direct-properties sheep)
                       collect (list (cons :name (property-spec-name property))
                                     (cons :value (property-spec-value property))
                                     (cons :readers (property-spec-readers property))
                                     (cons :writers (property-spec-writers property)))))
        ;; for the class, we just store the symbol
        (metaclass (class-name (class-of sheep)))
        (nickname (sheep-nickname sheep))
        (documentation (sheep-documentation sheep)))
    (list
     ;; Because only persistent-sheeple can be turned to JSON, we can't involve standard-sheep
     ;; objects in this. Thus, we get rid of =dolly=, since we can assume she'll be added
     ;; upon sheep re-creation.
     (cons :parents (if (eql parents (list =dolly=))
                        nil parents))
     (cons :properties properties)
     (cons :metaclass metaclass)
     (cons :nickname nickname)
     (cons :documentation documentation))))

(defun document->sheep (doc-id database)
  "This function generates a new sheep object based on an ALIST definition. If the definition
includes reader/writer definitions, it will define new readers/writes for the new sheep object."
  (with-db database
    (let ((alist (get-document doc-id)))
     (let ((parents (cdr (assoc :parents alist)))
           (properties (cdr (assoc :properties alist)))
           (metaclass (find-class (read-from-string (cdr (assoc :metaclass alist)))))
           (nickname (cdr (assoc :nickname alist)))
           (dox (cdr (assoc :documentation alist))))
       (spawn-sheep parents
                    :metaclass metaclass
                    :properties (property-alist->property-definition properties)
                    :nickname nickname
                    :documentation dox)))))

(defun property-alist->property-definition (alist)
  (mapcar #'format-property-definition-for-defsheep alist))
(defun format-property-definition-for-defsheep (def)
  (let ((name (cdr (assoc :name def)))
        (value (cdr (assoc :value def)))
        (readers (cdr (assoc :readers def)))
        (writers (cdr (assoc :writers def))))
    `(:name ,name :value ,value
            ,@(when readers
                    `(:readers ,readers))
            ,@(when writers
                    `(:writers ,writers)))))

;;;
;;; PSHEEP property access
;;;
(defmethod sheeple:direct-property-value ((sheep persistent-sheep) property-name)
  (read-property-externally sheep property-name))

(defmethod (setf property-value) (new-value (sheep persistent-sheep) property-name)
  (write-property-externally sheep property-name new-value)
  new-value)

(defun read-property-externally (sheep pname)
  (multiple-value-bind (value hasp)
      (document-property pname (get-document (db-id sheep)))
    (if hasp
        (values (db-entry->lisp-object value) t)
        (values nil nil))))
 
(defun db-entry->lisp-object (entry)
  (if (db-pointer-p entry)
      (db-pointer->sheep entry)
      entry))

(defun write-property-externally (sheep pname new-value)
  (put-document
   (set-document-property (get-document (db-id sheep))
                          pname new-value)))

(defmethod clouchdb:encode ((sheep persistent-sheep) stream)
  (encode (sheep->db-pointer sheep) stream))

(defun sheep->db-pointer (sheep)
  (let ((alist (sheep->alist sheep)))
    (list
     (cons :%persistent-sheep-pointer (cdr (assoc :db-id alist))))))
(defun db-pointer-p (entry)
  (if (and (listp entry) (assoc :%persistent-sheep-pointer entry))
      t nil))
(defun db-pointer->sheep (pointer)
  (let ((db-id (cdr (assoc :%persistent-sheep-pointer pointer))))
    (find-sheep-with-id db-id)))
(defun valid-sheep-pointer-p (entry)
  (let ((got-it? (numberp (cdr (assoc :%persistent-sheep-pointer entry)))))
    (if got-it? t nil)))
(defun sheep-pointer-p (entry)
  (let ((got-it? (assoc :%persistent-sheep-pointer entry)))
    (if got-it? t nil)))
