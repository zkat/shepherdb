(in-package :persistent-sheeple)

(defvar *db*)
(defclass database ()
  ((couch :accessor couch :initarg :couch)
   (db :accessor db :initarg :db)))

(defun make-database (db-name &key (address "localhost") (port couch::*couchdb-port*))
  (make-instance 'database :db db-name :couch (couch:make-couch address port)))

(define-condition invalid-db-error () ())

(defgeneric validate-database (db))
(defmethod validate-database (db)
  (error "~A is not a database" db))
(defmethod validate-database ((db database))
  (if (and (activep db))
      t
      (error "DB ~A did not pass validation." db)))

(defclass persistent-sheep (sheeple:standard-sheep)
  ((persistent-properties :initform nil :accessor sheep-persistent-properties)))
(defmethod initialize-instance :around ((sheep persistent-sheep))
  (unless (validate-database *db*)
    (call-next-method)))

(defmethod sheeple:direct-property-value ((sheep persistent-sheep) property-name)
  (if (persistent-property-p sheep property-name)
      (persistent-property-value sheep property-name)
      (call-next-method)))

(defmethod (setf sheeple:property-value) (new-value (sheep persistent-sheep) property-name)
  (write-property-externally sheep property-name new-value))
(defgeneric persistent-property-p (sheep property-name))
(defmethod persistent-property-p ((sheep persistent-sheep) property-name)
  t)

(defun persistent-property-value (sheep property-name)
  (read-property-externally sheep property-name))