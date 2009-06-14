(in-package :persistent-sheeple)

(defvar *db*)
(defclass database ()
  ((couch :accessor couch :initarg :couch)
   (db-name :accessor db-name :initarg :db)))

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
(defmethod initialize-instance :around ((sheep persistent-sheep) &key)
  (unless (validate-database *db*)
    (call-next-method)
    (allocate-sheep-in-database sheep *db*)))


;; What sheeple needs in order to make a sheep that works the same:
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
;; That also means I should probably store property definitions somewhere.
;;
;; How to rebuild a sheep from a JSON entry:
;; 1. read in the JSON into protosheep objects/alists/whatever
;; 2. figure out all the sheeple that have no direct-parents (in which case they only have =dolly=)
;; 3. build those sheeple
;; 4. find all the sheeple that depend on those sheeple, and figure out the dependency graph from there
;; 5. sequentially rebuild all sheeple by just calling spawn-sheep

(defgeneric sheep->alist (sheep))
(defmethod sheep->alist ((sheep persistent-sheep))
  (let ((parents (sheep-direct-parents sheep))
        (properties (loop for property in (sheep-direct-properties sheep)
                       collect (list (cons :name (name property))
                                     (cons :value (value property))
                                     (cons :readers (readers property))
                                     (cons :writers (writers property)))))
        (metaclass (class-name (class-of sheep)))
        (nickname (sheep-nickname sheep))
        (documentation (sheep-documentation sheep)))
    (list
     (cons :parents (if (eql parents (list =dolly=))
                        nil parents))
     (cons :properties properties)
     (cons :metaclass metaclass)
     (cons :nickname nickname)
     (cons :documentation documentation))))

(defgeneric allocate-sheep-in-database (sheep database))
(defmethod allocate-sheep-in-database ((sheep persistent-sheep) (db database))
  #-(and) (let* ((sheep-alist (sheep->alist sheep))
         (db-representation (json:encode-json-alist-to-string sheep-alist)))
    (cl-couchdb:save-document (couch db) (db-name db) sheep-alist (sheep-id sheep))))

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
