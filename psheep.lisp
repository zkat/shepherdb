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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package :persistent-sheeple)

;;;
;;; Database management
;;;
(defvar *db*)
(defclass database ()
  ((host :accessor host :initarg :host)
   (port :accessor port :initarg :port)
   (db-name :accessor db-name :initarg :db)))

(define-condition invalid-db-error () ())

;;;
;;; Persistent-Sheeple
;;;
(defclass persistent-sheep (standard-sheep)
  ((db-id :accessor db-id :initarg :db-id)))

(defmethod initialize-instance :after ((sheep persistent-sheep) &key)
  (allocate-sheep-in-database sheep))
(defgeneric allocate-sheep-in-database (sheep database))
(defmethod allocate-sheep-in-database ((sheep persistent-sheep) (db database))
  (assign-new-db-id sheep)
  (let* ((sheep-alist (sheep->alist sheep)))
    (put-document sheep-alist :id (db-id sheep))))

(defun assign-new-db-id (sheep)
  ;;todo
  )
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
        (documentation (sheep-documentation sheep))
        (id (db-id sheep)))
    (list
     ;; Because only persistent-sheeple can be turned to JSON, we can't involve standard-sheep
     ;; objects in this. Thus, we get rid of =dolly=, since we can assume she'll be added
     ;; upon sheep re-creation.
     (cons :parents (if (eql parents (list =dolly=))
                        nil parents))
     (cons :properties properties)
     (cons :metaclass metaclass)
     (cons :nickname nickname)
     (cons :documentation documentation)
     (cons :db-id id))))

(defun alist->sheep (alist)
  "This function generates a new sheep object based on an ALIST definition. If the definition
includes reader/writer definitions, it will define new readers/writes for the new sheep object."
  (let ((parents (cdr (assoc :parents alist)))
        (properties (cdr (assoc :properties alist)))
        (metaclass (find-class (cdr (assoc :metaclass alist))))
        (nickname (cdr (assoc :nickname alist)))
        (dox (cdr (assoc :documentation alist))))
    (spawn-sheep parents
                 :metaclass metaclass
                 :properties (property-alist->property-definition properties)
                 :nickname nickname
                 :documentation dox)))

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
(defmethod direct-property-value ((sheep persistent-sheep) property-name)
  (read-property-externally sheep property-name))

(defmethod (setf property-value) (new-value (sheep persistent-sheep) property-name)
  (write-property-externally sheep property-name new-value))

(defun read-property-externally (sheep pname)
  (db-entry->lisp-object
   (document-property pname (get-document (db-id sheep)))))

(defun write-property-externally (sheep pname new-value)
  (let ((value (check-db-digestibility new-value)))
    (put-document
     (set-document-property (get-document (db-id sheep))
                            pname value)
     :id (db-id sheep))))

(defun check-db-digestibility (object)
  (etypecase object
    (persistent-sheep
     (sheep->db-pointer object))
    ((or string number keyword symbol)
     object)
    (list
     (mapcar #'check-db-digestibility object))
    (hash-table
     (loop for val being the hash-values of object
          do (check-db-digestibility val))
     (loop for key being the hash-keys of object
          do (check-db-digestibility key))
     object)
    (vector
     (map 'list #'check-db-digestibility object))
    (t
     (error "~A can't go into the db, sorry bro." object))))

(defun sheep->db-pointer (sheep)
  (let ((alist (sheep->alist sheep)))
    (list
     (cons (as-keyword-symbol "_persistent_sheep_pointer") (cdr (assoc :db-id alist))))))

(defun valid-sheep-pointer-p (entry)
  (let ((got-it? (cdr (assoc (as-keyword-symbol "_persistent_sheep_pointer") entry))))
    (if got-it? t nil)))
(defun sheep-pointer-p (entry)
  (let ((got-it? (assoc (as-keyword-symbol "_persistent_sheep_pointer") entry)))
    (if got-it? t nil)))
