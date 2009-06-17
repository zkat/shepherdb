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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package :persistent-sheeple)

;;;
;;; Database management
;;;
;;; TODO - I'm not sure how to guarantee that I'll always be accessing the same database.
;;;        It doesn't help that there's 3 ways to say which database is being used.

(defvar *sheep-db*) ; this holds the current database. It says which database to use.

(defclass database ()
  ((host :accessor host :initform "localhost" :initarg :host)
   (port :accessor port :initform "5984" :initarg :port)
   (db-name :accessor db-name :initarg :db)
   (max-sheep-id :accessor %max-sheep-id :initform 0)))

(defun make-database (db-name &key (host "localhost") (port 5984))
  (let ((db (make-instance 'database :db db-name :host host 
                           :port (etypecase port
                                   (number (format nil "~A" port))
                                   (string port)))))
    (create-db :db-name (db-name db)
               :if-exists :ignore)
    db))

(defun use-database (database) ; this sets the current database. It says which database to use.
  (set-connection :host (host database)
                  :db-name (db-name database)
                  :port (port database)))

(defun load-db (name &key (hostname "localhost") (port "5984"))
  "This function does a ton of shit! :D"
  (let ((db (make-database name :host hostname :port port)))
    #+leave-this-alone-for-now (with-db db
      (reload-sheeple-from-database db))
    (use-database db)
    (setf *sheep-db* db)
    db))

;;; Convenience
;; this sets the current database. It says which datab....... FFFFFFFFFFFUUUUUUUUCK!!!11one
(defmacro with-db (database &body body)
  `(with-connection (:db-name (db-name ,database) :host (host ,database) :port (port ,database))
     ,@body))

;; Because we -totally- know which database is the relevant one, this just grabs whatever is
;; in *sheep-db* DURR HURR
(defun max-sheep-id ()
  (%max-sheep-id *sheep-db*))
(defun (setf max-sheep-id) (new-value)
  (setf (%max-sheep-id *sheep-db*) new-value))

;;;
;;; Persistent-Sheeple
;;;
(defvar *all-sheep* nil
  "all the sheep objects that are currently loaded.")

(defclass persistent-sheep (standard-sheep)
  ((db-id :accessor db-id :initarg :db-id :initform nil))
  (:documentation "Persistent-sheep objects themselves are just like regular sheep, except they
contain a DB-ID 'metavalue', which is used to uniquely identify the object in a database."))

(defmethod initialize-instance :after ((sheep persistent-sheep) &key)
  "Every time a persistent-sheep object is created, we need to tell the database about it, and add
the object to *all-sheep* for easy access."
  (allocate-sheep-in-database sheep *sheep-db*)
  (pushnew sheep *all-sheep*))

(defgeneric allocate-sheep-in-database (sheep database))
(defmethod allocate-sheep-in-database ((sheep persistent-sheep) (db database))
  "This takes care of dumping a proper representation of SHEEP into DB, so we can reload it later."
  (with-db db
    (unless (db-id sheep)
     (assign-new-db-id sheep))
    (unless (get-document (db-id sheep) :if-missing nil)
      (let ((sheep-alist (sheep->alist sheep)))
        (put-document sheep-alist :id (db-id sheep))))))

(defun assign-new-db-id (sheep)
  (unless (db-id sheep)
    (setf (db-id sheep)
          (incf (max-sheep-id)))))

;;; Convenience
(defmacro pclone (sheeple properties &rest options)
  `(clone ,sheeple ,properties ,@options (:metaclass 'persistent-sheep)))

(defmethod print-object ((sheep persistent-sheep) stream)
  (print-unreadable-object (sheep stream :identity t)
    (format stream "Persistent Sheep ID: ~A~@[ AKA: ~A~]" (db-id sheep) (sheep-nickname sheep))))

(defun find-sheep-with-id (db-id)
  "Poor man's basic query function. ;)"
  (find-if (lambda (sheep)
             (= db-id (db-id sheep)))
           *all-sheep*))

;;;
;;; Serialization
;;;

;;; Encoding
;;
;; In order to successfully serialize a sheep, we need a little more than just an object's slots
;; based on which slots are transient/persistent. Because sheeple carry information about 
;; readers/writers, parent objects, nickname, metaclass, etc, we need to dump a lot more info.
;;
;; So, our approach here is to take a sheep object and serialize all the metaobject properties
;; necessary for rebuilding the sheep as it was. These are:
;; 1. Its direct-parents
;;    Of course, we can't just dump anything into here, since we need something serializable.
;;    For that reason, persistent-sheeple are only allowed to have =dolly= and other persistent-sheeple
;;    as their direct parents.
;;    We cheat a little when it comes to =dolly=: Since we know she'll be back as soon as any sheep
;;    is created, if the sheep object only has =dolly= as a direct-parent, we save that parents-list
;;    as NIL.
;;    Another problem arises when it comes to saving persistent-sheeple: psheeple can have pointers
;;    to other psheep objects inside. Our approach in this case is to save those pointers as JSON
;;    objects that represent psheep pointers. When we reload the database, we can find them by ID.
;;
;; 2. its direct property *definitions*
;;    Fortunately, Sheeple has an exported protocol for creating property-spec metaobjects.
;;    We use these metaobjects to find out what direct properties a sheep has, what their values
;;    are, and what readers/writers were defined (we'll need to redefine them on reload.)
;;    As mentioned before, psheeple objects here are serialized as pointers, not as full objects.
;;
;; 3. Metaclass
;;    Here, we just save the symbol of the metaclass (in this case, just 'persistent-sheep). The
;;    class should already be defined by the time a database is reloaded.
;;
;; 4. Nickname/documentation
;;    These are both just strings, and we can dump them into the database as-is.

(defgeneric sheep->alist (sheep)
  (:documentation "Returns an alist representation of SHEEP. The alist contains everything
Sheeple needs in order to reload an identical sheep object."))

(defmethod sheep->alist ((sheep standard-sheep))
  "This serializes a standard-sheep object. It's used mostly for testing, since the only sheep
that should actually be serialized are PERSISTENT-SHEEP objects."
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
  "Convert SHEEP to an appropriate representation that will then be converted to JSON, 
for the database to store."
  (let ((parents (sheep-direct-parents sheep))
        ;; build an alist of alists by scanning the property-spec objects
        (properties (loop for property in (sheep-direct-properties sheep)
                       collect (list (cons :name (property-spec-name property))
                                     (cons :value (property-spec-value property))
                                     (cons :readers (property-spec-readers property))
                                     (cons :writers (property-spec-writers property)))))
        ;; for the class, we just store the symbol
        (metaclass (class-name (class-of sheep)))
        ;; Nickname can either be a string or a symbol, documentation can be a string.
        ;; Both of them are serialized as strings.
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

(defmethod clouchdb:encode ((sheep persistent-sheep) stream)
  "This method tells clouchdb what it should actually encode when it runs into a persistent-sheep
object in one of its alists. For parent lists/property values, we only save a 'pointer' that
holds the unique ID of the sheep object in the database."
  (encode (sheep->db-pointer sheep) stream))

(defun sheep->db-pointer (sheep)
  "Sheep pointers, on the lisp side, are one-item alists with the :%persistent-sheep-object keyword
as a key. The CDR of the pointer cons is the unique database ID of the sheep object."
  (list
   (cons :%persistent-sheep-pointer (db-id sheep))))

;; The difference here is that db-pointer-p checks whether any arbitrary object is a pointer,
;; sheep-pointer-p checks that it's actually a sheep pointer (not just an alist), and 
;; valid-sheep-pointer-p checks that the pointer points to a sheep-id for an existing sheep object.
(defun db-pointer-p (entry)
  (if (and (listp entry) (assoc :%persistent-sheep-pointer entry))
      t nil))

(defun sheep-pointer-p (entry)
  (let ((got-it? (assoc :%persistent-sheep-pointer entry)))
    (if got-it? t nil)))

(defun valid-sheep-pointer-p (entry)
  (let ((sheep-id (cdr (assoc :%persistent-sheep-pointer entry))))
    (if (and (numberp sheep-id)
             (get-document sheep-id :if-missing nil))
        t nil)))

;;; Decoding
;;
;; How to rebuild a sheep from a db entry:
;; 1. read in all the sheep-definition alists
;; 2. topologically sort all the sheep-definitions based on parent dependencies, put them in a list
;; 3. Iterate over that list of definitions, creating new sheeple.
;;    It's important to note, at this step, that since calling SPAWN-SHEEP changes the value of
;;    MAX-SHEEP-ID, this number needs to be juggled. Also because of this, MAX-SHEEP-ID needs to
;;    manually be set to one number underneath the sheep about to be built.
;;    This might be complicated, since the topologically-sorted sheep definition list won't
;;    necessarily be in numerical order.
;;
;; Use this to actually rebuild them:
;; (spawn-sheep (list parent1 parent2)
;;              :metaclass metaclass
;;              :properties (list (list :name 'propname :value "value" :readers ...))
;;              :nickname nickname
;;              :documentation documentation)
;;

(defun document->sheep (doc-id database)
  "This function generates a new sheep object based on an ALIST definition. If the definition
includes reader/writer definitions, it will define new readers/writes for the new sheep object."
  (with-db database
    (let ((alist (get-document doc-id)))
      (alist->sheep alist doc-id))))

(defun alist->sheep (alist doc-id)
  "This function extracts all the necessary info from ALIST, and then calls SPAWN-SHEEP with the
corresponding arguments. Doing (alist->sheep (sheep->alist *sheep*)) should generate two objects
that are basically EQUAL (identical characteristics, not same object). This only applies, though,
if no messages have been defined on *sheep*, except for readers/writers provided to spawn-sheep."
  (let ((parents (cdr (assoc :parents alist)))
        (properties (cdr (assoc :properties alist)))
        (metaclass (find-class (read-from-string (cdr (assoc :metaclass alist)))))
        (nickname (cdr (assoc :nickname alist)))
        (dox (cdr (assoc :documentation alist))))
    (spawn-sheep parents
                 :metaclass metaclass
                 :properties (property-alist->property-definition properties)
                 :nickname nickname
                 :documentation dox
                 :doc-id doc-id)))

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

(defun reload-sheeple-from-database (database)
  ;; TODO: This is fine and dandy as a first step, but this needs to do a lot more
  ;;       (such as being sorted as a precedence list)
  (let ((sorted-spec (topologically-sort-sheep-specs (get-all-sheep-specs-from-db database))))
    (loop for spec in sorted-spec
       do (let ((db-id (read-from-string (cdr (assoc :|id| spec)))))
            ;; TODO - this is where I need to do the goddamn sheep-id juggling. Fucking hell.
            (loop until (= (1- db-id) (%max-sheep-id database))
               do (incf (%max-sheep-id database)))
            (document->sheep db-id database))))
  *all-sheep*)

(defun get-all-sheep-specs-from-db (database)
  "This gets the full spec for all existing sheeple entries in DATABASE."
  (with-db database
    (loop for row in (cdr (assoc :|rows| (get-all-documents)))
       collect (get-document (cdr (assoc :|id| row))))))

(defun topologically-sort-sheep-specs (specs)
  "This takes a list of sheep-specs and topologically sorts them according to parent-dependencies,
such that iterating over the list and calling alist->sheep on each item generates new sheep objects,
without any parent-dependency conflicts. This can be guaranteed to work because SHEEPLE does not
allow cyclic hierarchy lists."
  ;; This sucks. And it doesn't work. Did I mention it sucks?
  (let ((sorted ())
        (base-sheeple (remove-if (lambda (spec)
                                   (cdr (assoc :parents spec)))
                                 specs)))
    (loop for sheep in base-sheeple
       do (pushnew sheep sorted)
       (loop for other-sheep in specs
          do (loop for parent-spec in (cdr (assoc :parents other-sheep))
                if (= (cdr (assoc :%persistent-sheep-pointer parent-spec))
                      (cdr (assoc :|_id| sheep)))
                do (pushnew other-sheep base-sheeple))))
    sorted))

;;;
;;; Persistent property access.
;;;
;;
;; At this point, our biggest concern is how accessing properties works when
;; dealing with persistent sheep
;; Fortunately, the bulk of what we have to do is just make sure that when a direct-property
;; is accessed, it's read/written out to the database, not to the sheep object itself.
;;
(defmethod direct-property-value ((sheep persistent-sheep) property-name)
  "We redirect this method over to our function to read stuff externally."
  (read-property-externally sheep property-name))

(defmethod (setf property-value) (new-value (sheep persistent-sheep) property-name)
  "Here, we just write stuff out to the database and forget about handing it to the sheep object."
  (write-property-externally sheep property-name new-value)
  new-value)

(defun read-property-externally (sheep pname)
  "Get the property directly from the sheep document, using the SHEEP's ID. If it's there,
return the value, and T. If not, return NIL NIL."
  (multiple-value-bind (value hasp)
      (document-property pname (get-document (db-id sheep)))
    (if hasp
        ;; We have to make sure we don't return our clever little pointers.
        (values (db-entry->lisp-object value) t)
        (values nil nil))))

(defun db-pointer->sheep (pointer)
  "This is used whenever we try to fetch a property-value represented as a sheep pointer.
It doesn't actually do any loading/reloading of sheep -- instead, it just finds the appropriate
object in *all-sheep*"
  (let ((db-id (cdr (assoc :%persistent-sheep-pointer pointer))))
    (find-sheep-with-id db-id)))

(defun db-entry->lisp-object (entry)
  "This takes care of turning ENTRY into an actual sheep object, if it's a pointer, or
the object itself otherwise."
  (if (db-pointer-p entry)
      (db-pointer->sheep entry)
      entry)) 

(defun write-property-externally (sheep pname new-value)
  "We just put the altered document with the new property-value 
straight into the database. CLOUCHDB:ENCODE takes care of all the nasty details."
  (put-document
   (set-document-property (get-document (db-id sheep))
                          pname new-value)))

