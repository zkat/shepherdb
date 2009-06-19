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
     (cons :parents (remove =dolly= parents))
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
  (list (cons :%persistent-sheep-pointer (db-id sheep))))

;; The difference here is that db-pointer-p checks whether any arbitrary object is a pointer,
;; sheep-pointer-p checks that it's actually a sheep pointer (not just an alist), and 
;; valid-sheep-pointer-p checks that the pointer points to a sheep-id for an existing sheep object.
(defun psheep-pointer-value (ptr)
  (cdr (assoc :%persistent-sheep-pointer ptr)))

(defun db-pointer-p (entry)
  (if (and (listp entry) (assoc :%persistent-sheep-pointer entry))
      t nil))

(defun sheep-pointer-p (entry)
  (let ((got-it? (psheep-pointer-value entry)))
    (if got-it? t nil)))

(defun valid-sheep-pointer-p (entry)
  (let ((sheep-id (psheep-pointer-value entry)))
    (if (and (numberp sheep-id)
             (get-document sheep-id :if-missing nil))
        t nil)))

