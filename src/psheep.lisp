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
  (setf *all-sheep* (append (list sheep) *all-sheep*)))

(defmethod finalize-sheep :after ((sheep persistent-sheep))
  (allocate-sheep-in-database sheep *sheep-db*))

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

(defmethod (setf sheeple:sheep-nickname) :after (new-nickname (sheep persistent-sheep))
  (let ((document (get-document (db-id sheep))))
    (put-document 
     (set-document-property document :nickname new-nickname))))
(defmethod (setf sheeple:sheep-documentation) :after (new-dox (sheep persistent-sheep))
  (let ((document (get-document (db-id sheep))))
    (put-document
     (set-document-property document :documentation new-dox))))