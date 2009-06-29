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
;;; Decoding
;;;
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
      (alist->sheep alist))))

(defun get-sheep-from-db (pointer)
  (get-document (psheep-pointer-value pointer)))

(defun alist->sheep (alist)
  "This function extracts all the necessary info from ALIST, and then calls SPAWN-SHEEP with the
corresponding arguments. Doing (alist->sheep (sheep->alist *sheep*)) should generate two objects
that are basically EQUAL (identical characteristics, not same object). This only applies, though,
if no messages have been defined on *sheep*, except for readers/writers provided to spawn-sheep."
  (let ((parents (mapcar (lambda (pointer)
                           (find-sheep-with-id (psheep-pointer-value pointer)))
                         (cdr (assoc :parents alist))))
        (property-values (cdr (assoc :property-values alist)))
        (property-specs (cdr (assoc :property-specs alist)))
        (metaclass (find-class (read-from-string (cdr (assoc :metaclass alist)))))
        (nickname (cdr (assoc :nickname alist)))
        (dox (cdr (assoc :documentation alist)))
        (db-id (get-id-from-spec alist)))
    (spawn-sheep parents
                 :metaclass metaclass
                 :properties (property-alist->property-definition property-values property-specs)
                 :nickname nickname
                 :documentation dox
                 :db-id db-id)))

(defun property-alist->property-definition (values specs)
  (loop for spec in specs
     collect (let* ((pname (cdr (assoc :name spec)))
                    (readers (cdr (assoc :readers spec)))
                    (writers (cdr (assoc :writers spec)))
                    (value (cdr (assoc pname values))))
               `(:name ,(read-from-string pname) :value ,value
                       ,@(when readers
                               `(:readers ,readers))
                       ,@(when writers)))))

(defun reload-sheeple-from-database (database)
  "Called during LOAD-DB, this function reads all entries in DATABASE, and assumes they'll all be
sheeple. It then takes the specs and reloads them into actual sheep objects. It also takes care
of setting MAX-SHEEP-ID."
  (let ((sorted-spec-ids (topologically-sort-sheep-specs
                          (get-all-sheep-specs-from-db database))))
    (loop for id in sorted-spec-ids
       do (document->sheep id database)
         (when (> id (max-sheep-id))
           (setf (max-sheep-id) id))))
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
  (reverse (topological-sort (mapcar #'get-id-from-spec specs)
                             (remove-duplicates
                              (find-all-edges specs)))))

(defun get-id-from-spec (spec)
  (read-from-string (cdr (assoc :|_id| spec))))

(defun topological-sort (id-nums edges)
  "This topologically sorts ELEMENTS according to the constraints provided. When
things could go either way, TIE-BREAKER is used to pick which one we want."
  ;; This is taken straight from sheeple, which took it directly from Closette.
  ;; It also seems to infinitely loop. Whooooo
  (let ((remaining-edges edges)
        (remaining-nums id-nums)
        (result ())) 
    (loop
       (let ((minimal-nums
              (remove-if
               (lambda (id)
                 (member id remaining-edges
                         :key #'cadr))
               remaining-nums)))
         (when (null minimal-nums)
           (if (null remaining-nums)
               (return-from topological-sort result)
               (error "Inconsistent precedence graph.")))
         (let ((choice (car minimal-nums)))
           (setf result (append result (list choice)))
           (setf remaining-nums
                 (remove choice remaining-nums))
           (setf remaining-edges
                 (remove choice
                         remaining-edges
                         :test #'member)))))))

(defun find-all-edges (specs)
  (loop for spec in specs
     appending (loop for edge in (edges-of-spec spec)
                collect (list (get-id-from-spec spec)
                              edge))))

(defun edges-of-spec (spec)
  (loop for pointer in (cdr (assoc :parents spec))
       collect (cdr (assoc :%persistent-sheep-pointer pointer))))

