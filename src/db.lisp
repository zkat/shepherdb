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

;;; Juicy Parts
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
    (setf *sheep-db* db)
    (with-db db
      (reload-sheeple-from-database db))
    (use-database db)
    db))