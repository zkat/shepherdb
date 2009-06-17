(defpackage #:persistent-sheeple
  (:use :cl :sheeple :clouchdb)
  (:nicknames :psheep)
  (:export
   ;; database
   :load-db
   ;; psheeple
   :*all-sheep*
   :persistent-sheep
   :pclone
   :print-object
   :find-sheep-with-id
   ;; property access
   :direct-property-value
   :property-value
   ))
