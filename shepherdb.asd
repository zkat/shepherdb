(in-package :cl-user)
(asdf:defsystem shepherdb
  :version "0"
  :description "Persistent prototype-based object store."
  :maintainer "Kat Marchán <zkat@Dagon>"
  :author "Kat Marchán <zkat@Dagon>"
  :licence "LLGPL"
  :depends-on (drakma sheeple cl-json flexi-streams)
  :serial t
  :components
  ((:module src
            :serial t
            :components
            ((:file "package")
             (:file "db")))))

