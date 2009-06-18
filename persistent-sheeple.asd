(asdf:defsystem persistent-sheeple
  :version "0"
  :description "Keep your sheeple around for as long as you need them!"
  :maintainer "Kat <kzm@sykosomatic.org>"
  :author "Kat <kzm@sykosomatic.org>"
  :licence "LLGPL"
  :depends-on (sheeple clouchdb)
  :serial t
  :components
  ((:module "src"
            :serial t
            :components
            ((:file "packages")
             (:file "db")
             (:file "psheep")
             (:file "encoding")
             (:file "decoding")))))

