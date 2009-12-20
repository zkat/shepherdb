(in-package :shepherdb)

(defproto =persistent-metaobject= =standard-metaobject=)

(defreply smop:allocate-object :before ((metaobject =persistent-metaobject=))
  (allocate-new-document-object))

(defreply smop:direct-property-value ((metaobject =persistent-metaobject=) object pname)
  (fetch-value-from-database (object-db object) object pname))

(defreply (setf smop:property-value) (new-value (metaobject =persistent-metaobject=)
                                      object property-name &key)
  (set-value-in-database (object-db object) object property-name new-value))

(defreply smop:property-makunbound ((mo =persistent-metaobject=) object property-name)
  (remove-key-from-document (object-db object) object property-name))

(defreply smop:remove-all-direct-properties ((mo =persistent-metaobject=) object)
  (wipe-object-db-keys (object-db object) object))

(defreply smop:direct-properties ((mo =persistent-metaobject=) object)
  (list-all-keys (object-db object) object))

(defreply smop:available-properties ((mo =persistent-metaobject=) object)
  (list-all-direct-and-delegated-keys (object-db object) object))
