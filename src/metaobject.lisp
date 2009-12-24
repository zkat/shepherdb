(in-package :shepherdb)

(defproto =persistent-metaobject= =standard-metaobject=)

(defreply smop:allocate-object :before ((metaobject =persistent-metaobject=))
  (allocate-new-document-object))

(defreply smop:direct-property-value ((metaobject =persistent-metaobject=) object pname)
  (fetch-value-from-database (object-db object) object pname))

(defreply smop:add-direct-property ((metaobject =persistent-metaobject=) object property-name &key transientp)
  (if transientp
      (call-next-reply)
      (allocate-property-in-db (object-db object) object property-name)))

(defreply (setf smop:direct-property-value) (new-value (metaobject =persistent-metaobject=) object property-name &key)
  ;; todo - Sheeple needs to add property metaobjects so we can special-case this for transient and persistent props.
  (set-value-in-database (object-db object) object property-name new-value))

(defreply smop:property-makunbound ((mo =persistent-metaobject=) object property-name)
  (remove-key-from-document (object-db object) object property-name))

(defreply smop:remove-all-direct-properties ((mo =persistent-metaobject=) object)
  (wipe-object-db-keys (object-db object) object))

(defreply smop:direct-properties ((mo =persistent-metaobject=) object)
  (list-all-keys (object-db object) object))

(defreply smop:available-properties ((mo =persistent-metaobject=) object)
  (list-all-direct-and-delegated-keys (object-db object) object))
