;; FIXME(#590) these should be invalid
(; (assert_invalid ;)
  (component
    (type $t (instance))
    (import "" (func (type $t)))
  )
  (; "type index is not a function") ;)

(; (assert_invalid ;)
  (component
    (type $t (instance))
    (import "" (module (type $t)))
  )
  (; "type index is not a module") ;)

(; (assert_invalid ;)
  (component
    (type $t (func))
    (import "" (instance (type $t)))
  )
  (; "type index is not an instance") ;)

(; (assert_invalid ;)
  (component
    (type $t (func))
    (type (component
      (import "" (instance (type $t)))
    ))
  )
  (; "type index is not an instance") ;)

(; (assert_invalid ;)
  (component
    (type $t (func))
    (type (component
      (import "" (module (type $t)))
    ))
  )
  (; "type index is not a module") ;)

(; (assert_invalid ;)
  (component
    (type $t (instance))
    (type (component
      (import "" (func (type $t)))
    ))
  )
  (; "type index is not a func") ;)

(assert_invalid
  (component
    (export "" (module 0))
  )
  "module index out of bounds")
(assert_invalid
  (component
    (export "" (instance 0))
  )
  "instance index out of bounds")
