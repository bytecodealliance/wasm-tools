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

(assert_invalid
  (component
    (type (module
      (export "" (func (type 0)))
    ))
  )
  "type index out of bounds")

(assert_invalid
  (component
    (type (module
      (export "" (func))
      (export "" (func))
    ))
  )
  "export name `` already defined")

(assert_invalid
  (component
    (type (module
      (import "" "" (func))
      (import "" "" (func))
    ))
  )
  "duplicate import name")

(assert_invalid
  (component
    (type (module
      (import "" "" (memory 70000))
    ))
  )
  "memory size must be at most")

(assert_invalid
  (component
    (type (component
      (export "" (func (type 0)))
    ))
  )
  "type index out of bounds")

(assert_invalid
  (component
    (type (component
      (export "" (func))
      (export "" (func))
    ))
  )
  "export name `` already defined")

(assert_invalid
  (component
    (type (component
      (import "" (func))
      (import "" (func))
    ))
  )
  "duplicate import name")

(assert_invalid
  (component $c
    (type $t (component
      (alias outer $c $t (type))
    ))
  )
  "failed to find type named `$t`")

(assert_invalid
  (component $c
    (type $t (component
      (alias outer $c 0 (type))
    ))
  )
  "type index out of bounds")

(assert_invalid
  (component $c
    (type $f (func))
    (type $t (component
      (alias outer 100 0 (type))
    ))
  )
  "invalid outer alias count of 100")

(assert_invalid
  (component $c
    (type $f (func))
    (type $t (component
      (type (module
        (export "" (func))
        (export "" (func))
      ))
    ))
  )
  "name `` already defined")

(assert_invalid
  (component
    (type (instance
      (export "" (func (type 0)))
    ))
  )
  "type index out of bounds")

(assert_invalid
  (component
    (type (instance
      (export "" (func))
      (export "" (func))
    ))
  )
  "export name `` already defined")

(assert_invalid
  (component $c
    (type $t (instance
      (alias outer $c $t (type))
    ))
  )
  "failed to find type named `$t`")

(assert_invalid
  (component $c
    (type $t (instance
      (alias outer $c 0 (type))
    ))
  )
  "type index out of bounds")

(assert_invalid
  (component $c
    (type $f (func))
    (type $t (instance
      (alias outer 100 0 (type))
    ))
  )
  "invalid outer alias count of 100")

(assert_invalid
  (component $c
    (type $f (func))
    (type $t (instance
      (type (module
        (export "" (func))
        (export "" (func))
      ))
    ))
  )
  "name `` already defined")

(assert_invalid
  (component $c
    (type $f (func (param "" string)))
  )
  "function parameter name cannot be empty")

(component
  (type $t (func (result (tuple (list u8) u32))))
)
