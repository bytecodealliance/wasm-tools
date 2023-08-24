(assert_invalid
  (component
    (type $t (instance))
    (import "a" (func (type $t)))
  )
  "type index 0 is not a function type")

(assert_invalid
  (component
    (core type $t (func))
    (import "a" (core module (type $t)))
  )
  "core type index 0 is not a module type")

(assert_invalid
  (component
    (type $t (func))
    (import "a" (instance (type $t)))
  )
  "type index 0 is not an instance type")

(assert_invalid
  (component
    (type $t (func))
    (type (component
      (import "a" (instance (type $t)))
    ))
  )
  "type index 0 is not an instance type")

(assert_invalid
  (component
    (core type $t (func))
    (type (component
      (import "a" (core module (type $t)))
    ))
  )
  "core type index 0 is not a module type")

(assert_invalid
  (component
    (type $t (instance))
    (type (component
      (import "a" (func (type $t)))
    ))
  )
  "type index 0 is not a function type")

(assert_invalid
  (component
    (export "a" (core module 0))
  )
  "module index out of bounds")

(assert_invalid
  (component
    (export "a" (instance 0))
  )
  "instance index out of bounds")

(assert_invalid
  (component
    (core type (module
      (export "a" (func (type 0)))
    ))
  )
  "type index out of bounds")

(assert_invalid
  (component
    (core type (module
      (export "a" (func))
      (export "a" (func))
    ))
  )
  "export name `a` already defined")

(assert_invalid
  (component
    (core type (module
      (import "" "" (func))
      (import "" "" (func))
    ))
  )
  "duplicate import name")

(assert_invalid
  (component
    (core type (module
      (import "" "" (memory 70000))
    ))
  )
  "memory size must be at most")

(assert_invalid
  (component
    (type (component
      (export "a" (func (type 0)))
    ))
  )
  "type index out of bounds")

(assert_invalid
  (component
    (type (component
      (export "a" (func))
      (export "A" (func))
    ))
  )
  "export name `A` conflicts with previous name `a`")

(assert_invalid
  (component
    (type (component
      (import "A" (func))
      (import "a" (func))
    ))
  )
  "import name `a` conflicts with previous name `A`")

(assert_invalid
  (component $c
    (core type $t (module
      (alias outer $c $t (type))
    ))
  )
  "unknown core type")

(assert_invalid
  (component $c
    (core type $t (module
      (alias outer $c 0 (type))
    ))
  )
  "type index out of bounds")

(assert_invalid
  (component $c
    (type $f (func))
    (core type $t (module
      (alias outer 100 0 (type))
    ))
  )
  "outer type aliases in module type declarations are limited to a maximum count of 1")

(assert_invalid
  (component $c
    (type $t (component
      (alias outer $c $t (type))
    ))
  )
  "unknown type")

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
      (core type (module
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
      (export "foo-BAR-baz" (func))
      (export "FOO-bar-BAZ" (func))
    ))
  )
  "export name `FOO-bar-BAZ` conflicts with previous name `foo-BAR-baz`")

(assert_invalid
  (component $c
    (type $t (instance
      (alias outer $c $t (type))
    ))
  )
  "unknown type")

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
      (core type (module
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

(component $C
  (core type $t (func))
  (core type (module
    (alias outer $C $t (type $a))
    (import "" "" (func (type $a)))
  ))
)

(component $C
  (component $C2
    (core type $t (func))
    (core type (module
      (alias outer $C2 $t (type $a))
      (import "" "" (func (type $a)))
    ))
  )
)

(assert_invalid
  (component $C
    (core type $t (func))
    (component $C2
      (core type (module
        (alias outer $C $t (type $a))
        (import "" "" (func (type $a)))
      ))
    )
  )
  "only the local or enclosing scope can be aliased"
)

(component
  (type (instance
    (type string)
    (export "a" (type (eq 0)))
  ))
)

(component
  (type (component
    (type string)
    (import "a" (type (eq 0)))
    (export "b" (type (eq 0)))
  ))
)

(assert_invalid
  (component
    (type (variant))
  )
  "variant type must have at least one case")

(assert_invalid
  (component
    (type (enum))
  )
  "enum type must have at least one variant")

(assert_invalid
  (component
    (type (record))
  )
  "record type must have at least one field")

(assert_invalid
  (component
    (type (flags))
  )
  "flags must have at least one entry")

(assert_invalid
  (component
    (type (tuple))
  )
  "tuple type must have at least one type")
