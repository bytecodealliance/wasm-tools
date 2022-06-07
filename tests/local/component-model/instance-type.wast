;; instances
(component
  (type (instance))

  (type $foo (func))

  (type $t (func (result string)))

  (core type (module
    (type $local_type (func))
    ;; functions
    (export "a" (func))
    (export "b" (func $foo))
    (export "c" (func (@name "bar")))
    (export "d" (func $foo (@name "bar")))
    (export "e" (func (type $local_type)))
    (export "f" (func (param i32)))
    (export "g" (func (param i32) (result i32 i64)))
    (export "h" (func (type $local_type) (result i32)))

    ;; globals
    (export "i" (global i32))
    (export "j" (global $foo i32))
    (export "k" (global (mut i32)))

    ;; tables
    (export "l" (table 1 funcref))
    (export "m" (table $foo 1 funcref))

    ;; memory
    (export "n" (memory 1))
    (export "o" (memory $foo 1))
    (export "p" (memory 1 2))
    (export "q" (memory 1 2 shared))
  ))

  (type $outer (instance
    (type $local_type (func))
    ;; functions
    (export "a" (func))
    (export "a2" (func (type $local_type)))
    (export "b" (func $foo))
    (export "c" (func (@name "bar")))
    (export "d" (func $foo (@name "bar")))
    (export "e" (func (type $t)))
    (export "f" (func (param string)))
    (export "g" (func (param s32) (result u32)))
    (export "h" (func (type $t)))

    ;; components
    (type $component_type (component))
    (export "c1" (component))
    (export "c2" (component (import "" (func))))
    (export "c3" (component (export "" (func))))
    (export "c4" (component (type $component_type)))
    (export "c5" (component
      (type $nested_func_type (func))
      (alias outer $outer $local_type (type $my_type))
      (import "1" (func (type $nested_func_type)))
      (import "2" (component))
      (export "1" (func (type $my_type)))
      (export "2" (component))
    ))
  ))
)

;; expand inline types
(component
  (type (instance (export "" (instance))))
)

;; reference outer types
(component
  (type (instance
    (type $t (instance))
    (export "" (instance (type $t)))
  ))
  (type $x (instance))
  (type (instance (export "" (instance (type $x)))))
)

;; recursive
(component
  (type (instance (export "" (core module
    (type $functype (func))

    (export "a" (func))
    (export "b" (func (type 0)))
    (export "c" (func (param i32)))
    (export "d" (func (type $functype)))

    ;; globals
    (export "e" (global i32))
    (export "f" (global (mut i32)))

    ;; tables
    (export "g" (table 1 funcref))

    ;; memory
    (export "h" (memory 1))
    (export "i" (memory 1 2))
    (export "j" (memory 1 2 shared))
  ))))
)

;; modules
(component
  (core type (module))

  (core type $foo (module))

  (type $empty (func))
  (type $i (instance))

  (core type (module
    (type $empty (func))
    (import "" "a" (func))
    (import "" "b" (func (type $empty)))
    (import "" "c" (func (param i32)))
    (import "" "d" (func (param i32) (result i32)))

    (import "" "e" (global i32))
    (import "" "f" (memory 1))
    (import "" "g" (table 1 funcref))

    (export "a" (func))
    (export "b" (global i32))
    (export "c" (memory 1))
    (export "d" (table 1 funcref))

    (export "e" (func (type $empty)))
    (export "f" (func (param i32)))
  ))

  (type (component
    (import "a" (func))
    (import "b" (func (type $empty)))
    (import "c" (func (param s32)))
    (import "d" (func (param s32) (result s32)))

    (import "h" (instance))
    (import "i" (instance (type $i)))
    (import "j" (instance
      (export "a" (func))
      (export "b" (func (type $empty)))
      (export "c" (func (param s32)))
    ))

    (import "k" (core module))
    (import "l" (core module
      (type $empty (func))
      (import "" "a" (func (type $empty)))
      (import "" "b" (func (param i32)))
      (export "a" (func (type $empty)))
      (export "b" (func (param i32)))
    ))

    (export "a" (func))
    (export "e" (func (type $empty)))
    (export "f" (func (param s32)))

    (export "g" (instance
      (export "a" (func))
      (export "b" (func (type $empty)))
      (export "c" (func (param s32)))
    ))

    (export "h" (core module
      (type $empty (func))
      (import "" "a" (func (type $empty)))
      (import "" "b" (func (param i32)))
      (export "a" (func (type $empty)))
      (export "b" (func (param i32)))
    ))
  ))
)

(assert_invalid
  (component
    (type (instance
      (export "" (func))
      (export "" (func)))))
  "duplicate export name")

(assert_invalid
  (component
    (type $t (func))
    (type (instance
      (export "" (instance (type $t)))
    )))
  "type index 0 is not an instance type")

(assert_invalid
  (component
    (core type $t (func))
    (type (instance
      (export "" (core module (type $t)))
    )))
  "core type index 0 is not a module type")

(assert_invalid
  (component
    (type $t (func))
    (type (instance
      (export "" (core module (type $t)))
    )))
  "failed to find core type named `$t`")

(assert_invalid
  (component
    (type $t (record (field "a" string)))
    (type (instance
      (export "" (func (type $t)))
    )))
  "type index 0 is not a function type")

(assert_invalid
  (component
    (type $t (instance))
    (type (instance
      (export "" (func (type $t)))
    )))
  "type index 0 is not a function type")

(assert_invalid
  (component
    (type $t (instance))
    (type (instance
      (export "" (instance
        (export "" (func (type $t)))
      ))
    )))
  "type index 0 is not a function type")
