;; instances
(module
  (type (instance))

  (type $foo (func))

  (type (func (result i32)))

  (type (instance
    ;; functions
    (export "a" (func))
    (export "b" (func $foo))
    (export "c" (func (@name "bar")))
    (export "d" (func $foo (@name "bar")))
    (export "e" (func (type 2)))
    (export "f" (func (param i32)))
    (export "g" (func (param i32) (result i32 i64)))
    (export "h" (func (type 2) (result i32)))

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
)

;; expand inline types
(module
  (type (instance (export "" (instance))))
)

;; reference outer types
(module
  (type (instance))
  (type (instance (export "" (instance (type 0)))))
  (type $x (instance))
  (type (instance (export "" (instance (type $x)))))
)

;; recursive
(module
  (type $functype (func))

  (type (instance (export "" (instance
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

    ;; instances
    (export "k" (instance))
  ))))
)

;; modules
(module
  (type (module))

  (type $foo (module))

  (type $empty (func))
  (type $i (instance))

  (type (module
    (import "" "a" (func))
    (import "" "b" (func (type $empty)))
    (import "" "c" (func (param i32)))
    (import "" "d" (func (param i32) (result i32)))

    (import "" "e" (global i32))
    (import "" "f" (memory 1))
    (import "" "g" (table 1 funcref))

    (import "" "h" (instance))
    (import "" "i" (instance (type $i)))
    (import "" "j" (instance
      (export "a" (func))
      (export "b" (func (type $empty)))
      (export "c" (func (param i32)))
    ))

    (import "" "k" (module))
    (import "" "l" (module
      (import "" "a" (func (type $empty)))
      (import "" "b" (func (param i32)))
      (export "a" (func (type $empty)))
      (export "b" (func (param i32)))
      (export "c" (module))
    ))

    (export "a" (func))
    (export "b" (global i32))
    (export "c" (memory 1))
    (export "d" (table 1 funcref))

    (export "e" (func (type $empty)))
    (export "f" (func (param i32)))

    (export "g" (instance
      (export "a" (func))
      (export "b" (func (type $empty)))
      (export "c" (func (param i32)))
    ))

    (export "h" (module
      (import "" "a" (func (type $empty)))
      (import "" "b" (func (param i32)))
      (export "a" (func (type $empty)))
      (export "b" (func (param i32)))
      (export "c" (module))
    ))
  ))
)

(assert_invalid
  (module
    (type (instance
      (export "" (func))
      (export "" (func)))))
  "duplicate export name")

(assert_invalid
  (module
    (type (func))
    (type (instance
      (export "" (instance (type 0)))
    )))
  "type index is not an instance")

(assert_invalid
  (module
    (type (module))
    (type (instance
      (export "" (instance (type 0)))
    )))
  "type index is not an instance")

(assert_invalid
  (module
    (type (func))
    (type (instance
      (export "" (module (type 0)))
    )))
  "type index is not a module")

(assert_invalid
  (module
    (type (instance))
    (type (instance
      (export "" (module (type 0)))
    )))
  "type index is not a module")

(assert_invalid
  (module
    (type (module))
    (type (instance
      (export "" (func (type 0)))
    )))
  "type index is not a function")

(assert_invalid
  (module
    (type (instance))
    (type (instance
      (export "" (func (type 0)))
    )))
  "type index is not a function")

(assert_invalid
  (module
    (type (instance))
    (type (instance
      (export "" (instance
        (export "" (func (type 0)))
      ))
    )))
  "type index is not a function")
