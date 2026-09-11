;; RUN: wast --assert default --snapshot tests/snapshots %

(module (@name ""))
(module (@name "a"))

(module
  (func $foo)
  (func (@name "foo"))
  (func $foo_1)
)

(module
  (func
    (local (@name "foo") i32)
    (local $foo i32)
    (local $foo_1 i32)
  )
)

(module
  (func (@name "")))

(module
  (func (local (@name "") i32)))

(module
  (type (@name "T") (func))
  (type (@name "T") (func (param i32)))
  (type (@name "S") (sub final (struct)))
  (rec
    (type (@name "R1") (struct
      (field (@name "f") i32)
      (field (@name "g") (ref null $r2))
    ))
    (type $r2 (@name "R2") (array (mut i8)))
  )
  (type (@name "R2") (struct (field (@name "f") i32)))
)

(module
  (import "" "" (func (@name "foo")))
  (import "" "" (table (@name "foo") 1 funcref))
)
