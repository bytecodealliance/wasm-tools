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
