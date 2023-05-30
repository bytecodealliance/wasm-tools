;; --enable-gc

(assert_invalid
  (module
    (type $a (func))
    (type $b (sub final $a (func)))
    (type $c (sub $b (func)))
  )
  "supertype must not be final"
)
(assert_invalid
  (module
    (type $a (func))
    (type $b (sub $a (struct)))
  )
  "subtype must match supertype"
)
(assert_invalid
  (module
    (type $a (func))
    (type $b (sub $a (func (param i32))))
  )
  "subtype must match supertype"
)
(assert_invalid
  (module
    (type $a (struct i32))
    (type $b (sub $a (struct i64)))
  )
  "subtype must match supertype"
)
