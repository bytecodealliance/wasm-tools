;; --enable-gc

(assert_invalid
  (module
    (type $a (func))
    (type $b (sub final $a (func)))
    (type $c (sub $b (func)))
  )
  "subtype must match supertype"
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
(assert_invalid
  (module
    (type $d (struct))
    (type $e (sub $d (struct (field (ref null $d)))))
    (type $f (sub $e (struct (field (ref $e)))))

    (type $g (func (param (ref $e)) (result (ref $e))))
    (type $i (sub $g (func (param (ref $f)) (result (ref $d))))) ;; error
  )
  "subtype must match supertype"
)
