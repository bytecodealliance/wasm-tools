;; --enable-gc

(assert_invalid
  (module
    (type $a (func))
    (type $b (sub final $a (func)))
    (type $c (sub $b (func))) ;; invalid
  )
  "subtype must match supertype"
)
(assert_invalid
  (module
    (type $a (func))
    (type $b (sub $a (struct))) ;; invalid
  )
  "subtype must match supertype"
)
(assert_invalid
  (module
    (type $a (func))
    (type $b (sub $a (func (param i32)))) ;; invalid
  )
  "subtype must match supertype"
)
(assert_invalid
  (module
    (type $a (struct i32))
    (type $b (sub $a (struct i64))) ;; invalid
  )
  "subtype must match supertype"
)
(assert_invalid
  (module
    (type $d (struct))
    (type $e (sub $d (struct (field (ref null $d)))))
    (type $f (sub $e (struct (field (ref $e)))))

    (type $g (func (param (ref $e)) (result (ref $e))))
    (type $i (sub $g (func (param (ref $f)) (result (ref $d))))) ;; invalid
  )
  "subtype must match supertype"
)
(assert_invalid
  (module
    (type $o (array i32))
    (type (sub $o (array (mut i32)))) ;; invalid
  )
  "subtype must match supertype"
)
(assert_invalid
  (module
    (type $o (array i32))
    (type (sub $o (array i64))) ;; invalid
  )
  "subtype must match supertype"
)
(assert_invalid
  (module
    (type $q (array (mut anyref)))
    (type $r (sub $q (array i31ref)))
    (type $s (sub $r (array (ref i31))))
    (type (sub $s (array (ref null i31)))) ;; invalid
  )
  "subtype must match supertype"
)
(assert_invalid
  (module
    (type $q (array (mut anyref)))
    (type $rr (sub $q (array arrayref)))
    (type $ss (sub $rr (array (ref array))))
    (type (sub $ss (array (ref null array)))) ;; invalid
  )
  "subtype must match supertype"
)
(assert_invalid
  (module
    (type $q (array (mut anyref)))
    (type $rrr (sub $q (array structref)))
    (type $sss (sub $rrr (array (ref struct))))
    (type (sub $sss (array (ref null struct)))) ;; invalid
  )
  "subtype must match supertype"
)
(assert_invalid
  (module
    (type $t (array (mut funcref)))
    (type $u (sub $t (array (ref null func))))
    (type (sub $u (array (mut (ref func))))) ;; invalid
  )
  "subtype must match supertype"
)
(assert_invalid
  (module
    (type $t (array (mut funcref)))
    (type $u (sub $t (array (ref null func))))
    (type (sub $u (array (ref null extern)))) ;; invalid
  )
  "subtype must match supertype"
)
(assert_invalid
  (module
    (type $t0 (array (mut externref)))
    (type $u0 (sub $t0 (array (ref null extern))))
    (type (sub $u0 (array (mut (ref extern))))) ;; invalid
  )
  "subtype must match supertype"
)
(assert_invalid
  (module
    (type $t0 (array (mut externref)))
    (type $u0 (sub $t0 (array (ref null extern))))
    (type $v0 (sub $u0 (array (ref extern))))
    (type (sub $v0 (array nullexternref))) ;; invalid
  )
  "subtype must match supertype"
)
(assert_invalid
  (module
    (type $t (array (mut funcref)))
    (type (sub $t (array nullexternref))) ;; invalid
  )
  "subtype must match supertype"
)
(assert_invalid
  (module
    (type $d (struct))
    (type $e (sub $d (struct (field (ref null $d)))))
    (type (sub $e (struct (field (ref 1000)))))
  )
  "type index out of bounds"
)
