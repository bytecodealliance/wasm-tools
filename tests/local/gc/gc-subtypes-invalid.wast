;; --enable-gc

(assert_invalid
  (module
    (type $a (func))
    (type $b (sub $a (func))) ;; invalid
  )
  "subtype must match supertype"
)
(assert_invalid
  (module
    (type $a (sub (func)))
    (type $b (sub final $a (func)))
    (type $c (sub $b (func))) ;; invalid
  )
  "subtype must match supertype"
)
(assert_invalid
  (module
    (type $a (sub (func)))
    (type $b (sub $a (struct))) ;; invalid
  )
  "subtype must match supertype"
)
(assert_invalid
  (module
    (type $a (sub (func)))
    (type $b (sub $a (func (param i32)))) ;; invalid
  )
  "subtype must match supertype"
)
(assert_invalid
  (module
    (type $a (sub (struct (field i32))))
    (type $b (sub $a (struct (field i64)))) ;; invalid
  )
  "subtype must match supertype"
)
(assert_invalid
  (module
    (type $d (sub (struct)))
    (type $e (sub $d (struct (field (ref null $d)))))
    (type $f (sub $e (struct (field (ref $e)))))

    (type $g (sub (func (param (ref $e)) (result (ref $e)))))
    (type $i (sub $g (func (param (ref $f)) (result (ref $d))))) ;; invalid
  )
  "subtype must match supertype"
)
(assert_invalid
  (module
    (type $o (sub (array i32)))
    (type (sub $o (array (mut i32)))) ;; invalid
  )
  "subtype must match supertype"
)
(assert_invalid
  (module
    (type $o (sub (array i32)))
    (type (sub $o (array i64))) ;; invalid
  )
  "subtype must match supertype"
)
(assert_invalid
  (module
    (type $q (sub (array (mut anyref))))
    (type $r (sub $q (array i31ref)))
    (type $s (sub $r (array (ref i31))))
    (type (sub $s (array (ref null i31)))) ;; invalid
  )
  "subtype must match supertype"
)
(assert_invalid
  (module
    (type $q (sub (array (mut anyref))))
    (type $rr (sub $q (array arrayref)))
    (type $ss (sub $rr (array (ref array))))
    (type (sub $ss (array (ref null array)))) ;; invalid
  )
  "subtype must match supertype"
)
(assert_invalid
  (module
    (type $q (sub (array (mut anyref))))
    (type $rrr (sub $q (array structref)))
    (type $sss (sub $rrr (array (ref struct))))
    (type (sub $sss (array (ref null struct)))) ;; invalid
  )
  "subtype must match supertype"
)
(assert_invalid
  (module
    (type $t (sub (array (mut funcref))))
    (type $u (sub $t (array (ref null func))))
    (type (sub $u (array (mut (ref func))))) ;; invalid
  )
  "subtype must match supertype"
)
(assert_invalid
  (module
    (type $t (sub (array (mut funcref))))
    (type $u (sub $t (array (ref null func))))
    (type (sub $u (array (ref null extern)))) ;; invalid
  )
  "subtype must match supertype"
)
(assert_invalid
  (module
    (type $t0 (sub (array (mut externref))))
    (type $u0 (sub $t0 (array (ref null extern))))
    (type (sub $u0 (array (mut (ref extern))))) ;; invalid
  )
  "subtype must match supertype"
)
(assert_invalid
  (module
    (type $t0 (sub (array (mut externref))))
    (type $u0 (sub $t0 (array (ref null extern))))
    (type $v0 (sub $u0 (array (ref extern))))
    (type (sub $v0 (array nullexternref))) ;; invalid
  )
  "subtype must match supertype"
)
(assert_invalid
  (module
    (type $t (sub (array (mut funcref))))
    (type (sub $t (array nullexternref))) ;; invalid
  )
  "subtype must match supertype"
)
(assert_invalid
  (module
    (type $d (sub (struct)))
    (type $e (sub $d (struct (field (ref null $d)))))
    (type (sub $e (struct (field (ref 1000)))))
  )
  "type index out of bounds"
)
(assert_invalid
  (module
    (type (struct (field $vt (mut i32)) (field $vt (mut i64))))
  )
  "duplicate identifier"
)
(assert_invalid
  (module
    (type $a (func)) ;; types without `(sub )` are considered final
    (type (sub $a (func)))
  )
  "subtype must match supertype"
)
