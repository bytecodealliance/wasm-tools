;; --enable-gc

(module
  (type $a (func))
  (type $b (sub $a (func)))
  (type $c (sub $b (func)))
  (type (sub final $a (func)))
  ;; (type (sub $a $b (func))) ;; invalid: WAT parser error: "expected `(`"

  ;; struct, ref types, mutability, nullability
  (type $d (struct))
  (type $e (sub $d (struct (field (mut (ref null $d)))))) ;; width
  (type $f (sub final $e (struct (field (ref $e))))) ;; depth

  ;; func
  (type $g (func (param (ref $e)) (result (ref $e))))
  (type $h (sub $g (func (param (ref $d)) (result (ref $f)))))

  (type $j (func (param (ref $b)) (result (ref $b))))
  (type $k (sub $j (func (param (ref $a)) (result (ref $c)))))

  ;; valid: expanded param/result types are equal to those of the parent
  (type $l (sub $j (func (param (ref $c)) (result (ref $a)))))

  ;; array, val types, ref types, mutability, nullability
  (type $m (array (mut i32)))
  (type $n (sub $m (array i32)))

  (type $o (array i32))
  (type $p (sub $o (array i32)))

  (type $q (array i31ref))
  (type $r (sub $q (array (ref null i31))))
  (type $s (sub $r (array (ref i31))))

  (type $t (array funcref))
  (type $u (sub $t (array (ref null func))))
  (type $v (sub $u (array (ref func))))
  (type $w (sub $v (array (ref $a))))
  (type $x (sub $t (array (ref null $a))))
)
