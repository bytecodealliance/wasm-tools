;; --enable-gc

(module
  (type $a (func))
  (type $b (sub $a (func)))
  (type $c (sub $b (func)))
  (type (sub final $a (func)))

  (type $d (struct))
  (type $e (sub $d (struct (field (ref null $a)))))
  (type $f (sub $e (struct (field (ref $b)))))

  (type $g (func (param (ref $e)) (result (ref $e))))
  (type $h (sub $g (func (param (ref $d)) (result (ref $f)))))

  ;; (type $i (sub $g (func (param (ref $f)) (result (ref $d))))) ;; error

  (type $j (func (param (ref $b)) (result (ref $b))))
  (type $k (sub $j (func (param (ref $a)) (result (ref $c)))))

  ;; (type $l (sub $j (func (param (ref $c)) (result (ref $a))))) ;; error
)
