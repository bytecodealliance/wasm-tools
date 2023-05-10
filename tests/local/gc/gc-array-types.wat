;; --enable-gc

(module
  (type $a (array i32))
  (type $b (array (mut i32)))
  (type $c (array (mut (ref null $b))))
)
