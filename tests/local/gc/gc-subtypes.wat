;; --enable-gc

(module
  (type $a (func))
  (type $b (sub $a (func)))
  (type (sub final $a (func)))
)
