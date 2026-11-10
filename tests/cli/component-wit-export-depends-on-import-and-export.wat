;; FAIL: component wit %
;;
;; `c:d/k` is exported and uses `a:b/j`, which is only imported and uses
;; `a:b/i`. As `a:b/i` is also exported, `c:d/k` would see two of it.

(component
  (import "a:b/i" (instance $i (export "t" (type (sub resource)))))
  (alias export $i "t" (type $t))
  (import "a:b/j" (instance $j
    (alias outer 1 $t (type))
    (export "t" (type (eq 0)))
  ))
  (alias export $j "t" (type $jt))
  (import "c:d/k" (instance $k
    (alias outer 1 $jt (type))
    (export "t" (type (eq 0)))
  ))
  (export "a:b/i" (instance $i))
  (export "c:d/k" (instance $k))
)
