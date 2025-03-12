;; RUN: wast --assert default --snapshot tests/snapshots %

;; --enable-gc

(module
  ;; When fields are mutable, a subtype's reference fields must be the exact
  ;; same the supertype's fields (i.e. are invariant).
  (type $a (sub    (struct (field (mut (ref null any))))))
  (type $b (sub $a (struct (field (mut (ref null any))))))

  ;; When fields are non-mutable, a subtype's reference fields can be subtypes
  ;; of the supertype's fields (i.e. are covariant).
  (type $c (sub    (struct (field (ref null any)))))
  (type $d (sub $c (struct (field (ref null none)))))
)
