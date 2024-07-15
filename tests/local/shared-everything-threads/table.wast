;; Check the `shared` attribute on tables.

(module
  ;; Imported.
  (table (import "spectest" "table_ref") shared 1 (ref null (shared func)))
  (table (import "spectest" "table_ref_with_max") shared 1 1 (ref null (shared func)))

  ;; Normal.
  (table shared 1 (ref null (shared func)))
  (table shared 1 1 (ref null (shared func)))

  ;; Inlined.
  (table shared (ref null (shared func)) (elem (ref.null (shared func))))
)

;; Note that shared elements can live within an unshared table.
(module
  (table (import "spectest" "table_ref") 1 (ref null (shared func)))
)

(assert_malformed
  (module quote "(table 1 shared funcref)")
  "unexpected token")

(assert_malformed
  (module quote "(table 1 funcref shared)")
  "unexpected token")

;; The proposal creates too much ambiguity to allow this syntax: the parser
;; would need to lookahead multiple tokens.
(assert_malformed
  (module quote "(table shared i64 (ref null (shared func)) (elem (ref.null (shared func))))")
  "unexpected token")

(assert_invalid
  (module (table (import "spectest" "table_ref") shared 0 funcref))
  "shared tables must have a shared element type")

(assert_invalid
  (module
    (type $t (func))
    (table shared 0 (ref $t)))
  "shared tables must have a shared element type")

;; Check `table.atomic.*` instructions.
(module (;eq;)
  (table $a (import "spectest" "table_eq") shared 1 (ref null (shared eq)))
  (table $b shared 1 (ref null (shared eq)))
  (func (export "table-atomic-get-eq-seq_cst-$a") (param $x i32) (result (ref null (shared eq)))
    local.get $x
    table.atomic.get seq_cst $a)
  (func (export "table-atomic-get-eq-seq_cst-$b") (param $x i32) (result (ref null (shared eq)))
    local.get $x
    table.atomic.get seq_cst $b)
  (func (export "table-atomic-get-eq-acq_rel-$a") (param $x i32) (result (ref null (shared eq)))
    local.get $x
    table.atomic.get acq_rel $a)
  (func (export "table-atomic-get-eq-acq_rel-$b") (param $x i32) (result (ref null (shared eq)))
    local.get $x
    table.atomic.get acq_rel $b)
  (func (export "table-atomic-set-eq-seq_cst-$a") (param $x i32) (param $y (ref null (shared eq)))
    local.get $x
    local.get $y
    table.atomic.set seq_cst $a)
  (func (export "table-atomic-set-eq-seq_cst-$b") (param $x i32) (param $y (ref null (shared eq)))
    local.get $x
    local.get $y
    table.atomic.set seq_cst $b)
  (func (export "table-atomic-set-eq-acq_rel-$a") (param $x i32) (param $y (ref null (shared eq)))
    local.get $x
    local.get $y
    table.atomic.set acq_rel $a)
  (func (export "table-atomic-set-eq-acq_rel-$b") (param $x i32) (param $y (ref null (shared eq)))
    local.get $x
    local.get $y
    table.atomic.set acq_rel $b)
  (func (export "table-atomic-rmw.xchg-eq-seq_cst-$a") (param $x i32) (param $y (ref null (shared eq))) (result (ref null (shared eq)))
    local.get $x
    local.get $y
    table.atomic.rmw.xchg seq_cst $a)
  (func (export "table-atomic-rmw.xchg-eq-seq_cst-$b") (param $x i32) (param $y (ref null (shared eq))) (result (ref null (shared eq)))
    local.get $x
    local.get $y
    table.atomic.rmw.xchg seq_cst $b)
  (func (export "table-atomic-rmw.xchg-eq-acq_rel-$a") (param $x i32) (param $y (ref null (shared eq))) (result (ref null (shared eq)))
    local.get $x
    local.get $y
    table.atomic.rmw.xchg acq_rel $a)
  (func (export "table-atomic-rmw.xchg-eq-acq_rel-$b") (param $x i32) (param $y (ref null (shared eq))) (result (ref null (shared eq)))
    local.get $x
    local.get $y
    table.atomic.rmw.xchg acq_rel $b)
  (func (export "table-atomic-rmw.cmpxchg-eq-seq_cst-$a") (param $x i32) (param $y (ref null (shared eq))) (param $z (ref null (shared eq))) (result (ref null (shared eq)))
    local.get $x
    local.get $y
    local.get $z
    table.atomic.rmw.cmpxchg seq_cst $a)
  (func (export "table-atomic-rmw.cmpxchg-eq-seq_cst-$b") (param $x i32) (param $y (ref null (shared eq))) (param $z (ref null (shared eq))) (result (ref null (shared eq)))
    local.get $x
    local.get $y
    local.get $z
    table.atomic.rmw.cmpxchg seq_cst $b)
  (func (export "table-atomic-rmw.cmpxchg-eq-acq_rel-$a") (param $x i32) (param $y (ref null (shared eq))) (param $z (ref null (shared eq))) (result (ref null (shared eq)))
    local.get $x
    local.get $y
    local.get $z
    table.atomic.rmw.cmpxchg acq_rel $a)
  (func (export "table-atomic-rmw.cmpxchg-eq-acq_rel-$b") (param $x i32) (param $y (ref null (shared eq))) (param $z (ref null (shared eq))) (result (ref null (shared eq)))
    local.get $x
    local.get $y
    local.get $z
    table.atomic.rmw.cmpxchg acq_rel $b)
)

(module (;any;)
  (table $a (import "spectest" "table_any") shared 1 (ref null (shared any)))
  (table $b shared 1 (ref null (shared any)))
  (func (export "table-atomic-get-any-seq_cst-$a") (param $x i32) (result (ref null (shared any)))
    local.get $x
    table.atomic.get seq_cst $a)
  (func (export "table-atomic-get-any-seq_cst-$b") (param $x i32) (result (ref null (shared any)))
    local.get $x
    table.atomic.get seq_cst $b)
  (func (export "table-atomic-get-any-acq_rel-$a") (param $x i32) (result (ref null (shared any)))
    local.get $x
    table.atomic.get acq_rel $a)
  (func (export "table-atomic-get-any-acq_rel-$b") (param $x i32) (result (ref null (shared any)))
    local.get $x
    table.atomic.get acq_rel $b)
  (func (export "table-atomic-set-any-seq_cst-$a") (param $x i32) (param $y (ref null (shared any)))
    local.get $x
    local.get $y
    table.atomic.set seq_cst $a)
  (func (export "table-atomic-set-any-seq_cst-$b") (param $x i32) (param $y (ref null (shared any)))
    local.get $x
    local.get $y
    table.atomic.set seq_cst $b)
  (func (export "table-atomic-set-any-acq_rel-$a") (param $x i32) (param $y (ref null (shared any)))
    local.get $x
    local.get $y
    table.atomic.set acq_rel $a)
  (func (export "table-atomic-set-any-acq_rel-$b") (param $x i32) (param $y (ref null (shared any)))
    local.get $x
    local.get $y
    table.atomic.set acq_rel $b)
  (func (export "table-atomic-rmw.xchg-any-seq_cst-$a") (param $x i32) (param $y (ref null (shared any))) (result (ref null (shared any)))
    local.get $x
    local.get $y
    table.atomic.rmw.xchg seq_cst $a)
  (func (export "table-atomic-rmw.xchg-any-seq_cst-$b") (param $x i32) (param $y (ref null (shared any))) (result (ref null (shared any)))
    local.get $x
    local.get $y
    table.atomic.rmw.xchg seq_cst $b)
  (func (export "table-atomic-rmw.xchg-any-acq_rel-$a") (param $x i32) (param $y (ref null (shared any))) (result (ref null (shared any)))
    local.get $x
    local.get $y
    table.atomic.rmw.xchg acq_rel $a)
  (func (export "table-atomic-rmw.xchg-any-acq_rel-$b") (param $x i32) (param $y (ref null (shared any))) (result (ref null (shared any)))
    local.get $x
    local.get $y
    table.atomic.rmw.xchg acq_rel $b)
  ;; table.atomic.rmw.cmpxchg only works with subtypes of eqref.
)

;; Check that cmpxchg only works with eqref subtypes.
(assert_invalid
  (module
    (table $a shared 0 (ref null (shared any)))
    (func (param $x i32) (param $y (ref null (shared any))) (param $z (ref null (shared any))) (result (ref null (shared any)))
      local.get $x
      local.get $y
      local.get $z
      table.atomic.rmw.cmpxchg seq_cst $a))
  "invalid type")
