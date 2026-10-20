;; RUN: wast --assert default --snapshot tests/snapshots % -f shared-everything-threads

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
  "expected a u64")

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
  (func (export "table-atomic-get-eq-seqcst-$a") (param $x i32) (result (ref null (shared eq)))
    local.get $x
    table.atomic.get seqcst $a)
  (func (export "table-atomic-get-eq-seqcst-$b") (param $x i32) (result (ref null (shared eq)))
    local.get $x
    table.atomic.get seqcst $b)
  (func (export "table-atomic-get-eq-acqrel-$a") (param $x i32) (result (ref null (shared eq)))
    local.get $x
    table.atomic.get acqrel $a)
  (func (export "table-atomic-get-eq-acqrel-$b") (param $x i32) (result (ref null (shared eq)))
    local.get $x
    table.atomic.get acqrel $b)
  (func (export "table-atomic-set-eq-seqcst-$a") (param $x i32) (param $y (ref null (shared eq)))
    local.get $x
    local.get $y
    table.atomic.set seqcst $a)
  (func (export "table-atomic-set-eq-seqcst-$b") (param $x i32) (param $y (ref null (shared eq)))
    local.get $x
    local.get $y
    table.atomic.set seqcst $b)
  (func (export "table-atomic-set-eq-acqrel-$a") (param $x i32) (param $y (ref null (shared eq)))
    local.get $x
    local.get $y
    table.atomic.set acqrel $a)
  (func (export "table-atomic-set-eq-acqrel-$b") (param $x i32) (param $y (ref null (shared eq)))
    local.get $x
    local.get $y
    table.atomic.set acqrel $b)
  (func (export "table-atomic-rmw.xchg-eq-seqcst-$a") (param $x i32) (param $y (ref null (shared eq))) (result (ref null (shared eq)))
    local.get $x
    local.get $y
    table.atomic.rmw.xchg seqcst $a)
  (func (export "table-atomic-rmw.xchg-eq-seqcst-$b") (param $x i32) (param $y (ref null (shared eq))) (result (ref null (shared eq)))
    local.get $x
    local.get $y
    table.atomic.rmw.xchg seqcst $b)
  (func (export "table-atomic-rmw.xchg-eq-acqrel-$a") (param $x i32) (param $y (ref null (shared eq))) (result (ref null (shared eq)))
    local.get $x
    local.get $y
    table.atomic.rmw.xchg acqrel $a)
  (func (export "table-atomic-rmw.xchg-eq-acqrel-$b") (param $x i32) (param $y (ref null (shared eq))) (result (ref null (shared eq)))
    local.get $x
    local.get $y
    table.atomic.rmw.xchg acqrel $b)
  (func (export "table-atomic-rmw.cmpxchg-eq-seqcst-$a") (param $x i32) (param $y (ref null (shared eq))) (param $z (ref null (shared eq))) (result (ref null (shared eq)))
    local.get $x
    local.get $y
    local.get $z
    table.atomic.rmw.cmpxchg seqcst $a)
  (func (export "table-atomic-rmw.cmpxchg-eq-seqcst-$b") (param $x i32) (param $y (ref null (shared eq))) (param $z (ref null (shared eq))) (result (ref null (shared eq)))
    local.get $x
    local.get $y
    local.get $z
    table.atomic.rmw.cmpxchg seqcst $b)
  (func (export "table-atomic-rmw.cmpxchg-eq-acqrel-$a") (param $x i32) (param $y (ref null (shared eq))) (param $z (ref null (shared eq))) (result (ref null (shared eq)))
    local.get $x
    local.get $y
    local.get $z
    table.atomic.rmw.cmpxchg acqrel $a)
  (func (export "table-atomic-rmw.cmpxchg-eq-acqrel-$b") (param $x i32) (param $y (ref null (shared eq))) (param $z (ref null (shared eq))) (result (ref null (shared eq)))
    local.get $x
    local.get $y
    local.get $z
    table.atomic.rmw.cmpxchg acqrel $b)
)

(module (;any;)
  (table $a (import "spectest" "table_any") shared 1 (ref null (shared any)))
  (table $b shared 1 (ref null (shared any)))
  (func (export "table-atomic-get-any-seqcst-$a") (param $x i32) (result (ref null (shared any)))
    local.get $x
    table.atomic.get seqcst $a)
  (func (export "table-atomic-get-any-seqcst-$b") (param $x i32) (result (ref null (shared any)))
    local.get $x
    table.atomic.get seqcst $b)
  (func (export "table-atomic-get-any-acqrel-$a") (param $x i32) (result (ref null (shared any)))
    local.get $x
    table.atomic.get acqrel $a)
  (func (export "table-atomic-get-any-acqrel-$b") (param $x i32) (result (ref null (shared any)))
    local.get $x
    table.atomic.get acqrel $b)
  (func (export "table-atomic-set-any-seqcst-$a") (param $x i32) (param $y (ref null (shared any)))
    local.get $x
    local.get $y
    table.atomic.set seqcst $a)
  (func (export "table-atomic-set-any-seqcst-$b") (param $x i32) (param $y (ref null (shared any)))
    local.get $x
    local.get $y
    table.atomic.set seqcst $b)
  (func (export "table-atomic-set-any-acqrel-$a") (param $x i32) (param $y (ref null (shared any)))
    local.get $x
    local.get $y
    table.atomic.set acqrel $a)
  (func (export "table-atomic-set-any-acqrel-$b") (param $x i32) (param $y (ref null (shared any)))
    local.get $x
    local.get $y
    table.atomic.set acqrel $b)
  (func (export "table-atomic-rmw.xchg-any-seqcst-$a") (param $x i32) (param $y (ref null (shared any))) (result (ref null (shared any)))
    local.get $x
    local.get $y
    table.atomic.rmw.xchg seqcst $a)
  (func (export "table-atomic-rmw.xchg-any-seqcst-$b") (param $x i32) (param $y (ref null (shared any))) (result (ref null (shared any)))
    local.get $x
    local.get $y
    table.atomic.rmw.xchg seqcst $b)
  (func (export "table-atomic-rmw.xchg-any-acqrel-$a") (param $x i32) (param $y (ref null (shared any))) (result (ref null (shared any)))
    local.get $x
    local.get $y
    table.atomic.rmw.xchg acqrel $a)
  (func (export "table-atomic-rmw.xchg-any-acqrel-$b") (param $x i32) (param $y (ref null (shared any))) (result (ref null (shared any)))
    local.get $x
    local.get $y
    table.atomic.rmw.xchg acqrel $b)
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
      table.atomic.rmw.cmpxchg seqcst $a))
  "invalid type")

(assert_invalid
  (module
    (table 1 funcref)
    (func
      i32.const 0
      table.atomic.get seqcst 0
    )
  )
  "invalid type: `table.atomic.get` only allows subtypes of `anyref`")

(assert_invalid
  (module
    (table 1 funcref)
    (func
      i32.const 0
      ref.null func
      table.atomic.set seqcst 0
    )
  )
  "invalid type: `table.atomic.set` only allows subtypes of `anyref`")

(assert_invalid
  (module
    (table 1 funcref)
    (func
      i32.const 0
      ref.null func
      table.atomic.rmw.xchg seqcst 0
    )
  )
  "invalid type: `table.atomic.rmw.xchg` only allows subtypes of `anyref`")
