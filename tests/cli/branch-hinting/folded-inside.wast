;; RUN: wast --assert default --snapshot tests/snapshots %

;; A branch hint annotation must immediately precede the instruction it applies
;; to. Placing it *inside* a folded form is rejected: between an `if`'s condition
;; and its `(then`, or after a `br_if`'s operand. (Older versions of this tool
;; emitted and accepted that form; it is no longer accepted.)

(assert_malformed
  (module quote
    "(func (param i32) (result i32)"
      "(if (result i32) (local.get 0)"
        "(@metadata.code.branch_hint \"\\01\")"
        "(then (i32.const 1))"
        "(else (i32.const 0))))"
  )
  "must precede an instruction")

(assert_malformed
  (module quote
    "(func (param i32)"
      "(block"
        "(br_if 0 (local.get 0)"
          "(@metadata.code.branch_hint \"\\00\"))))"
  )
  "must precede an instruction")

;; A dangling annotation with no following instruction is likewise rejected.
(assert_malformed
  (module quote "(func (@metadata.code.branch_hint \"\\00\"))")
  "must precede an instruction")
