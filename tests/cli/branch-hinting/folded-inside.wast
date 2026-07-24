;; RUN: wast --assert default,snapshot-folded --snapshot tests/snapshots %

;; Regression test for branch hint annotations placed *inside* a folded
;; instruction: between an `if`'s condition and its `(then`, and after a
;; `br_if`'s operand. This is the form older versions of this tool emitted when
;; printing folded, so it must keep parsing and attach the hint to the `if` /
;; `br_if` head instruction (not to a preceding operand). The folded printer now
;; normalizes these to an annotation preceding the instruction, so the
;; `.print-folded` snapshot shows the annotation moved out in front.

(module
  (func (param i32) (result i32)
    (if (result i32) (local.get 0)
      (@metadata.code.branch_hint "\01")
      (then (i32.const 1))
      (else (i32.const 0))))

  (func (param i32)
    (block
      (br_if 0 (local.get 0)
        (@metadata.code.branch_hint "\00")))))

;; A preceding annotation and a trailing one bind to the same folded
;; instruction, so using both is a duplicate (like two adjacent annotations).
(assert_malformed
  (module quote
    "(func (param i32) (result i32)"
      "(@metadata.code.branch_hint \"\\00\")"
      "(if (result i32) (local.get 0)"
        "(@metadata.code.branch_hint \"\\01\")"
        "(then (i32.const 1))"
        "(else (i32.const 0))))"
  )
  "duplicate annotation")
(assert_malformed
  (module quote
    "(func (param i32)"
      "(block"
        "(@metadata.code.branch_hint \"\\00\")"
        "(br_if 0 (local.get 0)"
          "(@metadata.code.branch_hint \"\\01\"))))"
  )
  "duplicate annotation")
