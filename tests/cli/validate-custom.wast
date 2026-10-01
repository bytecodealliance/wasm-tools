;; RUN: wast --assert default --snapshot tests/snapshots % -f custom-descriptors

(module
  (func (param i32)
    nop
    nop
    local.get 0
    (@metadata.code.branch_hint "\01") if end
    local.get 0
    (@metadata.code.branch_hint "\00") br_if 0)

  (func $dummy)
  (func (param i32 i32) (result i32)
    (@metadata.code.branch_hint "\00")
    (if (result i32) (local.get 0)
      (then
        (if (local.get 1) (then (call $dummy) (block) (nop)))
        (@metadata.code.branch_hint "\01")
        (if (result i32) (local.get 1)
          (then (call $dummy) (i32.const 9))
          (else (call $dummy) (i32.const 10))
        )
      )
      (else
        (@metadata.code.branch_hint "\00")
        (if (local.get 1) (then) (else (call $dummy) (block) (nop)))
        (i32.const 11)
      )
    )
  )
)

;; Function 5 doesn't exist.
(assert_invalid_custom
  (module
    (@custom "metadata.code.branch_hint" "\01\05\00")
  )
  "invalid function index 5 in branch hint section")

;; Function 0xffffffff doesn't exist
(assert_invalid_custom
  (module
    (@custom "metadata.code.branch_hint" "\01\ff\ff\ff\ff\0f\00")
  )
  "invalid function index 4294967295 in branch hint section")

;; Branch hints can't be specified for imported functions.
(assert_invalid_custom
  (module
    (import "" "" (func))
    (func)
    (@custom "metadata.code.branch_hint" (before code) "\01\00\00")
  )
  "invalid function index 0 in branch hint section")

;; A branch hint at function offset 3 points into the middle of the
;; `i32.const 1000` instruction (locals byte at 0, `i32.const` at 1).
(assert_invalid_custom
  (module
    (func
      i32.const 1000
      drop)
    (@custom "metadata.code.branch_hint" (before code) "\01\00\01\03\01\00")
  )
  "branch hint for bytes between instructions")

;; Only one branch hint section is allowed.
(assert_invalid_custom
  (module
    (func)
    (@custom "metadata.code.branch_hint" (before code) "\01\00\00")
    (@custom "metadata.code.branch_hint" (before code) "\00")
  )
  "duplicate branch hint section")

(module
  (memory 1)
  (data $a (i32.const 0) "x")
  (data $b "y")
)

;; Names data segment 1, but only data segment 0 exists.
(assert_invalid_custom
  (module
    (memory 1)
    (data (i32.const 0) "x")
    (@custom "name" "\09\04\01\01\01\61")
  )
  "invalid data naming index 1")

(module
  (func $f (param $x i32) (local $y i64)
    block $l1
      loop $l2
      end
    end)
)

;; exact function imports
(assert_invalid_custom
  (module
    (type (func))
    (import "" "" (func $f (exact (type 0))))
    ;; local names: func 0 -> local 5 = ""
    (@custom "name" "\02\05\01\00\01\05\00")
  )
  "invalid local name index 5")

(component
  (core module $a (func $f (param $x i32)))
  (core module $b (func $g (local $y i32)))
  (core instance (instantiate $b))
  (component $c
    (core module $d (func $h))
  )
)

(assert_invalid_custom
  (component
    (core module $a (func $f))
    (core module $b
      (func (local i32))
      ;; local names: func 0 -> local 5 = ""
      (@custom "name" "\02\05\01\00\01\05\00")
    )
    (core instance (instantiate $a))
  )
  "invalid local index 5")

(assert_invalid_custom
  (component
    ;; module name "c"
    (@custom "name" "\00\02\01c")
  )
  "core module `name` section in a component")
(assert_invalid_custom
  (component
    (@custom "metadata.code.branch_hint" "\01\00\00")
  )
  "core module branch hint section in a component")
