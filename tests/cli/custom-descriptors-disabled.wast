;; RUN: wast --assert default --snapshot tests/snapshots %

(assert_invalid
  (module
    (type $t (struct))
    (func (param (ref (exact $t))))
  )
  "custom descriptors required for exact reference types")

(assert_invalid
  (module
    (rec
      (type (descriptor 1) (struct (field i32)))
      (type (func))
    )
  )
  "custom descriptors proposal must be enabled")

(assert_invalid
  (module
    (rec
      (type (func))
      (type (describes 0) (struct (field (ref 0))))
    )
  )
  "custom descriptors proposal must be enabled")

(assert_invalid
  (module
    (type (func))
    (import "" "f" (func (exact (type 0)))))
  "custom descriptors required for exact function imports")

(assert_invalid
  (module
    (type $s (struct))
    (func (param anyref) (result i32)
      (ref.test (ref (exact $s)) (local.get 0))))
  "custom descriptors required for exact reference types")

(assert_invalid
  (module
    (type $s (struct))
    (func (param anyref) (result anyref)
      (ref.cast (ref null (exact $s)) (local.get 0))))
  "custom descriptors required for exact reference types")

(assert_invalid
  (module
    (type $s (struct))
    (func (param anyref) (result anyref)
      (br_on_cast 0 anyref (ref (exact $s)) (local.get 0))))
  "custom descriptors required for exact reference types")

(assert_invalid
  (module
    (type $s (struct))
    (func (param anyref) (result anyref)
      (br_on_cast_fail 0 anyref (ref (exact $s)) (local.get 0))))
  "custom descriptors required for exact reference types")
