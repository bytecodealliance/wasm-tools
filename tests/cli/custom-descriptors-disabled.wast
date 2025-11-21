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
