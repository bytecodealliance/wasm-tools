;; RUN: wast --assert default --snapshot tests/snapshots % -f shared-everything-threads,custom-descriptors

;; A described type and its descriptor must agree on sharedness.
(module
  (rec
    (type $a (shared (descriptor $b) (struct)))
    (type $b (shared (describes $a) (struct)))
  )
)
(assert_invalid
  (module
    (rec
      (type $a (shared (descriptor $b) (struct)))
      (type $b (describes $a) (struct))
    )
  )
  "descriptor and described type must have the same sharedness")
(assert_invalid
  (module
    (rec
      (type $a (descriptor $b) (struct))
      (type $b (shared (describes $a) (struct)))
    )
  )
  "descriptor and described type must have the same sharedness")
