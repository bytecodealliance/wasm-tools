(assert_invalid
  (module (rec))
  "requires `gc` proposal to be enabled")

(assert_invalid
  (module (rec (type (func))))
  "requires `gc` proposal to be enabled")

(assert_invalid
  (module (rec (type (func)) (type (func))))
  "requires `gc` proposal to be enabled")
