(assert_invalid
  (module (rec))
  "requires `gc` proposal to be enabled")

;; As a size optimization, we encode explicit rec groups of one type into the
;; equivalent implicit rec group of just that type's direct encoding.
;;
;; (assert_invalid
;;   (module (rec (type (func))))
;;   "requires `gc` proposal to be enabled")

(assert_invalid
  (module (rec (type (func)) (type (func))))
  "requires `gc` proposal to be enabled")
