(assert_invalid
  (module (rec))
  "requires `gc` proposal to be enabled")

;; As a size optimization, we encode explicit rec groups of one type into the
;; equivalent implicit rec group of just that type's direct encoding. Therefore,
;; we need to use `(module binary ...)` to actually get the explicit rec group
;; encoding to test here.
(assert_invalid
  (module binary
    "\00asm" "\01\00\00\00" ;; magic header and version number
    "\01\06"                ;; type section
    "\01"                   ;; 1 count
    "\4e\01"                ;; explicit rec group with 1 type
    "\60\00\00"             ;; func w/ empty params and empty returns
  )
  "requires `gc` proposal to be enabled")

(assert_invalid
  (module (rec (type (func)) (type (func))))
  "requires `gc` proposal to be enabled")
