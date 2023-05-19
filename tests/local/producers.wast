(module
  (@producers)
)

(module
  (@producers
    (language "foo" "bar")
  )
)

(module
  (@producers
    (language "foo" "bar")
    (language "foo" "bar")
    (sdk "foo" "bar")
    (processed-by "foo" "bar")
  )
)

(module
  (@producers)
  (@producers (sdk "foo" "bar"))
)

(assert_invalid
  (module quote "(@producers (foo))")
  "unexpected token")


;; invalid is ok
(module binary
  "\00asm" "\01\00\00\00"

  "\00"          ;; custom
  "\0b"          ;; length
  "\09producers" ;; custom section name
  "\01"          ;; 1 entry
)

;; weird name
(module binary
  "\00asm" "\01\00\00\00"

  "\00"          ;; custom
  "\14"          ;; length
  "\09producers" ;; custom section name
  "\01"          ;; 1 entry
  "\03a\na"      ;; name = "a\na"
  "\01"          ;; 1 field
  "\01a\01a"     ;; value = "a" version = "a"
)
