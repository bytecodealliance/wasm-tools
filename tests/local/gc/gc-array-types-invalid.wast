;; --enable-gc

(assert_invalid
  (module
    (type $a (array (mut (ref null 1000))))
  )
  "unknown type"
)
