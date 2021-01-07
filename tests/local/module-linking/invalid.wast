(assert_invalid
  (module
    (type (module
      (import "" (func (type 1)))
    ))
    (type (func))
  )
  "type index out of bounds")

(assert_invalid
  (module
    (func $f)
    (module
      (alias parent $f (func $f))
      (func
        call $f)
    )
  )
  "invalid external kind in alias")
