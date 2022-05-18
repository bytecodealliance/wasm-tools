(component
  (import "a" (func))
  (import "b" (instance))

  (import "c" (instance
    (export "" (func))
  ))
  (import "d" (component
    (import "" (module))
    (export "" (func))
  ))
)

;; TODO: these tests will all eventually fail once the binary format is updated
;; again. Currently the text parser ignores the "kind" on the import field but
;; it will one day be encode as part of the binary format.
(component
  (type $f (func))
  (import "" (instance (type $f)))
)
(component
  (type $f (func))
  (import "" (module (type $f)))
)
(component
  (type $f (module))
  (import "" (func (type $f)))
)

;; Disallow duplicate imports for core wasm modules
(assert_invalid
  (component
    (type (module
      (import "" "" (func))
      (import "" "" (func))
    ))
  )
  "duplicate import name `:`")
(assert_invalid
  (component
    (module
      (import "" "" (func))
      (import "" "" (func))
    )
  )
  "duplicate import name `:`")
(assert_invalid
  (component
    (type (module
      (import "" "a" (func))
      (import "" "a" (func))
    ))
  )
  "duplicate import name `:a`")
(assert_invalid
  (component
    (module
      (import "" "a" (func))
      (import "" "a" (func))
    )
  )
  "duplicate import name `:a`")

(assert_malformed
  (component quote
    "(import \"\" (func))"
    "(import \"\" (func))"
  )
  "duplicate import name `` already defined")

(assert_malformed
  (component quote
    "(type (component"
      "(import \"\" (func))"
      "(import \"\" (func))"
    "))"
  )
  "duplicate import name `` already defined")

(assert_invalid
  (component
    (import "" (func (type 100)))
  )
  "type index out of bounds")

(assert_invalid
  (component
    (module $m (func (export "")))
    (instance $i (instantiate (module $m)))
    (func (canon.lift (type 100) (func $i "")))
  )
  "type index out of bounds")

(component
  (import "" (value string))
)
