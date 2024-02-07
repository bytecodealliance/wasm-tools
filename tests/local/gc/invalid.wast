;; https://github.com/WebAssembly/gc/issues/516

(assert_invalid
  (module
    (func (param anyref) (result anyref)
      ref.null struct
      local.get 0
      ;; The label pushes an anyref, even though we really have a structref.
      br_on_null 0
      drop
      br_on_cast_fail 0  structref structref
    )
  )
  "type mismatch: expected structref, found anyref"
)

(assert_invalid
  (module
    (func (param anyref) (result anyref)
      ref.null struct
      ;; Adding an `unreachable` shouldn't change the fact that the `br_on_null`
      ;; pushes an `anyref` and the `br_on_cast_fail` expects a `structref`.
      unreachable
      local.get 0
      br_on_null 0
      drop
      br_on_cast_fail 0  structref structref
    )
  )
 "type mismatch: expected structref, found anyref"
 )
