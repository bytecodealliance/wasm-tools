;; RUN: wast --assert default --snapshot tests/snapshots % -f cm-async,cm64

;; i64 context.{get,set} slots are accepted when the component model 64-bit
;; feature is enabled.
(component
  (core func $get0 (canon context.get i64 0))
  (core func $set0 (canon context.set i64 0))

  (core module $m
    (import "" "get0" (func (result i64)))
    (import "" "set0" (func (param i64)))
  )
  (core instance (instantiate $m
    (with "" (instance
      (export "get0" (func $get0))
      (export "set0" (func $set0))
    ))
  ))
)

;; Mixed i32 and i64 slots round-trip independently.
(component
  (core func (canon context.get i32 0))
  (core func (canon context.set i32 0))
  (core func (canon context.get i64 0))
  (core func (canon context.set i64 0))
)

;; Different canon forms also accept i64.
(component
  (core func (canon context.get i64 0))
  (canon context.get i64 0 (core func))
  (core func (canon context.set i64 0))
  (canon context.set i64 0 (core func))
)

;; Signature must match the declared slot width.
(assert_invalid
  (component
    (core module $m (import "" "" (func (result i32))))
    (core func $f (canon context.get i64 0))
    (core instance (instantiate $m (with "" (instance (export "" (func $f))))))
  )
  "found:    (func (result i64))")

(assert_invalid
  (component
    (core module $m (import "" "" (func (param i32))))
    (core func $f (canon context.set i64 0))
    (core instance (instantiate $m (with "" (instance (export "" (func $f))))))
  )
  "found:    (func (param i64))")

;; Binary form: `0x7e` type byte is i64.
(component binary
  "\00asm" "\0d\00\01\00" ;; component header
  "\08\07"                ;; canonicals section, 7 bytes
  "\02"                   ;; 2 entries
  "\0a\7e\00"             ;; context.get i64 0
  "\0b\7e\00"             ;; context.set i64 0
)
