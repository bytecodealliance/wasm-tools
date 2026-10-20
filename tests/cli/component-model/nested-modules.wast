;; RUN: wast --assert default --snapshot tests/snapshots %

(component definition
  (import "i1" (core module))

  (core module)
  (core module)

  (core module (export "x"))

  (component
    (core module)
  )

  (component
    (core module $m)
    (import "a" (func (param "p" string)))
    (export "b" (core module $m))
  )
)

;; does the `import` use the type annotation specified later?
(component definition
  (import "a" (core module))
  (core type (module))
)

;; be sure to typecheck nested modules
(assert_invalid
  (component
    (core module
      (func
        i32.add)
    )
  )
  "type mismatch")

;; interleave module definitions with imports/aliases and ensure that we
;; typecheck the module code section correctly
(component definition
  (core module
    (func (export ""))
  )
  (import "a" (core module))
  (core module
    (func (export "") (result i32) i32.const 5)
  )
  (import "b" (instance (export "a" (core module))))
  (alias export 0 "a" (core module))
)

;; module section claims 100 bytes, only an 8-byte module header follows
(assert_malformed
  (component binary
    "\00asm\0d\00\01\00"
    "\01\64"              ;; core module section, size 100
    "\00asm\01\00\00\00") ;; ... but only 8 bytes present
  "unexpected end")

;; same for a nested component section
(assert_malformed
  (component binary
    "\00asm\0d\00\01\00"
    "\04\64"              ;; component section, size 100
    "\00asm\0d\00\01\00") ;; ... but only 8 bytes present
  "unexpected end")

;; same, but with valid sections in the nested module
(assert_malformed
  (component binary
    "\00asm\0d\00\01\00"
    "\01\64"                    ;; core module section, size 100
    "\00asm\01\00\00\00"
    "\01\04\01\60\00\00")       ;; type section: (func)
  "unexpected end")
