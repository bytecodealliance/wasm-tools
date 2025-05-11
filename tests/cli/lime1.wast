;; RUN: wast % -f lime1

;; import/export of mutable globals is allowed
(module
  (import "" "" (global (mut i32)))
  (global (export "") (mut i32) (i32.const 0))
)

;; saturating float-to-int instructions are allowed
(module
  (func (param f32) (result i32)
    local.get 0
    i32.trunc_sat_f32_s)
)

;; sign-extension is allowed
(module
  (func (param i32) (result i32)
    local.get 0
    i32.extend8_s)
)

;; multi-value is allowed
(module (type (func (result i32 i32))))

;; memory.copy and memory.fill are allowed ...
(module
  (memory 1)
  (func (param i32 i32 i32)
    local.get 0
    local.get 1
    local.get 2
    memory.copy
    local.get 0
    local.get 1
    local.get 2
    memory.fill))

;; ... but other bulk-memory features are not allowed
(assert_invalid
  (module (data))
  "passive data segments require the bulk-memory proposal")
(assert_invalid
  (module (elem func))
  "bulk memory must be enabled")
(assert_invalid
  (module (func data.drop 0))
  "bulk memory support is not enabled")
(assert_invalid
  (module (func memory.init 0))
  "bulk memory support is not enabled")
(assert_invalid
  (module (func table.init 0))
  "bulk memory support is not enabled")
(assert_invalid
  (module (func elem.drop 0))
  "bulk memory support is not enabled")
(assert_invalid
  (module (func table.copy))
  "bulk memory support is not enabled")

;; SIMD is not included
(assert_invalid
  (module (type (func (param v128))))
  "SIMD support is not enabled")

;; threads is not included
(assert_invalid
  (module (memory 1 1 shared))
  "threads must be enabled for shared memories")

;; tail call not enabled
(assert_invalid
  (module (func return_call 0))
  "tail calls support is not enabled")

;; floats are allowed
(module (func (param f32)))

;; multi memory not enabled
(assert_invalid
  (module (memory 1) (memory 1))
  "multiple memories")

;; exceptions not enabled
(assert_invalid
  (module (tag))
  "exceptions proposal not enabled")

;; memory64 not enabled
(assert_invalid
  (module (memory i64 1))
  "memory64 must be enabled for 64-bit memories")

;; extended const is allowed
(module
  (global i32 (i32.add (i32.const 0) (i32.const 1))))

;; GC is not allowed
(assert_invalid
  (module (type (sub (func))))
  "gc proposal must be enabled to use subtypes")

;; reference types is mostly not allowed...
(assert_invalid
  (module (type (func (param externref))))
  "reference types support is not enabled")
(assert_invalid
  (module (type (func (param funcref))))
  "reference types support is not enabled")
(assert_invalid
  (module (table 1 funcref) (table 1 funcref))
  "multiple tables")

;; ... but overlong encodings of `call_indirect` table immediates are allowed.
(module binary
  "\00asm" "\01\00\00\00" ;; magic header
  "\01\04"    ;; type section, 4 bytes
  "\01"       ;; 1 type
  "\60\00\00" ;; function type, no params, no results
  "\03\02"    ;; function section, 2 bytes
  "\01"       ;; 1 function
  "\00"       ;; function0 has type 0
  "\04\04"    ;; table section, 4 bytes
  "\01"       ;; 1 table
  "\70\00\01" ;; funcref table, no flags, min 1 element
  "\0a\0c"    ;; code section, 12 bytes
  "\01"       ;; 1 function
  "\0a"       ;; function is 10 bytes
  "\00"       ;; no locals
  "\41\00"    ;; i32.const 0

  ;; call_indirect opcode + (type 0) immediate
  "\11\00"

  ;; 4-byte encoding of the immediate 0, the table index for `call_indirect`
  "\80\80\80\00"

  "\0b"       ;; end
)
