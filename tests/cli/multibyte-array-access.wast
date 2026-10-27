;; RUN: wast --assert default --snapshot tests/snapshots % -f multibyte-array-access

;; Basic loads and stores on arrays of every allowed element type, in both
;; folded and unfolded forms, with and without offset/align immediates.
(module
  (type $i8 (array (mut i8)))
  (type $i16 (array (mut i16)))
  (type $i32 (array (mut i32)))
  (type $i64 (array (mut i64)))
  (type $f32 (array (mut f32)))
  (type $f64 (array (mut f64)))
  (type $v128 (array (mut v128)))
  (type $imm (array i8))

  (func (param (ref null $i8) i32) (result i32)
    (i32.load (type $i8) (local.get 0) (local.get 1)))
  (func (param (ref null $i8) i32) (result i32)
    local.get 0
    local.get 1
    i32.load (type $i8) offset=4 align=1)
  (func (param (ref null $i16) i32) (result i64)
    (i64.load (type $i16) offset=8 (local.get 0) (local.get 1)))
  (func (param (ref null $i32) i32) (result f32)
    (f32.load (type $i32) align=2 (local.get 0) (local.get 1)))
  (func (param (ref null $i64) i32) (result f64)
    (f64.load (type $i64) (local.get 0) (local.get 1)))
  (func (param (ref null $f32) i32) (result i32)
    (i32.load8_s (type $f32) (local.get 0) (local.get 1)))
  (func (param (ref null $f64) i32) (result i32)
    (i32.load8_u (type $f64) (local.get 0) (local.get 1)))
  (func (param (ref null $v128) i32) (result i32)
    (i32.load16_s (type $v128) (local.get 0) (local.get 1)))
  (func (param (ref $i8) i32) (result i32)
    (i32.load16_u (type $i8) (local.get 0) (local.get 1)))
  (func (param (ref $i8) i32) (result i64)
    (i64.load8_s (type $i8) (local.get 0) (local.get 1)))
  (func (param (ref $i8) i32) (result i64)
    (i64.load8_u (type $i8) (local.get 0) (local.get 1)))
  (func (param (ref $i8) i32) (result i64)
    (i64.load16_s (type $i8) (local.get 0) (local.get 1)))
  (func (param (ref $i8) i32) (result i64)
    (i64.load16_u (type $i8) (local.get 0) (local.get 1)))
  (func (param (ref $i8) i32) (result i64)
    (i64.load32_s (type $i8) (local.get 0) (local.get 1)))
  (func (param (ref $i8) i32) (result i64)
    (i64.load32_u (type $i8) (local.get 0) (local.get 1)))

  ;; loads are allowed on immutable arrays
  (func (param (ref null $imm) i32) (result i32)
    (i32.load8_u (type $imm) (local.get 0) (local.get 1)))

  (func (param (ref null $i8) i32 i32)
    (i32.store (type $i8) (local.get 0) (local.get 1) (local.get 2)))
  (func (param (ref null $i8) i32 i32)
    local.get 0
    local.get 1
    local.get 2
    i32.store (type $i8) offset=4 align=1)
  (func (param (ref null $i16) i32 i64)
    (i64.store (type $i16) (local.get 0) (local.get 1) (local.get 2)))
  (func (param (ref null $i32) i32 f32)
    (f32.store (type $i32) (local.get 0) (local.get 1) (local.get 2)))
  (func (param (ref null $i64) i32 f64)
    (f64.store (type $i64) (local.get 0) (local.get 1) (local.get 2)))
  (func (param (ref null $f32) i32 i32)
    (i32.store8 (type $f32) (local.get 0) (local.get 1) (local.get 2)))
  (func (param (ref null $f64) i32 i32)
    (i32.store16 (type $f64) (local.get 0) (local.get 1) (local.get 2)))
  (func (param (ref null $v128) i32 i64)
    (i64.store8 (type $v128) (local.get 0) (local.get 1) (local.get 2)))
  (func (param (ref null $i8) i32 i64)
    (i64.store16 (type $i8) (local.get 0) (local.get 1) (local.get 2)))
  (func (param (ref null $i8) i32 i64)
    (i64.store32 (type $i8) (local.get 0) (local.get 1) (local.get 2)))

  ;; SIMD loads and stores
  (func (param (ref null $i8) i32) (result v128)
    (v128.load (type $i8) (local.get 0) (local.get 1)))
  (func (param (ref null $i8) i32 v128)
    (v128.store (type $i8) offset=16 (local.get 0) (local.get 1) (local.get 2)))
  (func (param (ref null $i8) i32) (result v128)
    (v128.load8x8_s (type $i8) (local.get 0) (local.get 1)))
  (func (param (ref null $i8) i32) (result v128)
    (v128.load8x8_u (type $i8) (local.get 0) (local.get 1)))
  (func (param (ref null $i8) i32) (result v128)
    (v128.load16x4_s (type $i8) (local.get 0) (local.get 1)))
  (func (param (ref null $i8) i32) (result v128)
    (v128.load16x4_u (type $i8) (local.get 0) (local.get 1)))
  (func (param (ref null $i8) i32) (result v128)
    (v128.load32x2_s (type $i8) (local.get 0) (local.get 1)))
  (func (param (ref null $i8) i32) (result v128)
    (v128.load32x2_u (type $i8) (local.get 0) (local.get 1)))
  (func (param (ref null $i8) i32) (result v128)
    (v128.load8_splat (type $i8) (local.get 0) (local.get 1)))
  (func (param (ref null $i8) i32) (result v128)
    (v128.load16_splat (type $i8) (local.get 0) (local.get 1)))
  (func (param (ref null $i8) i32) (result v128)
    (v128.load32_splat (type $i8) (local.get 0) (local.get 1)))
  (func (param (ref null $i8) i32) (result v128)
    (v128.load64_splat (type $i8) (local.get 0) (local.get 1)))
  (func (param (ref null $i8) i32) (result v128)
    (v128.load32_zero (type $i8) (local.get 0) (local.get 1)))
  (func (param (ref null $i8) i32) (result v128)
    (v128.load64_zero (type $i8) (local.get 0) (local.get 1)))

  ;; lane loads and stores take a trailing lane index
  (func (param (ref null $i8) i32 v128) (result v128)
    (v128.load8_lane (type $i8) 0 (local.get 0) (local.get 1) (local.get 2)))
  (func (param (ref null $i8) i32 v128) (result v128)
    (v128.load16_lane (type $i8) offset=2 align=1 7 (local.get 0) (local.get 1) (local.get 2)))
  (func (param (ref null $i8) i32 v128) (result v128)
    (v128.load32_lane (type $i8) 3 (local.get 0) (local.get 1) (local.get 2)))
  (func (param (ref null $i8) i32 v128) (result v128)
    (v128.load64_lane (type $i8) 1 (local.get 0) (local.get 1) (local.get 2)))
  (func (param (ref null $i8) i32 v128)
    (v128.store8_lane (type $i8) 15 (local.get 0) (local.get 1) (local.get 2)))
  (func (param (ref null $i8) i32 v128)
    (v128.store16_lane (type $i8) 0 (local.get 0) (local.get 1) (local.get 2)))
  (func (param (ref null $i8) i32 v128)
    local.get 0
    local.get 1
    local.get 2
    v128.store32_lane (type $i8) offset=4 0)
  (func (param (ref null $i8) i32 v128)
    (v128.store64_lane (type $i8) 1 (local.get 0) (local.get 1) (local.get 2)))
)

;; Binary encoding: bit 5 of the flags is set and the type index follows the
;; flags, before the offset.
(module binary
  "\00asm\01\00\00\00"
  "\01\0b\02"                             ;; type section
    "\5e\78\01"                           ;; (array (mut i8))
    "\60\02\63\00\7f\01\7f"               ;; (func (param (ref null 0) i32) (result i32))
  "\03\02\01\01"                          ;; function section
  "\0a\0c\01"                             ;; code section
    "\0a\00"
    "\20\00"                              ;; local.get 0
    "\20\01"                              ;; local.get 1
    "\28\21\00\04"                        ;; i32.load flags=0x21 type=0 offset=4
    "\0b"                                 ;; end
)

;; Bit 6 (a memory index) may not be combined with bit 5.
(assert_malformed
  (module binary
    "\00asm\01\00\00\00"
    "\01\0b\02"                           ;; type section
      "\5e\78\01"                         ;; (array (mut i8))
      "\60\02\63\00\7f\01\7f"             ;; (func (param (ref null 0) i32) (result i32))
    "\03\02\01\01"                        ;; function section
    "\05\03\01\00\01"                     ;; memory section
    "\0a\0d\01"                           ;; code section
      "\0b\00"
      "\20\00"                            ;; local.get 0
      "\20\01"                            ;; local.get 1
      "\28\60\00\00\00"                   ;; i32.load flags=0x60 memory=0 type=0 offset=0
      "\0b"                               ;; end
  )
  "cannot specify both memory and array index")

;; Stores require a mutable array.
(assert_invalid
  (module
    (type $a (array i8))
    (func (param (ref null $a) i32 i32)
      (i32.store (type $a) (local.get 0) (local.get 1) (local.get 2))))
  "array is immutable")
(assert_invalid
  (module
    (type $a (array i8))
    (func (param (ref null $a) i32 v128)
      (v128.store8_lane (type $a) 0 (local.get 0) (local.get 1) (local.get 2))))
  "array is immutable")

;; The element type must be numeric, vector, or packed.
(assert_invalid
  (module
    (type $a (array (mut anyref)))
    (func (param (ref null $a) i32) (result i32)
      (i32.load (type $a) (local.get 0) (local.get 1))))
  "require a numeric, vector, or packed element type")
(assert_invalid
  (module
    (type $a (array (mut (ref null func))))
    (func (param (ref null $a) i32 i32)
      (i32.store (type $a) (local.get 0) (local.get 1) (local.get 2))))
  "require a numeric, vector, or packed element type")

;; The type must be an array type.
(assert_invalid
  (module
    (type $s (struct (field (mut i32))))
    (func (param (ref null $s) i32) (result i32)
      (i32.load (type $s) (local.get 0) (local.get 1))))
  "expected array type")
(assert_invalid
  (module
    (type $f (func))
    (func (param (ref null $f) i32) (result i32)
      (i32.load (type $f) (local.get 0) (local.get 1))))
  "expected array type")
(assert_invalid
  (module
    (func (param i32) (result i32)
      (i32.load (type 100) (ref.null none) (local.get 0))))
  "unknown type")

;; Operand types: the address is always an `i32` and the array reference sits
;; beneath it.
(assert_invalid
  (module
    (type $a (array (mut i8)))
    (func (param (ref null $a) i64) (result i32)
      (i32.load (type $a) (local.get 0) (local.get 1))))
  "type mismatch")
(assert_invalid
  (module
    (type $a (array (mut i8)))
    (type $b (array (mut i16)))
    (func (param (ref null $b) i32) (result i32)
      (i32.load (type $a) (local.get 0) (local.get 1))))
  "type mismatch")
(assert_invalid
  (module
    (type $a (array (mut i8)))
    (func (param i32) (result i32)
      (i32.load (type $a) (local.get 0))))
  "type mismatch")
(assert_invalid
  (module
    (type $a (array (mut i8)))
    (func (param (ref null $a) i32 i64)
      (i32.store (type $a) (local.get 0) (local.get 1) (local.get 2))))
  "type mismatch")

;; Alignment is still bounded by the natural alignment of the access.
(assert_invalid
  (module
    (type $a (array (mut i8)))
    (func (param (ref null $a) i32) (result i32)
      (i32.load (type $a) align=8 (local.get 0) (local.get 1))))
  "alignment must not be larger than natural")

;; Atomic instructions not allowed
(assert_invalid
  (module
    (type $a (array (mut i32)))
    (func (param (ref null $a) i32) (result i32)
      (i32.atomic.load (type $a) (local.get 0) (local.get 1))))
  "atomic instructions cannot operate on arrays")
