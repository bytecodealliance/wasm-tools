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

(assert_invalid
  (module
    (type $t (func))
    (func struct.new $t drop))
  "expected struct type at index 0, found (func ...)")

(assert_invalid
  (module
    (type $t (struct))
    (func struct.get $t 100))
  "unknown field: field index out of bounds")

(assert_invalid
  (module
    (func struct.get 200 100))
  "unknown type: type index out of bounds")

(assert_invalid
  (module
    (type $t (struct))
    (func array.new $t drop))
  "expected array type at index 0, found (struct ...)")

(assert_invalid
  (module
    (func array.new 100))
  "unknown type: type index out of bounds")

(assert_invalid
  (module
    (type $t (struct))
    (func (type $t)))
  "type index 0 is not a function type")

(assert_invalid
  (module
    (type $t (struct))
    (func block (type $t) end))
  "expected func type at index 0, found (struct ...)")

(assert_invalid
  (module
    (func
      block $l
        unreachable
        br_on_non_null $l
      end
    )
  )
  "type mismatch: br_on_non_null target has no label types")

(assert_invalid
  (module
    (func
      block $l (result i32)
        unreachable
        br_on_non_null $l
      end
    )
  )
  "type mismatch: br_on_non_null target does not end with heap type")

(assert_invalid
  (module
    (type $t (struct (field (ref func))))
    (func
      struct.new_default $t
    )
  )
  "invalid `struct.new_default`: (ref func) field is not defaultable")

(assert_invalid
  (module
    (type $t (struct (field $f i32)))
    (func
      unreachable
      struct.get_u $t $f
    )
  )
  "cannot use struct.get_u with non-packed storage types")

(assert_invalid
  (module
    (type $t (array (ref func)))
    (func
      array.new_default $t
    )
  )
  "invalid `array.new_default`: (ref func) field is not defaultable")

(assert_invalid
  (module
    (type $t (array (ref func)))
    (func
      i32.const 0
      i32.const 0
      array.new_data $t $d
      drop
    )
    (data $d "xxx")
  )
  "array.new_data can only create arrays with numeric and vector elements")

(assert_invalid
  (module binary
    "\00asm" "\01\00\00\00"     ;; module header

    "\01\07"                    ;; type section, 7 bytes
    "\02"                       ;; 2 types
    "\5e\7f\00"                 ;; (type (array i32))
    "\60\00\00"                 ;; (type (func))

    "\03\02"                    ;; func section, 2 bytes
    "\01"                       ;; 1 func
    "\01"                       ;; type 1

    "\0a\0d"                    ;; code section, 13 bytes
    "\01"                       ;; 1 count
    "\0b"                       ;; 11-byte function
    "\00"                       ;; no locals
    "\41\00"                    ;; i32.const 0
    "\41\00"                    ;; i32.const 0
    "\fb\09\00\00"              ;; array.new_data 0 0
    "\1a"                       ;; drop
    "\0b"                       ;; end

    "\0b\06"                    ;; data section, 6 bytes
    "\01"                       ;; 1 count
    "\01"                       ;; passive
    "\03xxx"                    ;; 3 bytes of data "xxx"
  )
  "data count section required")

;; slightly modified version of the above with a data count section
(module binary
  "\00asm" "\01\00\00\00"     ;; module header

  "\01\07"                    ;; type section, 7 bytes
  "\02"                       ;; 2 types
  "\5e\7f\00"                 ;; (type (array i32))
  "\60\00\00"                 ;; (type (func))

  "\03\02"                    ;; func section, 2 bytes
  "\01"                       ;; 1 func
  "\01"                       ;; type 1

  "\0c\01"                    ;; data count section, 1 byte
  "\01"                       ;; 1 data

  "\0a\0d"                    ;; code section, 13 bytes
  "\01"                       ;; 1 count
  "\0b"                       ;; 11-byte function
  "\00"                       ;; no locals
  "\41\00"                    ;; i32.const 0
  "\41\00"                    ;; i32.const 0
  "\fb\09\00\00"              ;; array.new_data 0 0
  "\1a"                       ;; drop
  "\0b"                       ;; end

  "\0b\06"                    ;; data section, 6 bytes
  "\01"                       ;; 1 count
  "\01"                       ;; passive
  "\03xxx"                    ;; 3 bytes of data "xxx"
)

(assert_invalid
  (module
    (type $t (array i8))
    (func
      i32.const 0
      i32.const 0
      array.new_data $t 100
      drop
    )
  )
  "unknown data segment 100")

(assert_invalid
  (module
    (type $t (array i8))
    (func
      i32.const 0
      i32.const 0
      array.new_elem $t $e
      drop
    )
    (elem $e funcref)
  )
  "type mismatch: array.new_elem can only create arrays with reference elements")

(assert_invalid
  (module
    (type $t (array (ref any)))
    (func
      i32.const 0
      i32.const 0
      array.new_elem $t $e
      drop
    )
    (elem $e funcref)
  )
  "invalid array.new_elem instruction: element segment 0 type mismatch: expected (ref any), found funcref")

(assert_invalid
  (module
    (type $t1 (array (mut i8)))
    (type $t2 (array (mut i16)))
    (func
      unreachable
      array.copy $t1 $t2
    )
  )
  "array types do not match: expected i8, found i16")

(assert_invalid
  (module
    (type $t1 (array (mut i16)))
    (type $t2 (array (mut i8)))
    (func
      unreachable
      array.copy $t1 $t2
    )
  )
  "array types do not match: expected i16, found i8")

(assert_invalid
  (module
    (type $t1 (array (mut i32)))
    (type $t2 (array (mut i64)))
    (func
      unreachable
      array.copy $t1 $t2
    )
  )
  "array types do not match: expected i32, found i64")

(assert_invalid
  (module
    (type $t1 (array (mut i32)))
    (type $t2 (array (mut i8)))
    (func
      unreachable
      array.copy $t1 $t2
    )
  )
  "array types do not match: expected i32, found i8")

(assert_invalid
  (module
    (type $t1 (array (mut i8)))
    (type $t2 (array (mut i32)))
    (func
      unreachable
      array.copy $t1 $t2
    )
  )
  "array types do not match: expected i8, found i32")

(module
  (type $t1 (array (mut i16)))
  (type $t2 (array (mut i16)))
  (func
    unreachable
    array.copy $t1 $t2
  )
)

(assert_invalid
  (module
    (type $t1 (array (mut i8)))
    (type $t2 (array (mut i32)))
    (func
      block
        unreachable
        br_on_cast_fail 0 anyref anyref
      end
    )
  )
  "type mismatch: expected a reference type, found nothing")

(assert_invalid
  (module
    (table 1 externref)
    (func
      i32.const 0
      call_indirect
      ))
  "indirect calls must go through a table with type <= funcref")
