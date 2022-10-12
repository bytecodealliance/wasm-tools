(component
  (type (func))
  (type (func (param "x" s8)))
  (type (func (param "x" u8)))
  (type (func (param "x" s16)))
  (type (func (param "x" u16)))
  (type (func (param "x" s32)))
  (type (func (param "x" u32)))
  (type (func (param "x" s64)))
  (type (func (param "x" u64)))
  (type (func (param "x" float32)))
  (type (func (param "x" float64)))
  (type (func (param "x" bool)))
  (type (func (param "x" string)))
  (type (record (field "a" s8) (field "b" u8) (field "c" s16) (field "d" u16) (field "e" s32) (field "f" u32) (field "g" s64) (field "h" u64) (field "i" float32) (field "j" float64) (field "k" bool) (field "l" string)))
  (type (func (param "x" 13)))
  (type (list 13))
  (type (func (param "x" 15)))
  (type (tuple 13 string))
  (type (func (param "x" 17)))
  (type (flags "a" "b" "c"))
  (type (func (param "x" 19)))
  (type (enum "a" "b" "c"))
  (type (func (param "x" 21)))
  (type (union s8 string 13))
  (type (func (param "x" 23)))
  (type (variant (case "a" s8) (case "b" u8) (case "c" s16) (case "d" u16) (case "e" s32) (case "f" u32) (case "g" s64) (case "h" u64) (case "i" float32) (case "j" float64) (case "k" bool) (case "l" string) (case "m" 13)))
  (type (option 25))
  (type (func (param "x" 26)))
  (type (result 13 (error string)))
  (type (func (result 28)))
  (type
    (instance
      (alias outer 1 0 (type))
      (export "a" (func (type 0)))
      (alias outer 1 1 (type))
      (export "b" (func (type 1)))
      (alias outer 1 2 (type))
      (export "c" (func (type 2)))
      (alias outer 1 3 (type))
      (export "d" (func (type 3)))
      (alias outer 1 4 (type))
      (export "e" (func (type 4)))
      (alias outer 1 5 (type))
      (export "f" (func (type 5)))
      (alias outer 1 6 (type))
      (export "g" (func (type 6)))
      (alias outer 1 7 (type))
      (export "h" (func (type 7)))
      (alias outer 1 8 (type))
      (export "i" (func (type 8)))
      (alias outer 1 9 (type))
      (export "j" (func (type 9)))
      (alias outer 1 10 (type))
      (export "k" (func (type 10)))
      (alias outer 1 11 (type))
      (export "l" (func (type 11)))
      (alias outer 1 12 (type))
      (export "m" (func (type 12)))
      (alias outer 1 13 (type))
      (export "record1"  (type (eq 13)))
      (alias outer 1 14 (type))
      (export "n" (func (type 14)))
      (alias outer 1 16 (type))
      (export "o" (func (type 15)))
      (alias outer 1 18 (type))
      (export "p" (func (type 16)))
      (alias outer 1 19 (type))
      (export "flags1"  (type (eq 17)))
      (alias outer 1 20 (type))
      (export "q" (func (type 18)))
      (alias outer 1 21 (type))
      (export "enum1"  (type (eq 19)))
      (alias outer 1 22 (type))
      (export "r" (func (type 20)))
      (alias outer 1 23 (type))
      (export "union1"  (type (eq 21)))
      (alias outer 1 24 (type))
      (export "s" (func (type 22)))
      (alias outer 1 25 (type))
      (export "variant1"  (type (eq 23)))
      (alias outer 1 27 (type))
      (export "t" (func (type 24)))
      (alias outer 1 29 (type))
      (export "u" (func (type 25)))
    )
  )
  (type (func (param "x" string) (result string)))
  (import "a" (instance (type 30)))
  (core module
    (import "a" "m" (func (param i32 i32)))
    (func $m (param i32 i32) (result i32) unreachable)
    (func $canonical_abi_realloc (param i32 i32 i32 i32) (result i32) unreachable)
    (memory 0)
    (export "memory" (memory 0))
    (export "m" (func $m))
    (export "canonical_abi_realloc" (func $canonical_abi_realloc))
  )
  (core module
    (type (func (param i32 i32)))
    (type (func (param i32 i32 i32 i32 i32 i32 i64 i64 f32 f64 i32 i32 i32)))
    (type (func (param i32 i32 i32 i32 i32 i32 i64 i64 f32 f64 i32 i32 i32 i32 i32)))
    (type (func (param i32 i32 i32 i32 i32 i32 i32 i64 i64 f32 f64 i32 i32 i32)))
    (type (func (param i32 i32 i64 i32 i32 i32 i32 i32 i64 i64 f32 f64 i32 i32 i32)))
    (type (func (param i32)))
    (func (type 0) (param i32 i32)
      local.get 0
      local.get 1
      i32.const 0
      call_indirect (type 0)
    )
    (func (type 1) (param i32 i32 i32 i32 i32 i32 i64 i64 f32 f64 i32 i32 i32)
      local.get 0
      local.get 1
      local.get 2
      local.get 3
      local.get 4
      local.get 5
      local.get 6
      local.get 7
      local.get 8
      local.get 9
      local.get 10
      local.get 11
      local.get 12
      i32.const 1
      call_indirect (type 1)
    )
    (func (type 0) (param i32 i32)
      local.get 0
      local.get 1
      i32.const 2
      call_indirect (type 0)
    )
    (func (type 2) (param i32 i32 i32 i32 i32 i32 i64 i64 f32 f64 i32 i32 i32 i32 i32)
      local.get 0
      local.get 1
      local.get 2
      local.get 3
      local.get 4
      local.get 5
      local.get 6
      local.get 7
      local.get 8
      local.get 9
      local.get 10
      local.get 11
      local.get 12
      local.get 13
      local.get 14
      i32.const 3
      call_indirect (type 2)
    )
    (func (type 3) (param i32 i32 i32 i32 i32 i32 i32 i64 i64 f32 f64 i32 i32 i32)
      local.get 0
      local.get 1
      local.get 2
      local.get 3
      local.get 4
      local.get 5
      local.get 6
      local.get 7
      local.get 8
      local.get 9
      local.get 10
      local.get 11
      local.get 12
      local.get 13
      i32.const 4
      call_indirect (type 3)
    )
    (func (type 4) (param i32 i32 i64 i32 i32 i32 i32 i32 i64 i64 f32 f64 i32 i32 i32)
      local.get 0
      local.get 1
      local.get 2
      local.get 3
      local.get 4
      local.get 5
      local.get 6
      local.get 7
      local.get 8
      local.get 9
      local.get 10
      local.get 11
      local.get 12
      local.get 13
      local.get 14
      i32.const 5
      call_indirect (type 4)
    )
    (func (type 5) (param i32)
      local.get 0
      i32.const 6
      call_indirect (type 5)
    )
    (table 7 7 funcref)
    (export "0" (func 0))
    (export "1" (func 1))
    (export "2" (func 2))
    (export "3" (func 3))
    (export "4" (func 4))
    (export "5" (func 5))
    (export "6" (func 6))
    (export "$imports" (table 0))
  )
  (core module
    (type (func (param i32 i32)))
    (type (func (param i32 i32 i32 i32 i32 i32 i64 i64 f32 f64 i32 i32 i32)))
    (type (func (param i32 i32 i32 i32 i32 i32 i64 i64 f32 f64 i32 i32 i32 i32 i32)))
    (type (func (param i32 i32 i32 i32 i32 i32 i32 i64 i64 f32 f64 i32 i32 i32)))
    (type (func (param i32 i32 i64 i32 i32 i32 i32 i32 i64 i64 f32 f64 i32 i32 i32)))
    (type (func (param i32)))
    (import "" "0" (func (type 0)))
    (import "" "1" (func (type 1)))
    (import "" "2" (func (type 0)))
    (import "" "3" (func (type 2)))
    (import "" "4" (func (type 3)))
    (import "" "5" (func (type 4)))
    (import "" "6" (func (type 5)))
    (import "" "$imports" (table 7 7 funcref))
    (elem (i32.const 0) func 0 1 2 3 4 5 6)
  )
  (core instance (instantiate 1))
  (alias core export 0 "0" (core func))
  (alias core export 0 "1" (core func))
  (alias core export 0 "2" (core func))
  (alias core export 0 "3" (core func))
  (alias core export 0 "4" (core func))
  (alias core export 0 "5" (core func))
  (alias core export 0 "6" (core func))
  (alias export 0 "a" (func))
  (alias export 0 "b" (func))
  (alias export 0 "c" (func))
  (alias export 0 "d" (func))
  (alias export 0 "e" (func))
  (alias export 0 "f" (func))
  (alias export 0 "g" (func))
  (alias export 0 "h" (func))
  (alias export 0 "i" (func))
  (alias export 0 "j" (func))
  (alias export 0 "k" (func))
  (alias export 0 "l" (func))
  (alias export 0 "q" (func))
  (alias export 0 "r" (func))
  (core func (canon lower (func 0)))
  (core func (canon lower (func 1)))
  (core func (canon lower (func 2)))
  (core func (canon lower (func 3)))
  (core func (canon lower (func 4)))
  (core func (canon lower (func 5)))
  (core func (canon lower (func 6)))
  (core func (canon lower (func 7)))
  (core func (canon lower (func 8)))
  (core func (canon lower (func 9)))
  (core func (canon lower (func 10)))
  (core func (canon lower (func 11)))
  (core func (canon lower (func 12)))
  (core func (canon lower (func 13)))
  (core instance
    (export "m" (func 0))
    (export "n" (func 1))
    (export "o" (func 2))
    (export "p" (func 3))
    (export "s" (func 4))
    (export "t" (func 5))
    (export "u" (func 6))
    (export "a" (func 7))
    (export "b" (func 8))
    (export "c" (func 9))
    (export "d" (func 10))
    (export "e" (func 11))
    (export "f" (func 12))
    (export "g" (func 13))
    (export "h" (func 14))
    (export "i" (func 15))
    (export "j" (func 16))
    (export "k" (func 17))
    (export "l" (func 18))
    (export "q" (func 19))
    (export "r" (func 20))
  )
  (core instance (instantiate 0
      (with "a" (instance 1))
    )
  )
  (alias core export 2 "memory" (core memory))
  (alias core export 2 "canonical_abi_realloc" (core func))
  (alias core export 0 "$imports" (core table))
  (alias export 0 "m" (func))
  (alias export 0 "n" (func))
  (alias export 0 "o" (func))
  (alias export 0 "p" (func))
  (alias export 0 "s" (func))
  (alias export 0 "t" (func))
  (alias export 0 "u" (func))
  (core func (canon lower (func 14) (memory 0) (realloc 21) string-encoding=utf8))
  (core func (canon lower (func 15) (memory 0) (realloc 21) string-encoding=utf8))
  (core func (canon lower (func 16) (memory 0) (realloc 21) string-encoding=utf8))
  (core func (canon lower (func 17) (memory 0) (realloc 21) string-encoding=utf8))
  (core func (canon lower (func 18) (memory 0) (realloc 21) string-encoding=utf8))
  (core func (canon lower (func 19) (memory 0) (realloc 21) string-encoding=utf8))
  (core func (canon lower (func 20) (memory 0) (realloc 21) string-encoding=utf8))
  (core instance
    (export "$imports" (table 0))
    (export "0" (func 22))
    (export "1" (func 23))
    (export "2" (func 24))
    (export "3" (func 25))
    (export "4" (func 26))
    (export "5" (func 27))
    (export "6" (func 28))
  )
  (core instance (instantiate 2
      (with "" (instance 3))
    )
  )
  (alias core export 2 "m" (core func))
  (func (type 31) (canon lift (core func 29) (memory 0) (realloc 21) string-encoding=utf8))
  (instance (export "m" (func 21)))
  (export "x" (instance 1))
)
