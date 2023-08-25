(component
  (type $i1
    (instance
      (type $f1 (func))
      (type $f2 (func (param "x" s8)))
      (type $f3 (func (param "x" u8)))
      (type $f4 (func (param "x" s16)))
      (type $f5 (func (param "x" u16)))
      (type $f6 (func (param "x" s32)))
      (type $f7 (func (param "x" u32)))
      (type $f8 (func (param "x" s64)))
      (type $f9 (func (param "x" u64)))
      (type $f10 (func (param "x" float32)))
      (type $f11 (func (param "x" float64)))
      (type $f12 (func (param "x" bool)))
      (type $f13 (func (param "x" string)))
      (export "a" (func (type $f1)))
      (export "b" (func (type $f2)))
      (export "c" (func (type $f3)))
      (export "d" (func (type $f4)))
      (export "e" (func (type $f5)))
      (export "f" (func (type $f6)))
      (export "g" (func (type $f7)))
      (export "h" (func (type $f8)))
      (export "i" (func (type $f9)))
      (export "j" (func (type $f10)))
      (export "k" (func (type $f11)))
      (export "l" (func (type $f12)))
      (export "m" (func (type $f13)))
      (type $t1' (record (field "a" s8) (field "b" u8) (field "c" s16) (field "d" u16) (field "e" s32) (field "f" u32) (field "g" s64) (field "h" u64) (field "i" float32) (field "j" float64) (field "k" bool) (field "l" string)))
      (export $t1 "record1"  (type (eq $t1')))
      (type $f14 (func (param "x" $t1)))
      (export "n" (func (type $f14)))
      (type $t2 (list $t1))
      (type $f15 (func (param "x" $t2)))
      (export "o" (func (type $f15)))
      (type $t3 (tuple $t1 string))
      (type $f16 (func (param "x" $t3)))
      (export "p" (func (type $f16)))
      (type $t4' (flags "a" "b" "c"))
      (export $t4 "flags1"  (type (eq $t4')))
      (type $f17 (func (param "x" $t4)))
      (export "q" (func (type $f17)))
      (type $t5' (enum "a" "b" "c"))
      (export $t5 "enum1"  (type (eq $t5')))
      (type $f18 (func (param "x" $t5)))
      (export "r" (func (type $f18)))
      (type $t6' (option $t1))
      (export $t6 "option1"  (type (eq $t6')))
      (type $f19 (func (param "x" $t6)))
      (export "s" (func (type $f19)))
      (type $t7' (variant (case "a" s8) (case "b" u8) (case "c" s16) (case "d" u16) (case "e" s32) (case "f" u32) (case "g" s64) (case "h" u64) (case "i" float32) (case "j" float64) (case "k" bool) (case "l" string) (case "m" $t1)))
      (export $t7 "variant1"  (type (eq $t7')))
      (type $t8 (option $t7))
      (type $f20 (func (param "x" $t8)))
      (export "t" (func (type $f20)))
      (type $t9 (result $t1 (error string)))
      (type $f21 (func (result $t9)))
      (export "u" (func (type $f21)))
    )
  )
  (type $f22 (func (param "x" string) (result string)))
  (import "a" (instance $i1 (type $i1)))
  (core module $m0
    (import "a" "m" (func (param i32 i32)))
    (func $m (param i32 i32) (result i32) unreachable)
    (func $canonical_abi_realloc (param i32 i32 i32 i32) (result i32) unreachable)
    (memory 0)
    (export "memory" (memory 0))
    (export "m" (func $m))
    (export "canonical_abi_realloc" (func $canonical_abi_realloc))
  )
  (core module $m1
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
  (core module $m2
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
  (core instance $m1 (instantiate $m1))
  (core func $a (canon lower (func $i1 "a")))
  (core func $b (canon lower (func $i1 "b")))
  (core func $c (canon lower (func $i1 "c")))
  (core func $d (canon lower (func $i1 "d")))
  (core func $e (canon lower (func $i1 "e")))
  (core func $f (canon lower (func $i1 "f")))
  (core func $g (canon lower (func $i1 "g")))
  (core func $h (canon lower (func $i1 "h")))
  (core func $i (canon lower (func $i1 "i")))
  (core func $j (canon lower (func $i1 "j")))
  (core func $k (canon lower (func $i1 "k")))
  (core func $l (canon lower (func $i1 "l")))
  (core func $q (canon lower (func $i1 "q")))
  (core func $r (canon lower (func $i1 "r")))
  (core instance $m0 (instantiate $m0
      (with "a" (instance
        (export "m" (func $m1 "0"))
        (export "n" (func $m1 "1"))
        (export "o" (func $m1 "2"))
        (export "p" (func $m1 "3"))
        (export "s" (func $m1 "4"))
        (export "t" (func $m1 "5"))
        (export "u" (func $m1 "6"))
        (export "a" (func $a))
        (export "b" (func $b))
        (export "c" (func $c))
        (export "d" (func $d))
        (export "e" (func $e))
        (export "f" (func $f))
        (export "g" (func $g))
        (export "h" (func $h))
        (export "i" (func $i))
        (export "j" (func $j))
        (export "k" (func $k))
        (export "l" (func $l))
        (export "q" (func $q))
        (export "r" (func $r))
      ))
    )
  )
  (alias core export $m0 "memory" (core memory $m))
  (alias core export $m0 "canonical_abi_realloc" (core func $realloc))
  (alias core export $m1 "$imports" (core table $t))
  (core func (canon lower (func $i1 "m") (memory $m) (realloc $realloc) string-encoding=utf8))
  (core func (canon lower (func $i1 "n") (memory $m) (realloc $realloc) string-encoding=utf8))
  (core func (canon lower (func $i1 "o") (memory $m) (realloc $realloc) string-encoding=utf8))
  (core func (canon lower (func $i1 "p") (memory $m) (realloc $realloc) string-encoding=utf8))
  (core func (canon lower (func $i1 "s") (memory $m) (realloc $realloc) string-encoding=utf8))
  (core func (canon lower (func $i1 "t") (memory $m) (realloc $realloc) string-encoding=utf8))
  (core func (canon lower (func $i1 "u") (memory $m) (realloc $realloc) string-encoding=utf8))
  (core instance (instantiate $m2
    (with "" (instance
      (export "$imports" (table $t))
      (export "0" (func 22))
      (export "1" (func 23))
      (export "2" (func 24))
      (export "3" (func 25))
      (export "4" (func 26))
      (export "5" (func 27))
      (export "6" (func 28))
    ))
  ))
  (func $m (type $f22)
    (canon lift (core func $m0 "m") (memory $m) (realloc $realloc) string-encoding=utf8)
  )
  (instance $x (export "m" (func $m)))
  (export "x" (instance $x))
)
