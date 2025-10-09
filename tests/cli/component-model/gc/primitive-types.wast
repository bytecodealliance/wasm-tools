;; RUN: wast --assert default --snapshot tests/snapshots % -f gc,cm-gc

;; bool

(component
  (import "f" (func $f (param "x" bool)))
  (core type $ty (func (param i32)))
  (core func (canon lower (func $f) gc (core-type $ty)))
)

(assert_invalid
  (component
    (import "f" (func $f (param "x" bool)))
    (core type $ty (func (param anyref)))
    (core func (canon lower (func $f) gc (core-type $ty)))
  )
  "expected to lower component `bool` type to core `i32` type, found `anyref`"
)

(component
  (import "f" (func $f (param "x" (list bool))))
  (core type $list (array i8))
  (core type $ty (func (param (ref $list))))
  (core func (canon lower (func $f) gc (core-type $ty)))
)

(assert_invalid
  (component
    (import "f" (func $f (param "x" (list bool))))
    (core type $list (array i32))
    (core type $ty (func (param (ref $list))))
    (core func (canon lower (func $f) gc (core-type $ty)))
  )
  "expected to lower component `bool` type to core `i8` type, found `i32`"
)

;; u8

(component
  (import "f" (func $f (param "x" u8)))
  (core type $ty (func (param i32)))
  (core func (canon lower (func $f) gc (core-type $ty)))
)

(assert_invalid
  (component
    (import "f" (func $f (param "x" u8)))
    (core type $ty (func (param anyref)))
    (core func (canon lower (func $f) gc (core-type $ty)))
  )
  "expected to lower component `u8` type to core `i32` type, found `anyref`"
)

(component
  (import "f" (func $f (param "x" (list u8))))
  (core type $list (array i8))
  (core type $ty (func (param (ref $list))))
  (core func (canon lower (func $f) gc (core-type $ty)))
)

(assert_invalid
  (component
    (import "f" (func $f (param "x" (list u8))))
    (core type $list (array i32))
    (core type $ty (func (param (ref $list))))
    (core func (canon lower (func $f) gc (core-type $ty)))
  )
  "expected to lower component `u8` type to core `i8` type, found `i32`"
)

;; s8

(component
  (import "f" (func $f (param "x" s8)))
  (core type $ty (func (param i32)))
  (core func (canon lower (func $f) gc (core-type $ty)))
)

(assert_invalid
  (component
    (import "f" (func $f (param "x" s8)))
    (core type $ty (func (param anyref)))
    (core func (canon lower (func $f) gc (core-type $ty)))
  )
  "expected to lower component `s8` type to core `i32` type, found `anyref`"
)

(component
  (import "f" (func $f (param "x" (list s8))))
  (core type $list (array i8))
  (core type $ty (func (param (ref $list))))
  (core func (canon lower (func $f) gc (core-type $ty)))
)

(assert_invalid
  (component
    (import "f" (func $f (param "x" (list s8))))
    (core type $list (array i32))
    (core type $ty (func (param (ref $list))))
    (core func (canon lower (func $f) gc (core-type $ty)))
  )
  "expected to lower component `s8` type to core `i8` type, found `i32`"
)

;; u16

(component
  (import "f" (func $f (param "x" u16)))
  (core type $ty (func (param i32)))
  (core func (canon lower (func $f) gc (core-type $ty)))
)

(assert_invalid
  (component
    (import "f" (func $f (param "x" u16)))
    (core type $ty (func (param anyref)))
    (core func (canon lower (func $f) gc (core-type $ty)))
  )
  "expected to lower component `u16` type to core `i32` type, found `anyref`"
)

(component
  (import "f" (func $f (param "x" (list u16))))
  (core type $list (array i16))
  (core type $ty (func (param (ref $list))))
  (core func (canon lower (func $f) gc (core-type $ty)))
)

(assert_invalid
  (component
    (import "f" (func $f (param "x" (list u16))))
    (core type $list (array i32))
    (core type $ty (func (param (ref $list))))
    (core func (canon lower (func $f) gc (core-type $ty)))
  )
  "expected to lower component `u16` type to core `i16` type, found `i32`"
)

;; s16

(component
  (import "f" (func $f (param "x" s16)))
  (core type $ty (func (param i32)))
  (core func (canon lower (func $f) gc (core-type $ty)))
)

(assert_invalid
  (component
    (import "f" (func $f (param "x" s16)))
    (core type $ty (func (param anyref)))
    (core func (canon lower (func $f) gc (core-type $ty)))
  )
  "expected to lower component `s16` type to core `i32` type, found `anyref`"
)

(component
  (import "f" (func $f (param "x" (list s16))))
  (core type $list (array i16))
  (core type $ty (func (param (ref $list))))
  (core func (canon lower (func $f) gc (core-type $ty)))
)

(assert_invalid
  (component
    (import "f" (func $f (param "x" (list s16))))
    (core type $list (array i32))
    (core type $ty (func (param (ref $list))))
    (core func (canon lower (func $f) gc (core-type $ty)))
  )
  "expected to lower component `s16` type to core `i16` type, found `i32`"
)

;; u32

(component
  (import "f" (func $f (param "x" u32)))
  (core type $ty (func (param i32)))
  (core func (canon lower (func $f) gc (core-type $ty)))
)

(assert_invalid
  (component
    (import "f" (func $f (param "x" u32)))
    (core type $ty (func (param anyref)))
    (core func (canon lower (func $f) gc (core-type $ty)))
  )
  "expected to lower component `u32` type to core `i32` type, found `anyref`"
)

(component
  (import "f" (func $f (param "x" (list u32))))
  (core type $list (array i32))
  (core type $ty (func (param (ref $list))))
  (core func (canon lower (func $f) gc (core-type $ty)))
)

(assert_invalid
  (component
    (import "f" (func $f (param "x" (list u32))))
    (core type $list (array anyref))
    (core type $ty (func (param (ref $list))))
    (core func (canon lower (func $f) gc (core-type $ty)))
  )
  "expected to lower component `u32` type to core `i32` type, found `anyref`"
)

;; s32

(component
  (import "f" (func $f (param "x" s32)))
  (core type $ty (func (param i32)))
  (core func (canon lower (func $f) gc (core-type $ty)))
)

(assert_invalid
  (component
    (import "f" (func $f (param "x" s32)))
    (core type $ty (func (param anyref)))
    (core func (canon lower (func $f) gc (core-type $ty)))
  )
  "expected to lower component `s32` type to core `i32` type, found `anyref`"
)

(component
  (import "f" (func $f (param "x" (list s32))))
  (core type $list (array i32))
  (core type $ty (func (param (ref $list))))
  (core func (canon lower (func $f) gc (core-type $ty)))
)

(assert_invalid
  (component
    (import "f" (func $f (param "x" (list s32))))
    (core type $list (array anyref))
    (core type $ty (func (param (ref $list))))
    (core func (canon lower (func $f) gc (core-type $ty)))
  )
  "expected to lower component `s32` type to core `i32` type, found `anyref`"
)

;; u64

(component
  (import "f" (func $f (param "x" u64)))
  (core type $ty (func (param i64)))
  (core func (canon lower (func $f) gc (core-type $ty)))
)

(assert_invalid
  (component
    (import "f" (func $f (param "x" u64)))
    (core type $ty (func (param anyref)))
    (core func (canon lower (func $f) gc (core-type $ty)))
  )
  "expected to lower component `u64` type to core `i64` type, found `anyref`"
)

(component
  (import "f" (func $f (param "x" (list u64))))
  (core type $list (array i64))
  (core type $ty (func (param (ref $list))))
  (core func (canon lower (func $f) gc (core-type $ty)))
)

(assert_invalid
  (component
    (import "f" (func $f (param "x" (list u64))))
    (core type $list (array anyref))
    (core type $ty (func (param (ref $list))))
    (core func (canon lower (func $f) gc (core-type $ty)))
  )
  "expected to lower component `u64` type to core `i64` type, found `anyref`"
)

;; s64

(component
  (import "f" (func $f (param "x" s64)))
  (core type $ty (func (param i64)))
  (core func (canon lower (func $f) gc (core-type $ty)))
)

(assert_invalid
  (component
    (import "f" (func $f (param "x" s64)))
    (core type $ty (func (param anyref)))
    (core func (canon lower (func $f) gc (core-type $ty)))
  )
  "expected to lower component `s64` type to core `i64` type, found `anyref`"
)

(component
  (import "f" (func $f (param "x" (list s64))))
  (core type $list (array i64))
  (core type $ty (func (param (ref $list))))
  (core func (canon lower (func $f) gc (core-type $ty)))
)

(assert_invalid
  (component
    (import "f" (func $f (param "x" (list s64))))
    (core type $list (array anyref))
    (core type $ty (func (param (ref $list))))
    (core func (canon lower (func $f) gc (core-type $ty)))
  )
  "expected to lower component `s64` type to core `i64` type, found `anyref`"
)

;; f32

(component
  (import "f" (func $f (param "x" f32)))
  (core type $ty (func (param f32)))
  (core func (canon lower (func $f) gc (core-type $ty)))
)

(assert_invalid
  (component
    (import "f" (func $f (param "x" f32)))
    (core type $ty (func (param anyref)))
    (core func (canon lower (func $f) gc (core-type $ty)))
  )
  "expected to lower component `f32` type to core `f32` type, found `anyref`"
)

(component
  (import "f" (func $f (param "x" (list f32))))
  (core type $list (array f32))
  (core type $ty (func (param (ref $list))))
  (core func (canon lower (func $f) gc (core-type $ty)))
)

(assert_invalid
  (component
    (import "f" (func $f (param "x" (list f32))))
    (core type $list (array anyref))
    (core type $ty (func (param (ref $list))))
    (core func (canon lower (func $f) gc (core-type $ty)))
  )
  "expected to lower component `f32` type to core `f32` type, found `anyref`"
)

;; f64

(component
  (import "f" (func $f (param "x" f64)))
  (core type $ty (func (param f64)))
  (core func (canon lower (func $f) gc (core-type $ty)))
)

(assert_invalid
  (component
    (import "f" (func $f (param "x" f64)))
    (core type $ty (func (param anyref)))
    (core func (canon lower (func $f) gc (core-type $ty)))
  )
  "expected to lower component `f64` type to core `f64` type, found `anyref`"
)

(component
  (import "f" (func $f (param "x" (list f64))))
  (core type $list (array f64))
  (core type $ty (func (param (ref $list))))
  (core func (canon lower (func $f) gc (core-type $ty)))
)

(assert_invalid
  (component
    (import "f" (func $f (param "x" (list f64))))
    (core type $list (array anyref))
    (core type $ty (func (param (ref $list))))
    (core func (canon lower (func $f) gc (core-type $ty)))
  )
  "expected to lower component `f64` type to core `f64` type, found `anyref`"
)

;; char

(component
  (import "f" (func $f (param "x" char)))
  (core type $ty (func (param i32)))
  (core func (canon lower (func $f) gc (core-type $ty)))
)

(assert_invalid
  (component
    (import "f" (func $f (param "x" char)))
    (core type $ty (func (param anyref)))
    (core func (canon lower (func $f) gc (core-type $ty)))
  )
  "expected to lower component `char` type to core `i32` type, found `anyref`"
)

(component
  (import "f" (func $f (param "x" (list char))))
  (core type $list (array i32))
  (core type $ty (func (param (ref $list))))
  (core func (canon lower (func $f) gc (core-type $ty)))
)

(assert_invalid
  (component
    (import "f" (func $f (param "x" (list char))))
    (core type $list (array anyref))
    (core type $ty (func (param (ref $list))))
    (core func (canon lower (func $f) gc (core-type $ty)))
  )
  "expected to lower component `char` type to core `i32` type, found `anyref`"
)

;; utf8 strings

(component
  (import "f" (func $f (param "x" string)))
  (core type $s (array i8))
  (core type $ty (func (param (ref $s))))
  (core func (canon lower (func $f) gc string-encoding=utf8 (core-type $ty)))
)

(assert_invalid
  (component
    (import "f" (func $f (param "x" string)))
    (core type $ty (func (param anyref)))
    (core func (canon lower (func $f) gc string-encoding=utf8 (core-type $ty)))
  )
  "expected to lower component `string` type to core `(ref null? (array (mut? i8)))` type, found `anyref`"
)

(component
  (import "f" (func $f (param "x" (list string))))
  (core type $s (array i8))
  (core type $list (array (ref $s)))
  (core type $ty (func (param (ref $list))))
  (core func (canon lower (func $f) gc string-encoding=utf8 (core-type $ty)))
)

(assert_invalid
  (component
    (import "f" (func $f (param "x" (list string))))
    (core type $list (array anyref))
    (core type $ty (func (param (ref $list))))
    (core func (canon lower (func $f) gc string-encoding=utf8 (core-type $ty)))
  )
  "expected to lower component `string` type to core `(ref null? (array (mut? i8)))` type, found `anyref`"
)

;; utf16 strings

(component
  (import "f" (func $f (param "x" string)))
  (core type $s (array i16))
  (core type $ty (func (param (ref $s))))
  (core func (canon lower (func $f) gc string-encoding=utf16 (core-type $ty)))
)

(assert_invalid
  (component
    (import "f" (func $f (param "x" string)))
    (core type $ty (func (param anyref)))
    (core func (canon lower (func $f) gc string-encoding=utf16 (core-type $ty)))
  )
  "expected to lower component `string` type to core `(ref null? (array (mut? i16)))` type, found `anyref`"
)

(component
  (import "f" (func $f (param "x" (list string))))
  (core type $s (array i16))
  (core type $list (array (ref $s)))
  (core type $ty (func (param (ref $list))))
  (core func (canon lower (func $f) gc string-encoding=utf16 (core-type $ty)))
)

(assert_invalid
  (component
    (import "f" (func $f (param "x" (list string))))
    (core type $list (array anyref))
    (core type $ty (func (param (ref $list))))
    (core func (canon lower (func $f) gc string-encoding=utf16 (core-type $ty)))
  )
  "expected to lower component `string` type to core `(ref null? (array (mut? i16)))` type, found `anyref`"
)

;; compact utf16 strings

(component
  (import "f" (func $f (param "x" string)))
  (core type $s (array i8))
  (core type $ty (func (param (ref $s))))
  (core func (canon lower (func $f) gc string-encoding=latin1+utf16 (core-type $ty)))
)

(assert_invalid
  (component
    (import "f" (func $f (param "x" string)))
    (core type $ty (func (param anyref)))
    (core func (canon lower (func $f) gc string-encoding=latin1+utf16 (core-type $ty)))
  )
  "expected to lower component `string` type to core `(ref null? (array (mut? i8)))` type, found `anyref`"
)

(component
  (import "f" (func $f (param "x" (list string))))
  (core type $s (array i8))
  (core type $list (array (ref $s)))
  (core type $ty (func (param (ref $list))))
  (core func (canon lower (func $f) gc string-encoding=latin1+utf16 (core-type $ty)))
)

(assert_invalid
  (component
    (import "f" (func $f (param "x" (list string))))
    (core type $list (array anyref))
    (core type $ty (func (param (ref $list))))
    (core func (canon lower (func $f) gc string-encoding=latin1+utf16 (core-type $ty)))
  )
  "expected to lower component `string` type to core `(ref null? (array (mut? i8)))` type, found `anyref`"
)
