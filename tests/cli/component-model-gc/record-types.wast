;; RUN: wast --assert default --snapshot tests/snapshots % -f gc,cm-gc

(component
  (type $record (record (field "a" bool)
                        (field "b" u8)
                        (field "c" s8)
                        (field "d" u16)
                        (field "e" s16)
                        (field "f" u32)
                        (field "g" s32)
                        (field "h" u64)
                        (field "i" s64)
                        (field "j" f32)
                        (field "k" f64)
                        (field "l" char)
                        (field "m" string)))

  (core type $string (array i8))
  (core type $record (struct (field i8)
                             (field i8)
                             (field i8)
                             (field i16)
                             (field i16)
                             (field i32)
                             (field i32)
                             (field i64)
                             (field i64)
                             (field f32)
                             (field f64)
                             (field i32)
                             (field (ref $string))))
  (core type $ty (func (param (ref $record))))

  (import "i" (instance $i
                (export $record' "ty" (type (eq $record)))
                (export "f" (func (param "x" $record')))))
  (core func (canon lower (func $i "f") gc string-encoding=utf8 (core-type $ty)))
)

(assert_invalid
  (component
    (type $record (record (field "a" bool)))

    (core type $record (struct (field i8)
                               (field i8)))
    (core type $ty (func (param (ref $record))))

    (import "i" (instance $i
                  (export $record' "ty" (type (eq $record)))
                  (export "f" (func (param "x" $record')))))
    (core func (canon lower (func $i "f") gc string-encoding=utf8 (core-type $ty)))
  )
  "core `struct` has 2 fields, but component `record` has 1 fields"
)

(assert_invalid
  (component
    (type $record (record (field "a" bool)
                          (field "b" bool)))

    (core type $record (struct (field i8)))
    (core type $ty (func (param (ref $record))))

    (import "i" (instance $i
                  (export $record' "ty" (type (eq $record)))
                  (export "f" (func (param "x" $record')))))
    (core func (canon lower (func $i "f") gc string-encoding=utf8 (core-type $ty)))
  )
  "core `struct` has 1 fields, but component `record` has 2 fields"
)

(assert_invalid
  (component
    (type $record (record (field "a" bool)
                          (field "b" bool)))

    (core type $ty (func (param i32)))

    (import "i" (instance $i
                  (export $record' "ty" (type (eq $record)))
                  (export "f" (func (param "x" $record')))))
    (core func (canon lower (func $i "f") gc string-encoding=utf8 (core-type $ty)))
  )
  "expected to lower component `record` type to core `(ref null? (struct ...))`, but found `i32`"
)
