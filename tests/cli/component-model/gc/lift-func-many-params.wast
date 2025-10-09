;; RUN: wast --assert default --snapshot tests/snapshots % -f gc,cm-gc

(component
  (core module $m
    (type $string (array i8))
    (type $list (array i64))
    (type $record (struct (field i32) (field f32)))
    (type $tuple (struct (field i8) (field i16)))

    (func (export "f") (param i32           ;; bool
                              i32           ;; s8
                              i32           ;; u8
                              i32           ;; s16
                              i32           ;; u16
                              i32           ;; s32
                              i32           ;; u32
                              i64           ;; s64
                              i64           ;; u64
                              f32           ;; f32
                              f64           ;; f64
                              (ref $string) ;; string
                              i32           ;; enum
                              i32           ;; flags
                              (ref $list)   ;; list
                              (ref $record) ;; record
                              (ref extern)  ;; (own resource)
                              (ref extern)  ;; (borrow resource)
                              (ref $tuple)  ;; tuple
                              ))
  )

  (core instance $i (instantiate $m))

  (type $enum' (enum "a" "b" "c"))
  (export $enum "enum" (type $enum'))

  (type $flags' (flags "d" "e" "f"))
  (export $flags "flags" (type $flags'))

  (type $list' (list u64))
  (export $list "list" (type $list'))

  (type $record' (record (field "g" u32) (field "h" f32)))
  (export $record "record" (type $record'))

  (type $resource' (resource (rep i32)))
  (export $resource "resource" (type $resource'))

  (type $tuple' (tuple u8 u16))
  (export $tuple "tuple" (type $tuple'))

  (func (export "f") (param "p0" bool)
                     (param "p1" s8)
                     (param "p2" u8)
                     (param "p3" s16)
                     (param "p4" u16)
                     (param "p5" s32)
                     (param "p6" u32)
                     (param "p7" s64)
                     (param "p8" u64)
                     (param "p9" f32)
                     (param "p10" f64)
                     (param "p11" string)
                     (param "p12" $enum)
                     (param "p13" $flags)
                     (param "p14" $list)
                     (param "p15" $record)
                     (param "p16" (own $resource))
                     (param "p17" (borrow $resource))
                     (param "p18" $tuple)
    (canon lift (core func $i "f") gc)
  )
)
