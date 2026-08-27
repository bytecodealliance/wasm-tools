;; RUN: wast % --assert default --snapshot tests/snapshots -f cm-fixed-length-lists,cm-async,cm-map

(assert_invalid
  (component
    (type $a (list u8 268435447))
    (type (record (field "a" $a) (field "b" u64))))
  "exceeds maximum byte size")

(component
  (type $a (list u8 268435439))
  (type (record (field "a" $a) (field "b" u64))))

(assert_invalid
  (component
    (type $a (list u8 268435455))
    (component
      (alias outer 1 0 (type $t))
      (type (tuple $t u8))))
  "exceeds maximum byte size")

(assert_invalid
  (component
    (type $a (list u8 268435455))
    (import "i" (instance $i
      (alias outer 1 0 (type $a2))
      (export "t" (type (eq $a2)))))
    (alias export $i "t" (type $t))
    (type (list $t 2)))
  "exceeds maximum byte size")

(component
  (type $a (list u8 134217727))
  (import "i" (instance $i
    (alias outer 1 0 (type $a2))
    (export "t" (type (eq $a2)))))
  (alias export $i "t" (type $t))
  (type (list $t 2)))

(assert_invalid
  (component
    (type (component
      (type (list u64 33554432)))))
  "exceeds maximum byte size")

(assert_invalid
  (component
    (type (instance
      (type (list u64 33554432)))))
  "exceeds maximum byte size")

(component
  (type $r (resource (rep i32)))
  (type $big (list u8 268435455))
  (type (list (own $r) 1000))
  (type (list (future $big) 1000))
  (type (list (stream $big) 1000)))

(component
  (type $big (list u8 268435455))
  (type (list (map u8 $big) 16777215)))
