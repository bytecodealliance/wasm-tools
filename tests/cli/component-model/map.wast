;; RUN: wast % --assert default --snapshot tests/snapshots -f cm-map,cm-error-context

(component
  (core module $m
    (memory (export "memory") 1)
    (func (export "ret-map") (result i32) unreachable)
  )
  (core instance $i (instantiate $m))

  (func (export "ret-map") (result (map string u32))
    (canon lift (core func $i "ret-map") (memory (core memory $i "memory")))
  )
)

(component
  (core module $m
    (memory (export "memory") 1)
    (func (export "param-map") (param i32 i32) unreachable)
    (func (export "realloc") (param i32 i32 i32 i32) (result i32) unreachable)
  )
  (core instance $i (instantiate $m))

  (func (export "param-map") (param "m" (map string u32))
    (canon lift (core func $i "param-map") (memory (core memory $i "memory")) (realloc (core func $i "realloc")))
  )
)

(component
  (type $map-type (map u32 string))
  (import "f" (func (param "x" $map-type)))
)

(component
  (type $nested-map (map string (map string u32)))
  (import "f" (func (param "x" $nested-map)))
)

(component
  (type $map-with-list (map string (list u32)))
  (import "f" (func (param "x" $map-with-list)))
)

(component
  (type $map-with-option (map u32 (option string)))
  (import "f" (func (param "x" $map-with-option)))
)

(assert_invalid
  (component
    (import "y" (component $c
      (type $t (map string u32))
      (import "x" (type (eq $t)))
    ))

    (type $x (map u32 string))
    (instance (instantiate $c (with "x" (type $x))))
  )
  "type mismatch for import `x`")

(assert_invalid
  (component
    (import "y" (component $c
      (type $t (map string u32))
      (import "x" (type (eq $t)))
    ))

    (type $x (list u32))
    (instance (instantiate $c (with "x" (type $x))))
  )
  "type mismatch for import `x`")


;; map keys are restricted to bool, integers, char and string
(component
  (type (map bool u32))
  (type (map s8 u32))
  (type (map u8 u32))
  (type (map s16 u32))
  (type (map u16 u32))
  (type (map s32 u32))
  (type (map u32 u32))
  (type (map s64 u32))
  (type (map u64 u32))
  (type (map char u32))
  (type (map string u32))
  (type $s string)
  (type (map $s u32))
)
(assert_invalid (component (type (map f32 u32))) "invalid map key type")
(assert_invalid (component (type (map f64 u32))) "invalid map key type")
(assert_invalid (component (type $l (list u8)) (type (map $l u32))) "invalid map key type")
(assert_invalid (component (type $r (record (field "a" u32))) (type (map $r u32))) "invalid map key type")
(assert_invalid (component (type $o (option u32)) (type (map $o u32))) "invalid map key type")
(assert_invalid
  (component
    (type $res (resource (rep i32)))
    (type $own (own $res))
    (type (map $own u32)))
  "invalid map key type")

;; map values can't contain handles
(component
  (type (stream (map string u32)))
  (type (future (map string u32)))
)
(assert_invalid
  (component
    (type $r (resource (rep i32)))
    (type (map u32 (own $r))))
  "map value type cannot contain a handle")
(assert_invalid
  (component
    (type $r (resource (rep i32)))
    (type (map string (option (borrow $r)))))
  "map value type cannot contain a handle")
(assert_invalid
  (component (type (map string (record (field "f" (future u8))))))
  "map value type cannot contain a handle")
(assert_invalid
  (component (type (map u8 (list (stream u8)))))
  "map value type cannot contain a handle")
(assert_invalid
  (component (type (map u8 (tuple u32 error-context))))
  "map value type cannot contain a handle")
(assert_invalid
  (component
    (import "r" (type $r (sub resource)))
    (type $v (variant (case "a") (case "b" (own $r))))
    (import "f" (func (param "m" (map string (map string $v))))))
  "map value type cannot contain a handle")

;; map values in scripts: entries keep their order and duplicate keys
(component
  (core module $m
    (memory (export "memory") 1)
    (func (export "echo") (param i32 i32) (result i32) unreachable)
    (func (export "realloc") (param i32 i32 i32 i32) (result i32) unreachable)
  )
  (core instance $i (instantiate $m))
  (func (export "echo") (param "m" (map u32 string)) (result (map u32 string))
    (canon lift (core func $i "echo") (memory (core memory $i "memory")) (realloc (core func $i "realloc")))
  )
)
(assert_return (invoke "echo" (map.const)) (map.const))
(assert_return
  (invoke "echo"
    (map.const
      (entry (u32.const 2) (str.const "a"))
      (entry (u32.const 1) (str.const "b"))
      (entry (u32.const 2) (str.const "c"))))
  (map.const
    (entry (u32.const 2) (str.const "a"))
    (entry (u32.const 1) (str.const "b"))
    (entry (u32.const 2) (str.const "c"))))
