;; RUN: wast % --assert default --snapshot tests/snapshots -f cm-map

(component
  (core module $m
    (memory (export "memory") 1)
    (func (export "ret-map") (result i32) unreachable)
  )
  (core instance $i (instantiate $m))

  (func (export "ret-map") (result (map string u32))
    (canon lift (core func $i "ret-map") (memory $i "memory"))
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
    (canon lift (core func $i "param-map") (memory $i "memory") (realloc (func $i "realloc")))
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

