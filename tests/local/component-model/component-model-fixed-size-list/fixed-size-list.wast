(component
  (core module $m
    (memory (export "memory") 1)
    (func (export "ret-list") (result i32) unreachable)
  )
  (core instance $i (instantiate $m))

  (func (export "ret-list") (result (list u32 4))
    (canon lift (core func $i "ret-list") (memory $i "memory"))
  )
)

(component
  (core module $m
    (func (export "param-list") (param i32 i32 i32 i32) unreachable)
  )
  (core instance $i (instantiate $m))

  (func (export "param-list") (param "l" (list u32 4))
    (canon lift (core func $i "param-list"))
  )
)

;; no easy way to check for an invalid u32?
;;(assert_invalid
;;  (component
;;    (core module $m
;;      (memory (export "memory") 1)
;;      (func (export "ret-list") (result i32) unreachable)
;;    )
;;    (core instance $i (instantiate $m))
;;
;;    (func (export "ret-list") (result (list u32 10000000000))
;;      (canon lift (core func $i "ret-list") (memory $i "memory"))
;;    )
;;  )
;;  "invalid u32 number: constant out of range"
;;)
