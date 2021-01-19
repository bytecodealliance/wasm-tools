(module
  (func
    atomic.fence)
)

(module
  (memory 1)
  (func (result i32)
    i32.const 0
    i32.const 0
    memory.atomic.notify offset=0
  )
)

;; Disabled because memory indices are not currently supported
;;
;; (assert_invalid
;;   (module
;;     (memory 1)
;;     (func (result i32)
;;       i32.const 0
;;       i32.const 0
;;       i64.const 0
;;       memory.atomic.wait32 1 offset=0
;;     )
;;   )
;;   "unknown memory 1")
