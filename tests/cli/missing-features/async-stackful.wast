;; RUN: wast % --assert default --snapshot tests/snapshots \
;;        -f=cm-async,-cm-async-builtins

(assert_invalid
  (component
    (core module $m (func (export "foo") (param i32)))
    (core instance $i (instantiate $m))

    (func (export "foo") (param "p1" u32) (result u32)
      (canon lift (core func $i "foo") async)
    )
  )
  "requires the async stackful feature")

;; waitable-set.wait async
(assert_invalid
  (component
    (core module $libc (memory (export "memory") 1))
    (core instance $libc (instantiate $libc))
    (core func (canon waitable-set.wait async (memory $libc "memory")))
  )
  "requires the component model async stackful feature")

(component
  (core module $libc (memory (export "memory") 1))
  (core instance $libc (instantiate $libc))
  (core func (canon waitable-set.wait (memory $libc "memory")))
)

;; waitable-set.poll async
(assert_invalid
  (component
    (core module $libc (memory (export "memory") 1))
    (core instance $libc (instantiate $libc))
    (core func (canon waitable-set.poll async (memory $libc "memory")))
  )
  "requires the component model async stackful feature")

(component
  (core module $libc (memory (export "memory") 1))
  (core instance $libc (instantiate $libc))
  (core func (canon waitable-set.poll (memory $libc "memory")))
)

;; yield
(assert_invalid
  (component (core func (canon yield async)))
  "requires the component model async stackful feature")

(component (core func (canon yield)))

