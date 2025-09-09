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
  "requires the component model async stackful feature")

;; waitable-set.wait cancellable
(assert_invalid
  (component
    (core module $libc (memory (export "memory") 1))
    (core instance $libc (instantiate $libc))
    (core func (canon waitable-set.wait cancellable (memory $libc "memory")))
  )
  "requires the component model async stackful feature")

(component
  (core module $libc (memory (export "memory") 1))
  (core instance $libc (instantiate $libc))
  (core func (canon waitable-set.wait (memory $libc "memory")))
)

;; waitable-set.poll cancellable
(assert_invalid
  (component
    (core module $libc (memory (export "memory") 1))
    (core instance $libc (instantiate $libc))
    (core func (canon waitable-set.poll cancellable (memory $libc "memory")))
  )
  "requires the component model async stackful feature")

(component
  (core module $libc (memory (export "memory") 1))
  (core instance $libc (instantiate $libc))
  (core func (canon waitable-set.poll (memory $libc "memory")))
)

;; thread.yield
(assert_invalid
  (component (core func (canon thread.yield cancellable)))
  "requires the component model async stackful feature")

(component (core func (canon thread.yield)))