;; RUN: wast % --assert default --snapshot tests/snapshots \
;;        -f=cm-async,-cm-async-builtins

;; waitable-set.poll async
(assert_invalid
  (component
    (core module $libc (memory (export "memory") 1))
    (core instance $libc (instantiate $libc))
    (core func (canon waitable-set.poll async (memory $libc "memory")))
  )
  "requires the component model async builtins feature")

(component
  (core module $libc (memory (export "memory") 1))
  (core instance $libc (instantiate $libc))
  (core func (canon waitable-set.poll (memory $libc "memory")))
)

;; yield
(assert_invalid
  (component (core func (canon yield async)))
  "requires the component model async builtins feature")

(component (core func (canon yield)))

;; {future,stream}.cancel-{read,write}
(assert_invalid
  (component
    (type $t (future))
    (core func (canon future.cancel-read $t async)))
  "requires the component model async builtins feature")
(assert_invalid
  (component
    (type $t (future))
    (core func (canon future.cancel-write $t async)))
  "requires the component model async builtins feature")
(assert_invalid
  (component
    (type $t (stream))
    (core func (canon stream.cancel-read $t async)))
  "requires the component model async builtins feature")
(assert_invalid
  (component
    (type $t (stream))
    (core func (canon stream.cancel-write $t async)))
  "requires the component model async builtins feature")

(component
  (type $f (future))
  (type $s (stream))
  (core func (canon future.cancel-read $f))
  (core func (canon future.cancel-write $f))
  (core func (canon stream.cancel-read $s))
  (core func (canon stream.cancel-write $s))
)
