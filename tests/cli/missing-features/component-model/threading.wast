;; RUN: wast % --assert default --snapshot tests/snapshots \
;;        -f=cm-async,-cm-threading

(assert_invalid
  (component
    (core module $m (func (export "foo") (param i32)))
    (core instance $i (instantiate $m))

    (func (export "foo") (param "p1" u32) (result u32)
      (canon lift (core func $i "foo") async)
    )
  )
  "requires the component model threading feature")

;; waitable-set.wait async
(assert_invalid
  (component
    (core module $libc (memory (export "memory") 1))
    (core instance $libc (instantiate $libc))
    (core func (canon waitable-set.wait cancellable (memory $libc "memory")))
  )
  "requires the component model threading feature")

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
  "requires the component model threading feature")

(component
  (core module $libc (memory (export "memory") 1))
  (core instance $libc (instantiate $libc))
  (core func (canon waitable-set.poll (memory $libc "memory")))
)

;; thread.yield
(assert_invalid
  (component (core func (canon thread.yield cancellable)))
  "requires the component model threading feature")
(component (core func (canon thread.yield)))

;; {future,stream}.cancel-{read,write}
(assert_invalid
  (component
    (type $t (future))
    (core func (canon future.cancel-read $t async)))
  "requires the component model threading feature")
(assert_invalid
  (component
    (type $t (future))
    (core func (canon future.cancel-write $t async)))
  "requires the component model threading feature")
(assert_invalid
  (component
    (type $t (stream))
    (core func (canon stream.cancel-read $t async)))
  "requires the component model threading feature")
(assert_invalid
  (component
    (type $t (stream))
    (core func (canon stream.cancel-write $t async)))
  "requires the component model threading feature")

(component
  (type $f (future))
  (type $s (stream))
  (core func (canon future.cancel-read $f))
  (core func (canon future.cancel-write $f))
  (core func (canon stream.cancel-read $s))
  (core func (canon stream.cancel-write $s))
)

;; async resource.drop
(assert_invalid
  (component
    (type $t (resource (rep i32)))
    (core func (canon resource.drop $t async)))
  "requires the component model threading feature")
(component
  (type $t (resource (rep i32)))
  (core func (canon resource.drop $t)))

;; thread.*
(assert_invalid
  (component
    (core func (canon thread.index))
    (core func (canon thread.new_indirect 0 (table 0)))
    (core func (canon thread.switch-to))
    (core func (canon thread.suspend))
    (core func (canon thread.resume-later))
    (core func (canon thread.yield-to)))
  "requires the component model threading feature")