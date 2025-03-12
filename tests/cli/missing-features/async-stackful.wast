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
