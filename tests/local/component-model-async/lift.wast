;; async lift; no callback
(component
  (core module $m
    (func (export "foo") (param i32) unreachable)
  )
  (core instance $i (instantiate $m))

  (func (export "foo") (param "p1" u32) (result u32)
    (canon lift (core func $i "foo") async)
  )
)

;; async lift; no callback; with post-return
(assert_invalid
  (component
    (core module $m
      (func (export "foo") (param i32) unreachable)
      (func (export "post-return-foo") unreachable)
    )
    (core instance $i (instantiate $m))

    (func (export "foo") (param "p1" u32) (result u32)
      (canon lift (core func $i "foo") async (post-return (func $i "post-return-foo")))
    )
  )
  "cannot specify post-return function when lifting async"
)

;; async lift; with callback
(component
  (core module $m
    (func (export "callback") (param i32 i32 i32 i32) (result i32) unreachable)
    (func (export "foo") (param i32) (result i32) unreachable)
  )
  (core instance $i (instantiate $m))

  (func (export "foo") (param "p1" u32) (result u32)
    (canon lift (core func $i "foo") async (callback (func $i "callback")))
  )
)

;; async lift; with incorrectly-typed callback
(assert_invalid
  (component
    (core module $m
      (func (export "callback") (param i32 i32 f32 i32) (result i32) unreachable)
      (func (export "foo") (param i32) (result i32) unreachable)
    )
    (core instance $i (instantiate $m))

    (func (export "foo") (param "p1" u32) (result u32)
      (canon lift (core func $i "foo") async (callback (func $i "callback")))
    )
  )
  "canonical option `callback` uses a core function with an incorrect signature"
)

;; async lift; with callback and post-return
(assert_invalid
  (component
    (core module $m
      (func (export "callback") (param i32 i32 i32 i32) (result i32) unreachable)
      (func (export "foo") (param i32) (result i32) unreachable)
      (func (export "post-return-foo") (param i32) unreachable)
    )
    (core instance $i (instantiate $m))

    (func (export "foo") (param "p1" u32) (result u32)
      (canon lift (core func $i "foo") async (callback (func $i "callback")) (post-return (func $i "post-return-foo")))
    )
  )
  "cannot specify post-return function when lifting async"
)

;; async lift; with incorrectly-typed core function
(assert_invalid
  (component
    (core module $m
      (func (export "callback") (param i32 i32 i32 i32) (result i32) unreachable)
      (func (export "foo") (param i32 i32) (result i32) unreachable)
    )
    (core instance $i (instantiate $m))

    (func (export "foo") (param "p1" u32) (result u32)
      (canon lift (core func $i "foo") async (callback (func $i "callback")))
    )
  )
  "lowered parameter types `[I32]` do not match parameter types `[I32, I32]` of core function 0"
)

;; async lift; with missing callback
(assert_invalid
  (component
    (core module $m
      (func (export "foo") (param i32) (result i32) unreachable)
    )
    (core instance $i (instantiate $m))

    (func (export "foo") (param "p1" u32) (result u32)
      (canon lift (core func $i "foo") async (callback (func $i "callback")))
    )
  )
  "core instance 0 has no export named `callback`"
)

;; sync lift; with redundant callback
(assert_invalid
  (component
    (core module $m
      (func (export "callback") (param i32 i32 i32 i32) (result i32) unreachable)
      (func (export "foo") (param i32) (result i32) unreachable)
    )
    (core instance $i (instantiate $m))

    (func (export "foo") (param "p1" u32) (result u32)
      (canon lift (core func $i "foo") (callback (func $i "callback")))
    )
  )
  "cannot specify callback without lifting async"
)
