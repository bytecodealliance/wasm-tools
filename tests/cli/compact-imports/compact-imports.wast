;; RUN: wast --assert default --snapshot tests/snapshots % -f compact-imports

(module
  (func (export "fi32") (result i32) i32.const 123)
  (func (export "fi64") (result i64) i64.const 456)
  (global (export "g1") i32 (i32.const 1))
  (global (export "g20") i32 (i32.const 20))
  (global (export "g300") i32 (i32.const 300))
  (global (export "g4000") i32 (i32.const 4000))
)
(register "test")

(module
  (import "test"
    (item "fi32" (func $beep (result i32)))
    (item "fi64" (func $boop (result i64)))
  )
  (import "test"
    (item "g1")
    (item "g20")
    (item "g300")
    (item "g4000")
    (global i32)
  )

  (func (export "testFuncs") (result i32)
    call $beep
    call $boop
    i32.wrap_i64
    i32.add
  )
  (func (export "testGlobals") (result i32)
    global.get 0
    global.get 1
    global.get 2
    global.get 3
    i32.add
    i32.add
    i32.add
  )
)
(assert_return (invoke "testFuncs") (i32.const 579))
(assert_return (invoke "testGlobals") (i32.const 4321))
