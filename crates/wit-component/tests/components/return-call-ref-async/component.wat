(component
  (core module $main (;0;)
    (type (;0;) (func))
    (type (;1;) (func (param i32 i32)))
    (type (;2;) (func (result i32)))
    (type (;3;) (func (param i32 i32) (result i32)))
    (type (;4;) (func (param i32)))
    (type (;5;) (func (param i32) (result i32)))
    (type (;6;) (func (param i32 i32 i32 i32) (result i32)))
    (import "$root" "[backpressure-inc]" (func (;0;) (type 0)))
    (import "$root" "[backpressure-dec]" (func (;1;) (type 0)))
    (import "[export]$root" "[task-cancel]" (func (;2;) (type 0)))
    (import "[export]$root" "[task-return]foo" (func (;3;) (type 1)))
    (import "[export]foo:foo/bar" "[task-return]foo" (func (;4;) (type 1)))
    (import "$root" "[waitable-set-new]" (func (;5;) (type 2)))
    (import "$root" "[waitable-set-wait]" (func (;6;) (type 3)))
    (import "$root" "[waitable-set-poll]" (func (;7;) (type 3)))
    (import "$root" "[waitable-set-drop]" (func (;8;) (type 4)))
    (import "$root" "[waitable-join]" (func (;9;) (type 1)))
    (import "$root" "[thread-yield]" (func (;10;) (type 2)))
    (import "$root" "[subtask-drop]" (func (;11;) (type 4)))
    (import "$root" "[subtask-cancel]" (func (;12;) (type 5)))
    (import "$root" "[error-context-new-utf8]" (func (;13;) (type 3)))
    (import "$root" "[error-context-new-utf16]" (func (;14;) (type 3)))
    (import "$root" "[error-context-new-latin1+utf16]" (func (;15;) (type 3)))
    (import "$root" "[error-context-debug-message-utf8]" (func (;16;) (type 1)))
    (import "$root" "[error-context-debug-message-utf16]" (func (;17;) (type 1)))
    (import "$root" "[error-context-debug-message-latin1+utf16]" (func (;18;) (type 1)))
    (import "$root" "[error-context-drop]" (func (;19;) (type 4)))
    (import "$root" "[context-get-0]" (func (;20;) (type 2)))
    (import "$root" "[context-set-0]" (func (;21;) (type 4)))
    (memory (;0;) 1)
    (export "[async-lift-stackful]foo" (func 22))
    (export "[async-lift-stackful]foo:foo/bar#foo" (func 23))
    (export "memory" (memory 0))
    (export "cabi_realloc" (func 24))
    (func (;22;) (type 1) (param i32 i32)
      unreachable
    )
    (func (;23;) (type 1) (param i32 i32)
      unreachable
    )
    (func (;24;) (type 6) (param i32 i32 i32 i32) (result i32)
      unreachable
    )
    (@producers
      (processed-by "wit-component" "$CARGO_PKG_VERSION")
      (processed-by "my-fake-bindgen" "123.45")
    )
  )
  (core module $wit-component-shim-module (;1;)
    (type (;0;) (func (param i32 i32) (result i32)))
    (type (;1;) (func (param i32 i32)))
    (type (;2;) (func (param i32 i32)))
    (global (;0;) (mut (ref 0)) ref.func $"trap stub before initialization")
    (global (;1;) (mut (ref 0)) ref.func $"trap stub before initialization")
    (global (;2;) (mut (ref 0)) ref.func $"trap stub before initialization")
    (global (;3;) (mut (ref 0)) ref.func $"trap stub before initialization")
    (global (;4;) (mut (ref 0)) ref.func $"trap stub before initialization")
    (global (;5;) (mut (ref 1)) ref.func $"#func6 trap stub before initialization")
    (global (;6;) (mut (ref 1)) ref.func $"#func6 trap stub before initialization")
    (global (;7;) (mut (ref 1)) ref.func $"#func6 trap stub before initialization")
    (global (;8;) (mut (ref 2)) ref.func $"#func10 trap stub before initialization")
    (global (;9;) (mut (ref 2)) ref.func $"#func10 trap stub before initialization")
    (export "g0" (global 0))
    (export "0" (func $waitable-set.wait))
    (export "g1" (global 1))
    (export "1" (func $waitable-set.poll))
    (export "g2" (global 2))
    (export "2" (func $error-new))
    (export "g3" (global 3))
    (export "3" (func $"#func4 error-new"))
    (export "g4" (global 4))
    (export "4" (func $"#func5 error-new"))
    (export "g5" (global 5))
    (export "5" (func $error-debug-message))
    (export "g6" (global 6))
    (export "6" (func $"#func8 error-debug-message"))
    (export "g7" (global 7))
    (export "7" (func $"#func9 error-debug-message"))
    (export "g8" (global 8))
    (export "8" (func $task-return-foo))
    (export "g9" (global 9))
    (export "9" (func $"#func12 task-return-foo"))
    (func $"trap stub before initialization" (;0;) (type 0) (param i32 i32) (result i32)
      unreachable
    )
    (func $waitable-set.wait (;1;) (type 0) (param i32 i32) (result i32)
      local.get 0
      local.get 1
      global.get 0
      return_call_ref 0
    )
    (func $waitable-set.poll (;2;) (type 0) (param i32 i32) (result i32)
      local.get 0
      local.get 1
      global.get 1
      return_call_ref 0
    )
    (func $error-new (;3;) (type 0) (param i32 i32) (result i32)
      local.get 0
      local.get 1
      global.get 2
      return_call_ref 0
    )
    (func $"#func4 error-new" (@name "error-new") (;4;) (type 0) (param i32 i32) (result i32)
      local.get 0
      local.get 1
      global.get 3
      return_call_ref 0
    )
    (func $"#func5 error-new" (@name "error-new") (;5;) (type 0) (param i32 i32) (result i32)
      local.get 0
      local.get 1
      global.get 4
      return_call_ref 0
    )
    (func $"#func6 trap stub before initialization" (@name "trap stub before initialization") (;6;) (type 1) (param i32 i32)
      unreachable
    )
    (func $error-debug-message (;7;) (type 1) (param i32 i32)
      local.get 0
      local.get 1
      global.get 5
      return_call_ref 1
    )
    (func $"#func8 error-debug-message" (@name "error-debug-message") (;8;) (type 1) (param i32 i32)
      local.get 0
      local.get 1
      global.get 6
      return_call_ref 1
    )
    (func $"#func9 error-debug-message" (@name "error-debug-message") (;9;) (type 1) (param i32 i32)
      local.get 0
      local.get 1
      global.get 7
      return_call_ref 1
    )
    (func $"#func10 trap stub before initialization" (@name "trap stub before initialization") (;10;) (type 2) (param i32 i32)
      unreachable
    )
    (func $task-return-foo (;11;) (type 2) (param i32 i32)
      local.get 0
      local.get 1
      global.get 8
      return_call_ref 2
    )
    (func $"#func12 task-return-foo" (@name "task-return-foo") (;12;) (type 2) (param i32 i32)
      local.get 0
      local.get 1
      global.get 9
      return_call_ref 2
    )
    (@producers
      (processed-by "wit-component" "$CARGO_PKG_VERSION")
    )
  )
  (core module $wit-component-fixup (;2;)
    (type (;0;) (func (param i32 i32) (result i32)))
    (type (;1;) (func (param i32 i32)))
    (type (;2;) (func (param i32 i32)))
    (type (;3;) (func))
    (import "shim" "g0" (global (;0;) (mut (ref 0))))
    (import "" "0" (func (;0;) (type 0)))
    (import "shim" "g1" (global (;1;) (mut (ref 0))))
    (import "" "1" (func (;1;) (type 0)))
    (import "shim" "g2" (global (;2;) (mut (ref 0))))
    (import "" "2" (func (;2;) (type 0)))
    (import "shim" "g3" (global (;3;) (mut (ref 0))))
    (import "" "3" (func (;3;) (type 0)))
    (import "shim" "g4" (global (;4;) (mut (ref 0))))
    (import "" "4" (func (;4;) (type 0)))
    (import "shim" "g5" (global (;5;) (mut (ref 1))))
    (import "" "5" (func (;5;) (type 1)))
    (import "shim" "g6" (global (;6;) (mut (ref 1))))
    (import "" "6" (func (;6;) (type 1)))
    (import "shim" "g7" (global (;7;) (mut (ref 1))))
    (import "" "7" (func (;7;) (type 1)))
    (import "shim" "g8" (global (;8;) (mut (ref 2))))
    (import "" "8" (func (;8;) (type 2)))
    (import "shim" "g9" (global (;9;) (mut (ref 2))))
    (import "" "9" (func (;9;) (type 2)))
    (start 10)
    (elem (;0;) declare func 0 1 2 3 4 5 6 7 8 9)
    (func (;10;) (type 3)
      ref.func 0
      global.set 0
      ref.func 1
      global.set 1
      ref.func 2
      global.set 2
      ref.func 3
      global.set 3
      ref.func 4
      global.set 4
      ref.func 5
      global.set 5
      ref.func 6
      global.set 6
      ref.func 7
      global.set 7
      ref.func 8
      global.set 8
      ref.func 9
      global.set 9
    )
    (@producers
      (processed-by "wit-component" "$CARGO_PKG_VERSION")
    )
  )
  (core instance $wit-component-shim-instance (;0;) (instantiate $wit-component-shim-module))
  (core func $backpressure.inc (;0;) (canon backpressure.inc))
  (core func $backpressure.dec (;1;) (canon backpressure.dec))
  (core func $waitable-set.new (;2;) (canon waitable-set.new))
  (alias core export $wit-component-shim-instance "0" (core func $waitable-set.wait (;3;)))
  (alias core export $wit-component-shim-instance "1" (core func $waitable-set.poll (;4;)))
  (core func $waitable-set.drop (;5;) (canon waitable-set.drop))
  (core func $waitable.join (;6;) (canon waitable.join))
  (core func $thread.yield (;7;) (canon thread.yield))
  (core func $subtask.drop (;8;) (canon subtask.drop))
  (core func $subtask.cancel (;9;) (canon subtask.cancel))
  (alias core export $wit-component-shim-instance "2" (core func $error-new (;10;)))
  (alias core export $wit-component-shim-instance "3" (core func $"#core-func11 error-new" (@name "error-new") (;11;)))
  (alias core export $wit-component-shim-instance "4" (core func $"#core-func12 error-new" (@name "error-new") (;12;)))
  (alias core export $wit-component-shim-instance "5" (core func $error-debug-message (;13;)))
  (alias core export $wit-component-shim-instance "6" (core func $"#core-func14 error-debug-message" (@name "error-debug-message") (;14;)))
  (alias core export $wit-component-shim-instance "7" (core func $"#core-func15 error-debug-message" (@name "error-debug-message") (;15;)))
  (core func $error-context.drop (;16;) (canon error-context.drop))
  (core func $"context.get 0" (;17;) (canon context.get i32 0))
  (core func $"context.set 0" (;18;) (canon context.set i32 0))
  (core instance $$root (;1;)
    (export "[backpressure-inc]" (func $backpressure.inc))
    (export "[backpressure-dec]" (func $backpressure.dec))
    (export "[waitable-set-new]" (func $waitable-set.new))
    (export "[waitable-set-wait]" (func $waitable-set.wait))
    (export "[waitable-set-poll]" (func $waitable-set.poll))
    (export "[waitable-set-drop]" (func $waitable-set.drop))
    (export "[waitable-join]" (func $waitable.join))
    (export "[thread-yield]" (func $thread.yield))
    (export "[subtask-drop]" (func $subtask.drop))
    (export "[subtask-cancel]" (func $subtask.cancel))
    (export "[error-context-new-utf8]" (func $error-new))
    (export "[error-context-new-utf16]" (func $"#core-func11 error-new"))
    (export "[error-context-new-latin1+utf16]" (func $"#core-func12 error-new"))
    (export "[error-context-debug-message-utf8]" (func $error-debug-message))
    (export "[error-context-debug-message-utf16]" (func $"#core-func14 error-debug-message"))
    (export "[error-context-debug-message-latin1+utf16]" (func $"#core-func15 error-debug-message"))
    (export "[error-context-drop]" (func $error-context.drop))
    (export "[context-get-0]" (func $"context.get 0"))
    (export "[context-set-0]" (func $"context.set 0"))
  )
  (core func $task.cancel (;19;) (canon task.cancel))
  (alias core export $wit-component-shim-instance "8" (core func $task-return-foo (;20;)))
  (core instance $"[export]$root" (;2;)
    (export "[task-cancel]" (func $task.cancel))
    (export "[task-return]foo" (func $task-return-foo))
  )
  (alias core export $wit-component-shim-instance "9" (core func $"#core-func21 task-return-foo" (@name "task-return-foo") (;21;)))
  (core instance $"[export]foo:foo/bar" (;3;)
    (export "[task-return]foo" (func $"#core-func21 task-return-foo"))
  )
  (core instance $main (;4;) (instantiate $main
      (with "$root" (instance $$root))
      (with "[export]$root" (instance $"[export]$root"))
      (with "[export]foo:foo/bar" (instance $"[export]foo:foo/bar"))
    )
  )
  (alias core export $main "memory" (core memory $memory (;0;)))
  (core func $"#core-func22 waitable-set.wait" (@name "waitable-set.wait") (;22;) (canon waitable-set.wait (memory $memory)))
  (core func $"#core-func23 waitable-set.poll" (@name "waitable-set.poll") (;23;) (canon waitable-set.poll (memory $memory)))
  (core func $error-context.new (;24;) (canon error-context.new (memory $memory) string-encoding=utf8))
  (core func $"#core-func25 error-context.new" (@name "error-context.new") (;25;) (canon error-context.new (memory $memory) string-encoding=utf16))
  (core func $"#core-func26 error-context.new" (@name "error-context.new") (;26;) (canon error-context.new (memory $memory) string-encoding=latin1+utf16))
  (alias core export $main "cabi_realloc" (core func $realloc (;27;)))
  (core func $error-context.debug-message (;28;) (canon error-context.debug-message (memory $memory) (realloc $realloc) string-encoding=utf8))
  (core func $"#core-func29 error-context.debug-message" (@name "error-context.debug-message") (;29;) (canon error-context.debug-message (memory $memory) (realloc $realloc) string-encoding=utf16))
  (core func $"#core-func30 error-context.debug-message" (@name "error-context.debug-message") (;30;) (canon error-context.debug-message (memory $memory) (realloc $realloc) string-encoding=latin1+utf16))
  (core func $task.return (;31;) (canon task.return (result string) (memory $memory) string-encoding=utf8))
  (core func $"#core-func32 task.return" (@name "task.return") (;32;) (canon task.return (result string) (memory $memory) string-encoding=utf8))
  (core instance $fixup-args (;5;)
    (export "0" (func $"#core-func22 waitable-set.wait"))
    (export "1" (func $"#core-func23 waitable-set.poll"))
    (export "2" (func $error-context.new))
    (export "3" (func $"#core-func25 error-context.new"))
    (export "4" (func $"#core-func26 error-context.new"))
    (export "5" (func $error-context.debug-message))
    (export "6" (func $"#core-func29 error-context.debug-message"))
    (export "7" (func $"#core-func30 error-context.debug-message"))
    (export "8" (func $task.return))
    (export "9" (func $"#core-func32 task.return"))
  )
  (core instance $fixup (;6;) (instantiate $wit-component-fixup
      (with "" (instance $fixup-args))
      (with "shim" (instance $wit-component-shim-instance))
    )
  )
  (type (;0;) (func async (param "s" string) (result string)))
  (alias core export $main "[async-lift-stackful]foo" (core func $"[async-lift-stackful]foo" (;33;)))
  (func $foo (;0;) (type 0) (canon lift (core func $"[async-lift-stackful]foo") (memory $memory) (realloc $realloc) string-encoding=utf8 async))
  (export $"#func1 foo" (@name "foo") (;1;) "foo" (func $foo))
  (alias core export $main "[async-lift-stackful]foo:foo/bar#foo" (core func $"[async-lift-stackful]foo:foo/bar#foo" (;34;)))
  (func $"#func2 foo" (@name "foo") (;2;) (type 0) (canon lift (core func $"[async-lift-stackful]foo:foo/bar#foo") (memory $memory) (realloc $realloc) string-encoding=utf8 async))
  (component $foo:foo/bar-shim-component (;0;)
    (type (;0;) (func async (param "s" string) (result string)))
    (import "import-func-foo" (func (;0;) (type 0)))
    (type (;1;) (func async (param "s" string) (result string)))
    (export (;1;) "foo" (func 0) (func (type 1)))
  )
  (instance $foo:foo/bar-shim-instance (;0;) (instantiate $foo:foo/bar-shim-component
      (with "import-func-foo" (func $"#func2 foo"))
    )
  )
  (export $foo:foo/bar (;1;) "foo:foo/bar" (instance $foo:foo/bar-shim-instance))
  (@producers
    (processed-by "wit-component" "$CARGO_PKG_VERSION")
  )
)
