(module
  ;; Lowered imports, where pointers and lengths are `i64`.
  (import "foo:foo/options" "a" (func))
  (import "foo:foo/options" "b" (func (param i64 i64)))
  (import "foo:foo/options" "c" (func (param i64 i64)))
  (import "foo:foo/options" "d" (func (param i32 i64 i64)))
  (import "foo:foo/options" "e" (func (param i32)))
  (import "foo:foo/options" "f" (func (param i32 i32)))
  (import "foo:foo/options" "g" (func (param i64 i64)))
  (import "foo:foo/options" "h" (func (param i64 i64)))
  (import "foo:foo/options" "i" (func (param i64 i64)))
  (import "foo:foo/options" "j" (func (param i32)))
  (import "foo:foo/options" "k" (func (param i64)))
  (import "foo:foo/options" "l" (func (param i64)))
  (import "foo:foo/options" "m" (func (param i64)))
  (import "foo:foo/options" "n" (func (result i32)))
  (import "foo:foo/options" "o" (func (param i64)))
  (import "foo:foo/options" "p" (func (param i64)))

  ;; Variants join pointers with other types in their payloads.
  (import "foo:foo/variants" "option-string" (func (param i32 i64 i64 i64)))
  (import "foo:foo/variants" "result-string-u64" (func (param i32 i64 i64 i64)))
  (import "foo:foo/variants" "result-list-f32" (func (param i32 i64 i64)))
  (import "foo:foo/variants" "take-mixed" (func (param i32 i64 i64)))
  (import "foo:foo/variants" "tup" (func (param i32 i64 i64 i64 i64 i64)))
  (import "foo:foo/variants" "many-params" (func (param i64)))

  ;; Imported resources are always `i32` handles.
  (import "foo:foo/imp" "[resource-drop]b" (func (param i32)))
  (import "foo:foo/imp" "[constructor]b" (func (param i64 i64) (result i32)))
  (import "foo:foo/imp" "[method]b.name" (func (param i32 i64)))
  (import "foo:foo/imp" "[method]b.set-name" (func (param i32 i64 i64)))
  (import "foo:foo/imp" "[static]b.from-list" (func (param i64 i64) (result i32)))

  ;; Exported resources have an `i64` representation.
  (import "[export]foo:foo/exp" "[resource-new]a" (func $new (param i64) (result i32)))
  (import "[export]foo:foo/exp" "[resource-rep]a" (func (param i32) (result i64)))
  (import "[export]foo:foo/exp" "[resource-drop]a" (func (param i32)))

  ;; Lifted exports.
  (func (export "foo:foo/options#a") unreachable)
  (func (export "foo:foo/options#b") (param i64 i64) unreachable)
  (func (export "foo:foo/options#c") (param i64 i64) unreachable)
  (func (export "foo:foo/options#d") (param i32 i64 i64) unreachable)
  (func (export "foo:foo/options#e") (param i32) unreachable)
  (func (export "foo:foo/options#f") (param i32 i32) unreachable)
  (func (export "foo:foo/options#g") (param i64 i64) unreachable)
  (func (export "foo:foo/options#h") (param i64 i64) unreachable)
  (func (export "foo:foo/options#i") (param i64 i64) unreachable)
  (func (export "foo:foo/options#j") (param i32) unreachable)
  (func (export "foo:foo/options#k") (result i64) unreachable)
  (func (export "foo:foo/options#l") (result i64) unreachable)
  (func (export "cabi_post_foo:foo/options#l") (param i64) unreachable)
  (func (export "foo:foo/options#m") (result i64) unreachable)
  (func (export "cabi_post_foo:foo/options#m") (param i64) unreachable)
  (func (export "foo:foo/options#n") (result i32) unreachable)
  (func (export "foo:foo/options#o") (result i64) unreachable)
  (func (export "cabi_post_foo:foo/options#o") (param i64) unreachable)
  (func (export "foo:foo/options#p") (result i64) unreachable)
  (func (export "cabi_post_foo:foo/options#p") (param i64) unreachable)

  (func (export "foo:foo/variants#option-string") (param i32 i64 i64) (result i64) unreachable)
  (func (export "cabi_post_foo:foo/variants#option-string") (param i64))
  (func (export "foo:foo/variants#result-string-u64") (param i32 i64 i64) (result i64) unreachable)
  (func (export "cabi_post_foo:foo/variants#result-string-u64") (param i64))
  (func (export "foo:foo/variants#result-list-f32") (param i32 i64 i64) unreachable)
  (func (export "foo:foo/variants#take-mixed") (param i32 i64 i64) unreachable)
  (func (export "foo:foo/variants#tup") (param i32 i64 i64 i64 i64 i64) unreachable)
  (func (export "foo:foo/variants#many-params") (param i64) unreachable)

  (func (export "foo:foo/exp#[constructor]a") (param i64 i64) (result i32)
    (call $new (i64.const 100))
  )
  (func (export "foo:foo/exp#[dtor]a") (param i64))
  (func (export "foo:foo/exp#[static]a.other-new") (result i32)
    (call $new (i64.const 200))
  )

  ;; `self` is the `i64` representation of the resource.
  (func (export "foo:foo/exp#[method]a.name") (param i64) (result i64)
    unreachable
  )
  (func (export "cabi_post_foo:foo/exp#[method]a.name") (param i64))
  (func (export "foo:foo/exp#[method]a.set-name") (param i64 i64 i64)
    unreachable
  )
  ;; A `borrow` of a locally-defined resource is its `i64` representation.
  (func (export "foo:foo/exp#[method]a.merge") (param i64 i64)
    unreachable
  )
  ;; A `borrow` of an imported resource is an `i32` handle.
  (func (export "foo:foo/exp#[method]a.with-imported") (param i64 i32)
    unreachable
  )
  (func (export "foo:foo/exp#[method]a.many") (param i64)
    unreachable
  )
  (func (export "foo:foo/exp#take-both") (param i64 i32 i32 i64)
    unreachable
  )
  ;; Resources from an exported dependency are also locally-defined.
  (func (export "foo:foo/exp2#take-a") (param i64)
    unreachable
  )

  (memory (export "memory") i64 1)
  (func (export "cabi_realloc") (param i64 i64 i64 i64) (result i64) unreachable)
)
