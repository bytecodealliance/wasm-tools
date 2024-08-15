;; RUN: component unbundle --threshold 0 % --module-dir %tmpdir | print %tmpdir/unbundled-module1.wasm

(component
  (core module $a
    (import "a" "a" (func))
  )
  (core module $b
    (import "b" "b" (func))
  )
)
