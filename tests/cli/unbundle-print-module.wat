;; RUN[gen]: component unbundle --threshold 0 % --module-dir %tmpdir
;; RUN[read0]: print %tmpdir/unbundled-module0.wasm
;; RUN[read1]: print %tmpdir/unbundled-module1.wasm

(component
  (core module $a
    (import "a" "a" (func))
  )
  (core module $b
    (import "b" "b" (func))
  )
)
