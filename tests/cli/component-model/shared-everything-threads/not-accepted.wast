;; RUN: wast --assert default --snapshot tests/snapshots % -f shared-everything-threads

(component
  (core module $A
    (memory (export "m") 1 2 shared))
  (core instance $A (instantiate $A))
  (alias core export $A "m" (core memory $m))

  (core module $B (import "" "" (memory 1 2 shared)))
  (core instance (instantiate $B (with "" (instance (export "" (memory $m))))))
)

(component
  (core module $A
    (table (export "m") shared 1 2 (ref null (shared func)))
  )
  (core instance $A (instantiate $A))
  (alias core export $A "m" (core table $m))

  (core module $B (import "" "" (table shared 1 2 (ref null (shared func)))))
  (core instance (instantiate $B (with "" (instance (export "" (table $m))))))
)

(assert_invalid
  (component
    (import "x" (func $x (param "x" string)))

    (core module $A
      (memory (export "m") 1 2 shared))
    (core instance $A (instantiate $A))
    (alias core export $A "m" (core memory $m))
    (core func (canon lower (func $x) (memory $m)))
  )
  "canonical ABI memory is not a 32-bit linear memory")
