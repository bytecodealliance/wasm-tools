;; RUN: wast --assert default --snapshot tests/snapshots % -f gc,cm-gc,cm-error-context

(component
  (core type $ty (func (param externref)))

  (import "i" (instance $i
                        (export "f" (func (param "x" error-context)))))
  (core func (canon lower (func $i "f") gc (core-type $ty)))
)

(assert_invalid
  (component
    (core type $ty (func (param anyref)))

    (import "i" (instance $i
                          (export "f" (func (param "x" error-context)))))
    (core func (canon lower (func $i "f") gc string-encoding=utf8 (core-type $ty)))
  )
  "expected to lower component `error-context` type into core `(ref null? extern)` type, but found `anyref`"
)
