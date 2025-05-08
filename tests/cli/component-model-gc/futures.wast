;; RUN: wast --assert default --snapshot tests/snapshots % -f gc,cm-gc,cm-async

(component
  (type $future (future bool))

  (core type $ty (func (param externref)))

  (import "i" (instance $i
                        (export "f" (func (param "x" $future)))))
  (core func (canon lower (func $i "f") gc string-encoding=utf8 (core-type $ty)))
)

(assert_invalid
  (component
    (type $future (future bool))

    (core type $ty (func (param anyref)))

    (import "i" (instance $i
                          (export "f" (func (param "x" $future)))))
    (core func (canon lower (func $i "f") gc string-encoding=utf8 (core-type $ty)))
  )
  "expected to lower component `future` type into core `(ref null? extern)` type, but found `anyref`"
)
