;; RUN: wast --assert default --snapshot tests/snapshots % -f gc,cm-gc

(component
  (type $flags (flags "a" "b" "c"))

  (core type $ty (func (param i32)))

  (import "i" (instance $i
                (export "ty" (type $flags' (eq $flags)))
                (export "f" (func (param "x" $flags')))))
  (core func (canon lower (func $i "f") gc string-encoding=utf8 (core-type $ty)))
)

(assert_invalid
  (component
    (type $flags (flags "a" "b" "c"))

    (core type $ty (func (param anyref)))

    (import "i" (instance $i
                  (export "ty" (type $flags' (eq $flags)))
                  (export "f" (func (param "x" $flags')))))
    (core func (canon lower (func $i "f") gc string-encoding=utf8 (core-type $ty)))
  )
  "expected to lower component `flags` type into core `i32` type, but found `anyref`"
)
