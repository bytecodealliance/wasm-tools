;; RUN: wast --assert default --snapshot tests/snapshots % -f gc,cm-gc

(component
  (type $enum (enum "a" "b" "c"))

  (core type $ty (func (param i32)))

  (import "i" (instance $i
                (export $enum' "ty" (type (eq $enum)))
                (export "f" (func (param "x" $enum')))))
  (core func (canon lower (func $i "f") gc string-encoding=utf8 (core-type $ty)))
)

(assert_invalid
  (component
    (type $enum (enum "a" "b" "c"))

    (core type $ty (func (param anyref)))

    (import "i" (instance $i
                  (export $enum' "ty" (type (eq $enum)))
                  (export "f" (func (param "x" $enum')))))
    (core func (canon lower (func $i "f") gc string-encoding=utf8 (core-type $ty)))
  )
  "expected to lower component `enum` type into core `i32` type, but found `anyref`"
)
