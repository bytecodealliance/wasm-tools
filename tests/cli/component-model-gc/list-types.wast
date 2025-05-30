;; RUN: wast --assert default --snapshot tests/snapshots % -f gc,cm-gc

(component
  (type $list (list string))

  (core type $string (array i8))
  (core type $list (array (ref $string)))
  (core type $ty (func (param (ref $list))))

  (import "i" (instance $i
                (export "ty" (type $list' (eq $list)))
                (export "f" (func (param "x" $list')))))
  (core func (canon lower (func $i "f") gc string-encoding=utf8 (core-type $ty)))
)

(assert_invalid
  (component
    (type $list (list string))

    (core type $ty (func (param i32)))

    (import "i" (instance $i
                          (export "ty" (type $list' (eq $list)))
                          (export "f" (func (param "x" $list')))))
    (core func (canon lower (func $i "f") gc string-encoding=utf8 (core-type $ty)))
  )
  "expected to lower component `list` type into `(ref null? (array ...))`, but found `i32`"
)
