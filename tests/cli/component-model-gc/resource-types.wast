;; RUN: wast --assert default --snapshot tests/snapshots % -f gc,cm-gc

(component
  (core type $ty (func (param externref)))

  (import "i" (instance $i
                        (export $resource "r" (type (sub resource)))
                        (export "f" (func (param "x" (own $resource))))))
  (core func (canon lower (func $i "f") gc string-encoding=utf8 (core-type $ty)))
)

(assert_invalid
  (component
     (core type $ty (func (param anyref)))

     (import "i" (instance $i
                           (export $resource "r" (type (sub resource)))
                           (export "f" (func (param "x" (own $resource))))))
     (core func (canon lower (func $i "f") gc string-encoding=utf8 (core-type $ty)))
  )
  "expected to lower component `own` type into core `(ref null? extern)` type, but found `anyref`"
)

(component
  (core type $ty (func (param externref)))

  (import "i" (instance $i
                        (export $resource "r" (type (sub resource)))
                        (export "f" (func (param "x" (borrow $resource))))))
  (core func (canon lower (func $i "f") gc string-encoding=utf8 (core-type $ty)))
)

(assert_invalid
  (component
     (core type $ty (func (param anyref)))

     (import "i" (instance $i
                           (export $resource "r" (type (sub resource)))
                           (export "f" (func (param "x" (borrow $resource))))))
     (core func (canon lower (func $i "f") gc string-encoding=utf8 (core-type $ty)))
  )
  "expected to lower component `borrow` type into core `(ref null? extern)` type, but found `anyref`"
)
