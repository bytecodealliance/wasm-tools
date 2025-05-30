;; RUN: wast --assert default --snapshot tests/snapshots % -f gc,cm-gc

(component
  (type $tup (tuple string bool))

  (core type $string (array i8))
  (core type $tup (struct (field (ref $string))
                          (field i8)))
  (core type $ty (func (param (ref $tup))))

  (import "i" (instance $i
                (export $tup' "ty" (type (eq $tup)))
                (export "f" (func (param "x" $tup')))))
  (core func (canon lower (func $i "f") gc string-encoding=utf8 (core-type $ty)))
)

(assert_invalid
  (component
    (type $tup (tuple string bool))

    (core type $string (array i8))
    (core type $tup (struct (field (ref $string))))
    (core type $ty (func (param (ref $tup))))

    (import "i" (instance $i
                          (export $tup' "ty" (type (eq $tup)))
                          (export "f" (func (param "x" $tup')))))
    (core func (canon lower (func $i "f") gc string-encoding=utf8 (core-type $ty)))
  )
  "core `struct` has 1 fields, but component `tuple` has 2 fields"
)

(assert_invalid
  (component
    (type $tup (tuple string bool))

    (core type $string (array i8))
    (core type $tup (struct (field (ref $string))
                            (field i8)
                            (field i8)))
    (core type $ty (func (param (ref $tup))))

    (import "i" (instance $i
                          (export $tup' "ty" (type (eq $tup)))
                          (export "f" (func (param "x" $tup')))))
    (core func (canon lower (func $i "f") gc string-encoding=utf8 (core-type $ty)))
  )
  "core `struct` has 3 fields, but component `tuple` has 2 fields"
)

(assert_invalid
  (component
    (type $tup (tuple string bool))

    (core type $ty (func (param i32)))

    (import "i" (instance $i
                          (export $tup' "ty" (type (eq $tup)))
                          (export "f" (func (param "x" $tup')))))
    (core func (canon lower (func $i "f") gc string-encoding=utf8 (core-type $ty)))
  )
  "expected to lower component `tuple` type to core `(ref null? (struct ...))`, but found `i32`"
)
