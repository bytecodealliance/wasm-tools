;; RUN: wast --assert default --snapshot tests/snapshots % -f gc,cm-gc

;; Basic.
(component
  (type $opt (option u32))

  (core type $opt (struct))
  (core type $ty (func (param (ref $opt))))

  (import "i" (instance $i
                (export "ty" (type $opt' (eq $opt)))
                (export "f" (func (param "x" $opt')))))
  (core func (canon lower (func $i "f") gc (core-type $ty)))
)

;; With a nullable reference.
(component
  (type $opt (option u32))

  (core type $opt (struct))
  (core type $ty (func (param (ref null $opt))))

  (import "i" (instance $i
                (export "ty" (type $opt' (eq $opt)))
                (export "f" (func (param "x" $opt')))))
  (core func (canon lower (func $i "f") gc (core-type $ty)))
)

;; With a custom rec group.
(component
  (type $opt (option u32))

  (core rec
    (type $opt (struct))
    (type $ty (func (param (ref $opt)))))

  (import "i" (instance $i
                (export "ty" (type $opt' (eq $opt)))
                (export "f" (func (param "x" $opt')))))
  (core func (canon lower (func $i "f") gc (core-type $ty)))
)

;; With a custom subtype.
(component
  (type $opt (option u32))

  (core type $base (sub (struct)))
  (core type $opt (sub $base (struct)))
  (core type $ty (func (param (ref $opt))))

  (import "i" (instance $i
                (export "ty" (type $opt' (eq $opt)))
                (export "f" (func (param "x" $opt')))))
  (core func (canon lower (func $i "f") gc (core-type $ty)))
)

;; Unexpected field.
(assert_invalid
  (component
    (type $opt (option u32))

    (core type $opt (struct (field i32)))
    (core type $ty (func (param (ref $opt))))

    (import "i" (instance $i
                          (export "ty" (type $opt' (eq $opt)))
                          (export "f" (func (param "x" $opt')))))
    (core func (canon lower (func $i "f") gc (core-type $ty)))
  )
  "expected to lower component `option` type to core `(ref null? (struct))`"
)

;; Lowering into a non-struct.
(assert_invalid
  (component
    (type $opt (option u32))

    (core type $ty (func (param externref)))

    (import "i" (instance $i
                          (export "ty" (type $opt' (eq $opt)))
                          (export "f" (func (param "x" $opt')))))
    (core func (canon lower (func $i "f") gc (core-type $ty)))
  )
  "expected to lower component `option` type to core `(ref null? (struct))`"
)
