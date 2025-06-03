;; RUN: wast --assert default --snapshot tests/snapshots % -f gc,cm-gc

;; Basic.
(component
  (type $variant (variant (case "a" bool)
                          (case "b" bool)
                          (case "c" u8)))

  (core type $variant (struct))
  (core type $ty (func (param (ref $variant))))

  (import "i" (instance $i
                (export "ty" (type $variant' (eq $variant)))
                (export "f" (func (param "x" $variant')))))
  (core func (canon lower (func $i "f") gc (core-type $ty)))
)

;; With a nullable reference.
(component
  (type $variant (variant (case "a" bool)
                          (case "b" bool)
                          (case "c" u8)))

  (core type $variant (struct))
  (core type $ty (func (param (ref null $variant))))

  (import "i" (instance $i
                (export "ty" (type $variant' (eq $variant)))
                (export "f" (func (param "x" $variant')))))
  (core func (canon lower (func $i "f") gc (core-type $ty)))
)

;; With a custom rec group.
(component
  (type $variant (variant (case "a" bool)
                          (case "b" bool)
                          (case "c" u8)))

  (core rec
    (type $variant (struct))
    (type $ty (func (param (ref null $variant)))))

  (import "i" (instance $i
                (export "ty" (type $variant' (eq $variant)))
                (export "f" (func (param "x" $variant')))))
  (core func (canon lower (func $i "f") gc (core-type $ty)))
)

;; With a custom subtype.
(component
  (type $variant (variant (case "a" bool)
                          (case "b" bool)
                          (case "c" u8)))

  (core type $base (sub (struct)))
  (core type $variant (sub $base (struct)))
  (core type $ty (func (param (ref null $variant))))

  (import "i" (instance $i
                (export "ty" (type $variant' (eq $variant)))
                (export "f" (func (param "x" $variant')))))
  (core func (canon lower (func $i "f") gc (core-type $ty)))
)

;; Unexpected field in struct.
(assert_invalid
  (component
    (type $variant (variant (case "a" bool)
                            (case "b" bool)
                            (case "c" u8)))

    (core type $variant (struct (field i8)))
    (core type $ty (func (param (ref $variant))))

    (import "i" (instance $i
                          (export "ty" (type $variant' (eq $variant)))
                          (export "f" (func (param "x" $variant')))))
    (core func (canon lower (func $i "f") gc (core-type $ty)))
  )
  "expected to lower component `variant` type to core `(ref null? (struct))`"
)

;; Lowering into a non-struct.
(assert_invalid
  (component
    (type $variant (variant (case "a" bool)
                            (case "b" bool)
                            (case "c" u8)))

    (core type $ty (func (param externref)))

    (import "i" (instance $i
                          (export "ty" (type $variant' (eq $variant)))
                          (export "f" (func (param "x" $variant')))))
    (core func (canon lower (func $i "f") gc (core-type $ty)))
  )
  "expected to lower component `variant` type to core `(ref null? (struct))`"
)
