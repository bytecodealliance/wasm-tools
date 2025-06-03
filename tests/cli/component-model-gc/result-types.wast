;; RUN: wast --assert default --snapshot tests/snapshots % -f gc,cm-gc

;; Basic.
(component
  (type $result (result u32 (error u8)))

  (core type $result (struct (field i8)))
  (core type $ty (func (param (ref $result))))

  (import "i" (instance $i
                (export "ty" (type $result' (eq $result)))
                (export "f" (func (param "x" $result')))))
  (core func (canon lower (func $i "f") gc (core-type $ty)))
)

;; With a nullable reference.
(component
  (type $result (result u32 (error u8)))

  (core type $result (struct (field i8)))
  (core type $ty (func (param (ref null $result))))

  (import "i" (instance $i
                (export "ty" (type $result' (eq $result)))
                (export "f" (func (param "x" $result')))))
  (core func (canon lower (func $i "f") gc (core-type $ty)))
)

;; With a custom rec group.
(component
  (type $result (result u32 (error u8)))

  (core rec
    (type $result (struct (field i8)))
    (type $ty (func (param (ref null $result)))))

  (import "i" (instance $i
                (export "ty" (type $result' (eq $result)))
                (export "f" (func (param "x" $result')))))
  (core func (canon lower (func $i "f") gc (core-type $ty)))
)

;; With a custom subtype.
(component
  (type $result (result u32 (error u8)))

  (core type $base (sub (struct)))
  (core type $result (sub $base (struct (field i8))))
  (core type $ty (func (param (ref null $result))))

  (import "i" (instance $i
                (export "ty" (type $result' (eq $result)))
                (export "f" (func (param "x" $result')))))
  (core func (canon lower (func $i "f") gc (core-type $ty)))
)

;; Missing discriminant.
(assert_invalid
  (component
    (type $result (result u32 (error u8)))

    (core type $result (struct))
    (core type $ty (func (param (ref $result))))

    (import "i" (instance $i
                          (export "ty" (type $result' (eq $result)))
                          (export "f" (func (param "x" $result')))))
    (core func (canon lower (func $i "f") gc (core-type $ty)))
  )
  "expected to lower component `result` type to core `(ref null? (struct (field i8)))`"
)

;; Wrong type for discriminant.
(assert_invalid
  (component
    (type $result (result u32 (error u8)))

    (core type $result (struct (field i32)))
    (core type $ty (func (param (ref $result))))

    (import "i" (instance $i
                          (export "ty" (type $result' (eq $result)))
                          (export "f" (func (param "x" $result')))))
    (core func (canon lower (func $i "f") gc (core-type $ty)))
  )
  "expected to lower component `result` type to core `(ref null? (struct (field i8)))`"
)

;; Additional fields after discriminant.
(assert_invalid
  (component
    (type $result (result u32 (error u8)))

    (core type $result (struct (field i8) (field i8)))
    (core type $ty (func (param (ref $result))))

    (import "i" (instance $i
                          (export "ty" (type $result' (eq $result)))
                          (export "f" (func (param "x" $result')))))
    (core func (canon lower (func $i "f") gc (core-type $ty)))
  )
  "expected to lower component `result` type to core `(ref null? (struct (field i8)))`"
)

;; Lowering into a non-struct.
(assert_invalid
  (component
    (type $result (result u32 (error u8)))

    (core type $ty (func (param externref)))

    (import "i" (instance $i
                          (export "ty" (type $result' (eq $result)))
                          (export "f" (func (param "x" $result')))))
    (core func (canon lower (func $i "f") gc (core-type $ty)))
  )
  "expected to lower component `result` type to core `(ref null? (struct (field i8)))`"
)
