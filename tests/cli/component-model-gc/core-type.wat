;; RUN: wast --assert default --snapshot tests/snapshots % -f gc,cm-gc

(component
  (import "f" (func $f (param "x" u32) (param "y" u32) (result u32)))
  (core type $ty (func (param i32 i32) (result i32)))
  (core func (canon lower (func $f) (core-type $ty)))
)

(assert_invalid
  (component
    (core module $m
      (memory (export "memory") 1)
      (func (export "f") (result i32) unreachable)
    )
    (core instance $i (instantiate $m))

    (core type $ty (func (result i32)))
    (func (export "f") (result u32)
      (canon lift (core func $i "f") (core-type $ty))
    )
  )
  "canonical option `core-type` is not allowed in `canon lift`"
)

(assert_invalid
  (component
    (import "f" (func $f (param "x" u32) (param "y" u32) (result u32)))
    (core type $ty (func (param i64 i32) (result i32)))
    (core func (canon lower (func $f) (core-type $ty)))
  )
  "type mismatch when checking `core-type` canonical option"
)

(assert_invalid
  (component
    (import "f" (func $f (param "x" u32) (param "y" u32) (result u32)))
    (core type $ty (func (param i32 i32) (result i64)))
    (core func (canon lower (func $f) (core-type $ty)))
  )
  "type mismatch when checking `core-type` canonical option"
)

(component
  (import "f" (func $f (param "x" u32) (param "y" u32) (result u32)))

  (core rec
    (type (func))
    (type $ty (func (param i32 i32) (result i32)))
  )

  (core func $f (canon lower (func $f) (core-type $ty)))

  (core module $m
    (rec
      (type (func))
      (type $ty (func (param i32 i32) (result i32)))
    )
    (import "a" "b" (func (type $ty)))
  )

  (core instance (instantiate $m (with "a" (instance (export "b" (func $f))))))
)

;; Satisfying an import with the "same" type but from the wrong rec group should
;; fail.
(assert_invalid
  (component
    (import "f" (func $f (param "x" u32) (param "y" u32) (result u32)))

    (core type $ty (func (param i32 i32) (result i32)))
    (core func $f (canon lower (func $f) (core-type $ty)))

    (core module $m
      (rec
        (type (func))
        (type $ty (func (param i32 i32) (result i32)))
      )
      (import "a" "b" (func (type $ty)))
    )

    (core instance (instantiate $m (with "a" (instance (export "b" (func $f))))))
  )
  "type mismatch for export `b` of module instantiation argument `a`"
)

;; Satisfying an import with an exact subtype should succeed.
(component
  (import "f" (func $f (param "x" u32) (param "y" u32) (result u32)))

  (core type $super_ty (sub (func (param i32 i32) (result i32))))
  (core type $sub_ty (sub $super_ty (func (param i32 i32) (result i32))))

  (core func $f (canon lower (func $f) (core-type $sub_ty)))

  (core module $m
    (type $super_ty (sub (func (param i32 i32) (result i32))))
    (type $sub_ty (sub $super_ty (func (param i32 i32) (result i32))))
    (import "a" "b" (func (type $sub_ty)))
  )

  (core instance (instantiate $m (with "a" (instance (export "b" (func $f))))))
)

;; Satisfying an import with a subtype should succeed.
(component
  (import "f" (func $f (param "x" u32) (param "y" u32) (result u32)))

  (core type $super_ty (sub (func (param i32 i32) (result i32))))
  (core type $sub_ty (sub $super_ty (func (param i32 i32) (result i32))))

  (core func $f (canon lower (func $f) (core-type $sub_ty)))

  (core module $m
    (type $super_ty (sub (func (param i32 i32) (result i32))))
    (import "a" "b" (func (type $super_ty)))
  )

  (core instance (instantiate $m (with "a" (instance (export "b" (func $f))))))
)

;; Satisfying an import with the "same" type but from a different subtyping
;; hierarchy should fail.
(assert_invalid
  (component
    (import "f" (func $f (param "x" u32) (param "y" u32) (result u32)))

    (core type $ty (func (param i32 i32) (result i32)))
    (core func $f (canon lower (func $f) (core-type $ty)))

    (core module $m
      (type $super_ty (sub (func (param i32 i32) (result i32))))
      (type $sub_ty (sub $super_ty (func (param i32 i32) (result i32))))
      (import "a" "b" (func (type $sub_ty)))
    )

    (core instance (instantiate $m (with "a" (instance (export "b" (func $f))))))
  )
  "type mismatch for export `b` of module instantiation argument `a`"
)

;; Satisfying an import with the "same" type but is a supertype, rather than a
;; subtype, should fail.
(assert_invalid
  (component
    (import "f" (func $f (param "x" u32) (param "y" u32) (result u32)))

    (core type $super_ty (sub (func (param i32 i32) (result i32))))
    (core func $f (canon lower (func $f) (core-type $super_ty)))

    (core module $m
      (type $super_ty (sub (func (param i32 i32) (result i32))))
      (type $sub_ty (sub $super_ty (func (param i32 i32) (result i32))))
      (import "a" "b" (func (type $sub_ty)))
    )

    (core instance (instantiate $m (with "a" (instance (export "b" (func $f))))))
  )
  "type mismatch for export `b` of module instantiation argument `a`"
)

;; Satisfying an import with the "same" type but with the wrong finality should
;; fail.
(assert_invalid
  (component
    (import "f" (func $f (param "x" u32) (param "y" u32) (result u32)))

    (core type $ty (sub final (func (param i32 i32) (result i32))))
    (core func $f (canon lower (func $f) (core-type $ty)))

    (core module $m
      (type $ty (sub (func (param i32 i32) (result i32))))
      (import "a" "b" (func (type $ty)))
    )

    (core instance (instantiate $m (with "a" (instance (export "b" (func $f))))))
  )
  "type mismatch for export `b` of module instantiation argument `a`"
)
(assert_invalid
  (component
    (import "f" (func $f (param "x" u32) (param "y" u32) (result u32)))

    (core type $ty (sub (func (param i32 i32) (result i32))))
    (core func $f (canon lower (func $f) (core-type $ty)))

    (core module $m
      (type $ty (sub final (func (param i32 i32) (result i32))))
      (import "a" "b" (func (type $ty)))
    )

    (core instance (instantiate $m (with "a" (instance (export "b" (func $f))))))
  )
  "type mismatch for export `b` of module instantiation argument `a`"
)

;; Too few parameters in declared `core-type`.
(assert_invalid
  (component
    (import "f" (func $f (param "x" u32) (param "y" u32) (result u32)))
    (core type $ty (func (param i32) (result i32)))
    (core func (canon lower (func $f) (core-type $ty)))
  )
  "type mismatch when checking `core-type` canonical option"
)

;; Too few results in declared `core-type`.
(assert_invalid
  (component
    (import "f" (func $f (param "x" u32) (param "y" u32) (result u32)))
    (core type $ty (func (param i32 i32) (result)))
    (core func (canon lower (func $f) (core-type $ty)))
  )
  "type mismatch when checking `core-type` canonical option"
)

;; Too many parameters in declared `core-type`.
(assert_invalid
  (component
    (import "f" (func $f (param "x" u32) (param "y" u32) (result u32)))
    (core type $ty (func (param i32 i32 i32) (result i32)))
    (core func (canon lower (func $f) (core-type $ty)))
  )
  "type mismatch when checking `core-type` canonical option"
)

;; Too many results in declared `core-type`.
(assert_invalid
  (component
    (import "f" (func $f (param "x" u32) (param "y" u32) (result u32)))
    (core type $ty (func (param i32 i32) (result i32 i32)))
    (core func (canon lower (func $f) (core-type $ty)))
  )
  "type mismatch when checking `core-type` canonical option"
)

;; Too many parameters to be passed flat, in combination with a `core-type` option.
(component
  (import "f" (func $f
    (param "p0" u32)
    (param "p1" u32)
    (param "p2" u32)
    (param "p3" u32)
    (param "p4" u32)
    (param "p5" u32)
    (param "p6" u32)
    (param "p7" u32)
    (param "p8" u32)
    (param "p9" u32)
    (param "p10" u32)
    (param "p11" u32)
    (param "p12" u32)
    (param "p13" u32)
    (param "p14" u32)
    (param "p15" u32)
    (param "p16" u32)
  ))

  (core module $m
    (memory (export "memory") 1 1)
  )
  (core instance $i (instantiate $m))

  (core type $ty (func (param i32)))
  (core func (canon lower (func $f) (core-type $ty) (memory $i "memory")))
)

;; Too many results to be passed flat, in combination with a `core-type` option.
(component
  (import "f" (func $f (result (tuple u32 u32 u32 u32))))

  (core module $m
    (memory (export "memory") 1 1)
  )
  (core instance $i (instantiate $m))

  (core type $ty (func (param i32)))
  (core func (canon lower (func $f) (core-type $ty) (memory $i "memory")))
)
