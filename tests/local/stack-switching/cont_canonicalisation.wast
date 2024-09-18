(module

  (type $int_to_int (func (param i32) (result i32)))
  (type $ft (func (param i32) (result i32)))
  (type $res_int_to_int (cont $int_to_int))
  (type $f3_t (func (param i32) (result i32)))
  (type $f3_ct (cont $f3_t))

  (tag $e3_int_to_int (param i32) (result i32))

  (func $test_case_4 (export "test_case_4") (result i32)

    (block $on_e3 (result i32 (ref $res_int_to_int))
      (resume $f3_ct (on $e3_int_to_int $on_e3) (i32.const 49) (ref.null $f3_ct))
      (unreachable))
    ;; after on_e3, expected stack: [50 resumption]
    (unreachable)
    )
)