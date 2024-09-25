;; Unrelated source & target continuations types
(assert_invalid
  (module
    (type $sft (func (param i32) (result i32)))
    (type $sct (cont $sft))
    (type $tft (func (param f32) (result f32)))
    (type $tct (cont $tft))

    (func $f (param $n i32) (result i32)
       (local.get $n))
    (elem declare func $f)

    (func $unrelated-types (export "unrelated-types")
      (cont.bind $sct $tct (cont.new $sct (ref.func $f)))
      (drop))
  )
  "type mismatch")