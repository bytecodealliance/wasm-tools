;; Simple state example - functional with heterogeneous continuations
(module $state
  (tag $get (result i32))
  (tag $set (param i32))

  (type $gf (func (param i32) (result i32)))
  (type $sf (func (result i32)))

  (type $gk (cont $gf))
  (type $sk (cont $sf))

  (func $getting (param $k (ref $gk)) (param $s i32) (result i32)
     (block $on_get (result (ref $gk))
       (block $on_set (result i32 (ref $sk))
          (resume $gk (on $get $on_get) (on $set $on_set)
            (local.get $s) (local.get $k)
          )
          (return)
        ) ;;  $on_set (result i32 (ref $sk))
        (return_call $setting)
      ) ;;  $on_get (result (ref $gk))
      (local.get $s)
      (return_call $getting)
  )

  (func $setting (param $s i32) (param $k (ref $sk)) (result i32)
     (block $on_get (result (ref $gk))
       (block $on_set (result i32 (ref $sk))
          (resume $sk (on $get $on_get) (on $set $on_set)
            (local.get $k)
          )
          (return)
        ) ;;  $on_set (result i32 (ref $sk))
        (return_call $setting)
      ) ;;  $on_get (result (ref $gk))
      (local.get $s)
      (return_call $getting)
  )

  (func $f (result i32)
    (suspend $set (i32.const 7))
    (i32.add
      (suspend $get)
      (i32.mul
        (i32.const 2)
        (suspend $set (i32.const 3))
        (i32.add
          (i32.const 3)
          (suspend $get)
        )
      )
    )
  )

  (elem declare func $f)
  (func (export "run") (result i32)
    (call $setting (i32.const 0) (cont.new $sk (ref.func $f)))
  )
)

(assert_return (invoke "run") (i32.const 19))
