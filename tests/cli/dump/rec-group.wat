;; RUN: dump %

(module
  (rec
    (type (func (param i32 i32) (result f64)))
  )
  (rec
    (type (sub (struct (field i32))))
    (type (sub (struct (field (mut i32)))))
    (type (sub (struct (field (mut i32)
                              (mut i64)
                              (mut f32)
                              (mut f64)
                              (mut v128)
                              (mut funcref)
                              (mut externref)
                              (mut (ref null 2))))))
  )
  (type $a (sub (array i32)))
  (type $b (sub $a (array i32)))
)
