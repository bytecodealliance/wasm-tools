;; RUN: print --name-unnamed %

(module
  (func
    (block $a
      (block ;; unnamed
        (block $c
          i32.const 0
          br_table $a 1 $c
        )
      )
    )
    (block $c
      (block ;; unnamed
        (block $a
          i32.const 0
          br_table $a 1 $c
        )
      )
    )
    (block $a ;; 3
      (block $a ;; 2
        (block $a ;; 1
          i32.const 0
          br_table
            0 ;; -> a/1
            1 ;; -> a/2
            2 ;; -> a/3
            3 ;; -> function label
        )
      )
    )
  )

  (func $foo
    unreachable
    block (param i32 f32) (result i32 f32)
      unreachable
    end
  )
)
