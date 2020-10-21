;;; --enable-multi-value
(module
  (func
    (unreachable)
    (block (param i32)
        (drop)
    )
  )
)

(assert_invalid
    (module
        (func
            (unreachable)
            (block (param i32))
        )
    )
    "type mismatch"
)

(assert_invalid
    (module
        (func
            (block (param i32)
                (drop)
            )
        )
    )
    "type mismatch"
)
