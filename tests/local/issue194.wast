;;; --enable-multi-value
(module
(func (param i32)
    (local i32)
    i32.const -64
    loop (param i32)
        local.set 0
        i32.const 0
        local.get 0
        br_if 0
        drop
    end))

(assert_invalid
    (module
    (func (param i32)
        (local i32)
        i32.const -64
        loop (param i32)
            local.set 0
            local.get 0
            br_if 0
        end))
    "type mismatch"
)

(assert_invalid
    (module
    (func (param i32)
        (local i32)
        i32.const -64
        loop (param i32)
            local.set 0
            i64.const 0
            local.get 0
            br_if 0
            drop
        end))
    "type mismatch"
)
