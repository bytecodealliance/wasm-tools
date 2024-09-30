(module
  (func $main (result i32)
    (local i32 i32 i32 i32)
    i32.const 0
    local.tee 2
    local.set 3
    loop  ;; label = @1
      local.get 2
      i32.const 50000
      i32.eq
      (@metadata.code.branch_hint "\00") if  ;; label = @2
        i32.const 1
        local.set 3
      end
      local.get 2
      i32.const 1
      i32.add
      local.tee 2
      i32.const 100000
      i32.ne
      (@metadata.code.branch_hint "\01") br_if 0 (;@1;)
    end
    local.get 3)
)

(module
  (import "" "" (func))

  (func
    i32.const 0
    (@metadata.code.branch_hint "\00")
    if
    end
  )
)

(module
  (import "" "" (func))

  (func
    i32.const 0
    (@metadata.code.branch_hint "\00")
    if
      i32.const 0
      (@metadata.code.branch_hint "\01")
      br_if 0
    end
  )

  (func
    i32.const 0
    (@metadata.code.branch_hint "\01")
    if
      i32.const 0
      (@metadata.code.branch_hint "\00")
      br_if 0
    end
  )
)

(assert_invalid
  (module quote "(@metadata.code.branch_hint)")
  "expected valid module field")
(assert_invalid
  (module quote "(func (@metadata.code.branch_hint))")
  "expected a string")
(assert_invalid
  (module quote "(func (@metadata.code.branch_hint \"a\"))")
  "invalid value for branch hint")

;; make sure this at least doesn't crash. Should perhaps be an error?
(module
  (func (@metadata.code.branch_hint "\00"))
  (func
    (@metadata.code.branch_hint "\00")
    (@metadata.code.branch_hint "\01")
  )
  (func
    (@metadata.code.branch_hint "\00")
    (@metadata.code.branch_hint "\00")
    (@metadata.code.branch_hint "\00")
    i32.const 0
    (@metadata.code.branch_hint "\00")
    (@metadata.code.branch_hint "\00")
    (@metadata.code.branch_hint "\00")
    if
    (@metadata.code.branch_hint "\00")
    (@metadata.code.branch_hint "\00")
    (@metadata.code.branch_hint "\00")
    end
    (@metadata.code.branch_hint "\00")
    (@metadata.code.branch_hint "\00")
    (@metadata.code.branch_hint "\00")
  )
)
