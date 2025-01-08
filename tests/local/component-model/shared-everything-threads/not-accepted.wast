(assert_invalid
  (component
    (core module $A
      (memory (export "m") 1 2 shared))
    (core instance $A (instantiate $A))
    (alias core export $A "m" (core memory $m))
  )
  "shared linear memories are not compatible with components yet")

(assert_invalid
  (component
    (core module $A
      (table (export "m") shared 1 2 (ref null (shared func)))
    )
    (core instance $A (instantiate $A))
    (alias core export $A "m" (core table $m))
  )
  "shared tables are not compatible with components yet")
