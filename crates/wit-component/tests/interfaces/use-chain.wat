(component
  (type (;0;)
    (component
      (type (;0;)
        (instance
          (type (;0;) u32)
          (export (;1;) "a" (type (eq 0)))
          (export (;2;) "b" (type (eq 1)))
          (export (;3;) "c" (type (eq 2)))
        )
      )
      (export (;0;) "foo" "pkg:/use-chain/foo" (instance (type 0)))
    )
  )
  (export (;1;) "use-chain" "pkg:/use-chain" (type 0))
)