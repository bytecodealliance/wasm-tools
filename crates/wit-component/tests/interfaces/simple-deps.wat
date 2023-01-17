(component
  (type (;0;)
    (component
      (type (;0;)
        (instance
          (type (;0;) u8)
          (export (;1;) "some-type" (type (eq 0)))
        )
      )
      (import "types" "path:/some-dep/types/types" (instance (type 0)))
      (alias export 0 "some-type" (type (;1;)))
      (type (;2;)
        (instance
          (alias outer 1 1 (type (;0;)))
          (export (;1;) "some-type" (type (eq 0)))
        )
      )
      (export (;0;) "foo" "pkg:/foo/foo" (instance (type 2)))
    )
  )
  (export (;1;) "foo" "pkg:/foo" (type 0))
)