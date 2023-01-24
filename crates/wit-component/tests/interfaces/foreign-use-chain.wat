(component
  (type (;0;)
    (component
      (type (;0;)
        (component
          (type (;0;)
            (instance
              (type (;0;) u8)
              (export (;1;) "the-type" (type (eq 0)))
            )
          )
          (import "the-interface" "path:/bar/bar/the-interface" (instance (type 0)))
          (alias export 0 "the-type" (type (;1;)))
          (type (;2;)
            (instance
              (alias outer 1 1 (type (;0;)))
              (export (;1;) "bar" (type (eq 0)))
            )
          )
          (import "bar" "path:/bar/bar/the-name" (instance (type 2)))
        )
      )
      (export (;0;) "foo" "pkg:/foo/foo" (component (type 0)))
    )
  )
  (export (;1;) "foo" "pkg:/foo" (type 0))
)