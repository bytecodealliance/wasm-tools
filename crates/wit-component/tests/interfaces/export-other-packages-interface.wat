(component
  (type (;0;)
    (component
      (type (;0;)
        (component
          (type (;0;)
            (instance
              (type (;0;) u8)
              (export (;1;) "t" (type (eq 0)))
            )
          )
          (export (;0;) "foo" "path:/the-dep/the-doc/the-interface" (instance (type 0)))
        )
      )
      (export (;0;) "foo" "pkg:/foo/foo" (component (type 0)))
    )
  )
  (export (;1;) "foo" "pkg:/foo" (type 0))
)