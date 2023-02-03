(component
  (type (;0;)
    (component
      (type (;0;)
        (component
          (type (;0;)
            (instance
              (type (;0;) (record))
              (export (;1;) "foo" (type (eq 0)))
              (export (;2;) "bar" (type (eq 1)))
            )
          )
          (export (;0;) "foo" "path:/my-dep/my-doc/my-interface" (instance (type 0)))
        )
      )
      (export (;0;) "foo" "pkg:/foo/foo" (component (type 0)))
    )
  )
  (export (;1;) "foo" "pkg:/foo" (type 0))
)