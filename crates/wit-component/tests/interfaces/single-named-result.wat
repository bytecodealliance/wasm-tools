(component
  (type (;0;)
    (component
      (type (;0;)
        (instance
          (type (;0;) (func (result "a" u32)))
          (export (;0;) "a" (func (type 0)))
        )
      )
      (export (;0;) "foo" "pkg:/single-named-result/foo" (instance (type 0)))
    )
  )
  (export (;1;) "single-named-result" "pkg:/single-named-result" (type 0))
)