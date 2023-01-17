(component
  (type (;0;)
    (component
      (type (;0;)
        (instance
          (type (;0;) (func (param "arg" string)))
          (export (;0;) "log" (func (type 0)))
        )
      )
      (export (;0;) "console" "pkg:/simple-world/console" (instance (type 0)))
      (type (;1;)
        (component
          (type (;0;)
            (instance
              (type (;0;) (func (param "arg" string)))
              (export (;0;) "log" (func (type 0)))
            )
          )
          (import "console" "pkg:/simple-world/console" (instance (type 0)))
        )
      )
      (export (;0;) "the-world" "pkg:/simple-world/the-world" (component (type 1)))
    )
  )
  (export (;1;) "simple-world" "pkg:/simple-world" (type 0))
)