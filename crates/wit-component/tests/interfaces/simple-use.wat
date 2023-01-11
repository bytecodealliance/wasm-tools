(component
  (type (;0;)
    (component
      (type (;0;)
        (instance
          (type (;0;) (enum "info" "debug"))
          (export (;1;) "level" (type (eq 0)))
        )
      )
      (export (;0;) "types" "pkg:/simple-use/types" (instance (type 0)))
      (alias export 0 "level" (type (;1;)))
      (type (;2;)
        (instance
          (alias outer 1 1 (type (;0;)))
          (export (;1;) "level" (type (eq 0)))
          (type (;2;) (func (param "level" 1) (param "msg" string)))
          (export (;0;) "log" (func (type 2)))
        )
      )
      (export (;1;) "console" "pkg:/simple-use/console" (instance (type 2)))
    )
  )
  (export (;1;) "simple-use" "pkg:/simple-use" (type 0))
)