(component
  (import "a"
    (instance
      (export "b" (type $b (sub resource)))
      (export "[constructor]b" (func (param "y" u32) (result (own $b))))
    )
  )
  (import "b" (instance))
)
