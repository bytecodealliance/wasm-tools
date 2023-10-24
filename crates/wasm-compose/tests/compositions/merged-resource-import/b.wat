(component
  (import "a"
    (instance
      (export "a" (type (sub resource)))
      (export $b "b" (type (sub resource)))
      (export "[constructor]b" (func (param "y" u32) (result (own $b))))
    )
  )
)
