(component
  (import "a"
    (instance
      (export $b "b" (type (sub resource)))
      (export "[constructor]b" (func (param "y" u32) (result (own $b))))
    )
  )
  (import "b" (instance))
)
