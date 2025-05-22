(component
  (import "example:service/logging@0.1.0"
    (instance
      (export "logger" (type $logger (sub resource)))
      (export "[method]logger.log" (func (param "self" (borrow $logger)) (param "message" string)))
      (export "get-logger" (func (result (own $logger))))
    )
  )
)
