(component
  (import "example:service/logging@0.1.0"
    (instance $import-logging-instance
      (export "logger" (type $logger (sub resource)))
      (export "[method]logger.log" (func (param "self" (borrow $logger)) (param "message" string)))
      (export "get-logger" (func (result (own $logger))))
    )
  )

  (core module $module
    (import "example:service/logging@0.1.0" "[method]logger.log" (func (param i32 i32 i32)))
    (import "example:service/logging@0.1.0" "get-logger" (func (result i32)))
    (import "[export]example:service/logging@0.1.0" "[resource-new]logger" (func (param i32) (result i32)))
    (func $cabi_realloc (param i32 i32 i32 i32) (result i32)
      unreachable
    )
    (func $logger-log (param i32 i32 i32)
      unreachable
    )
    (func $get-logger (result i32)
      unreachable
    )
    (memory $memory 1)
    (export "memory" (memory $memory))
    (export "cabi_realloc" (func $cabi_realloc))
    (export "example:service/logging@0.1.0#[method]logger.log" (func $logger-log))
    (export "example:service/logging@0.1.0#get-logger" (func $get-logger))
  )

  (core module $module-indirect
    (func $logger-log (param i32 i32 i32)
      unreachable
    )
    (func $get-logger (result i32)
      unreachable
    )
    (func $dtor-logger (param i32)
      unreachable
    )
    (export "logger.log" (func $logger-log))
    (export "[dtor]logger" (func $dtor-logger))
  )
  (core instance $module-indirect-instance (instantiate $module-indirect))

  (alias export $import-logging-instance "logger" (type $import-logger-type))
  (core func $import-drop-logger (canon resource.drop $import-logger-type))
  (alias core export $module-indirect-instance "logger.log" (core func $import-logger-log))
  (alias export $import-logging-instance "get-logger" (func $import-get-logger))
  (core func $import-get-logger-lowered (canon lower (func $import-get-logger)))
  (core instance $logger-impl-instance
    (export "[resource-drop]logger" (func $import-drop-logger))
    (export "[method]logger.log" (func $import-logger-log))
    (export "get-logger" (func $import-get-logger-lowered))
  )

  (alias core export $module-indirect-instance "[dtor]logger" (core func $logger-dtor))
  (type $logger-resource (resource (rep i32) (dtor (func $logger-dtor))))
  (core func $logger-new (canon resource.new $logger-resource))
  (core instance $logger-new-instance
    (export "[resource-new]logger" (func $logger-new))
  )

  (core instance $module-instance
    (instantiate $module
      (with "example:service/logging@0.1.0" (instance $logger-impl-instance))
      (with "[export]example:service/logging@0.1.0" (instance $logger-new-instance))
    )
  )
  (alias core export $module-instance "memory" (core memory $memory))
  (alias core export $module-instance "cabi_realloc" (core func $realloc))

  (type $logger-log-type (func (param "self" (borrow $logger-resource)) (param "message" string)))
  (alias core export $module-instance "example:service/logging@0.1.0#[method]logger.log" (core func $logger-log))
  (func $logger-log-lifted
    (type $logger-log-type)
    (canon lift (core func $logger-log) (memory $memory) (realloc $realloc) string-encoding=utf8)
  )
  (alias core export $module-instance "example:service/logging@0.1.0#get-logger" (core func $get-logger))
  (type $get-logger-type (func (result (own $logger-resource))))
  (func $get-logger-lifted (type $get-logger-type) (canon lift (core func $get-logger)))

  (component $logger
    (import "import-type-logger" (type $import-logger (sub resource)))
    (import "import-method-logger-log"
      (func $import-logger-log (param "self" (borrow $import-logger)) (param "message" string))
    )
    (import "import-func-get-logger" (func $import-get-logger (result (own $import-logger))))
    (export $logger "logger" (type $import-logger))
    (export "[method]logger.log" (func 0) (func (param "self" (borrow $logger)) (param "message" string)))
    (type (;8;) (own 5))
    (type (;9;) (func (result 8)))
    (export (;3;) "get-logger" (func $import-get-logger) (func (result (own $logger))))
  )

  (instance $logger-instance (instantiate $logger
      (with "import-method-logger-log" (func $logger-log-lifted))
      (with "import-func-get-logger" (func $get-logger-lifted))
      (with "import-type-logger" (type $logger-resource))
    )
  )

  (export "example:service/logging@0.1.0" (instance $logger-instance))
)
