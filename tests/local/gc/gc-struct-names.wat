;; --enable-gc

(module
  (type $named (struct (field $x i32) (field $y i32)))
  (type $partially_named (struct (field $x i32) (field i32)))
  (type $unnamed (struct (field i32) (field i32)))

  (func (param (ref $named) (ref $partially_named) (ref $unnamed))
    i32.const 1
    i32.const 2
    struct.new $named
    local.tee 0
    struct.get $named 0
    local.get 0
    struct.get $named 1
    drop
    drop

    i32.const 1
    i32.const 2
    struct.new $partially_named
    local.tee 1
    struct.get $partially_named 0
    local.get 1
    struct.get $partially_named 1
    drop
    drop

    i32.const 1
    i32.const 2
    struct.new $unnamed
    local.tee 2
    struct.get $unnamed 0
    local.get 2
    struct.get $unnamed 1
    drop
    drop
  )
)
