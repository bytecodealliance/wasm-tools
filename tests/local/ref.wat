(module
  (type (;0;) (func))
  (type (;1;) (func (param funcref)))
  (func (;0;) (type 0)
    (local externref funcref)
    ref.null extern
    local.set 0
    ref.null func
    local.set 1)
  (func (;1;) (type 1) (param funcref)
    global.get 0
    ref.is_null
    drop
    local.get 0
    ref.is_null
    drop)
  (global (;0;) externref (ref.null extern))

  (func $select-join
    ref.null func
    ref.null func
    i32.const 0
    select (result funcref)
    drop

    ref.null extern
    ref.null extern
    i32.const 0
    select (result externref)
    drop

    block
      unreachable
      select (result externref)
      drop
    end

    block
      unreachable
      i32.const 0
      select (result externref)
      drop
    end

    block
      unreachable
      ref.null extern
      i32.const 0
      select (result externref)
      drop
    end

    block
      unreachable
      ref.null extern
      ref.null extern
      i32.const 0
      select (result externref)
      drop
    end
  )
)
