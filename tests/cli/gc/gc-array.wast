;; RUN: wast --assert default --snapshot tests/snapshots %

;; --enable-gc

(module
  (type $a (array i32))
  (type $b (array (mut i32)))
  (type $c (array (mut (ref null func))))

  (data $data "\\0\\1\\2\\3")
  (elem $elem func $func)

  (func $func)
  (func (param (ref $a) (ref $b))
    i32.const 1
    i32.const 1
    array.new $a
    drop

    i32.const 1
    array.new_default $a
    drop

    local.get 0
    i32.const 1
    array.get $a
    drop

    local.get 1
    i32.const 2
    i32.const 3
    array.set $b

    i32.const 1
    i32.const 1
    array.new_fixed $a 2
    drop

    i32.const 0
    i32.const 4
    array.new_data $a $data
    drop

    i32.const 0
    i32.const 1
    array.new_elem $c $elem
    drop

    local.get 1
    i32.const 0
    local.get 1
    i32.const 0
    i32.const 1
    array.copy $b $b
  )
)
