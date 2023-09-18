;; --enable-gc

(module
  (type $a (array i32))
  (type $b (array (mut i32)))
  (type $c (array (mut (ref null $b))))

  (data $data "\\0\\1\\2\\3")
  (elem $elem func $func)

  (func $func)
  (func
    array.new $a
    array.new_default $a
    array.get $a
    array.set $b
    array.new_fixed $a 10
    array.new_data $a $data
    array.new_elem $a $elem
    array.copy $a $a
  )
)
