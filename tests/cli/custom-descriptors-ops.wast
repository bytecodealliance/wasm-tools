;; RUN: wast --assert default --snapshot tests/snapshots % -f custom-descriptors

;; --enable-gc

(module
  (type $f (func (result i32)))
  (rec
    (type $t1 (sub (descriptor $t2) (struct (field i32))))
    (type $t2 (sub (describes $t1) (struct (field (ref $f)))))
    (type $t1sub (sub $t1 (descriptor $t2sub) (struct (field i32 f32))))
    (type $t2sub (sub $t2 (describes $t1sub) (struct (field (ref $f) (ref $f)))))
  )

  (import "" "rtt1" (global $g1 (ref (exact $t2))))
  (import "" "rtt1sub" (global $g1sub (ref (exact $t2sub))))

  (elem declare func $f_impl)
  (func $f_impl (type $f) (result i32)
    unreachable
  )

  (func $test-struct-new-desc
    i32.const 3
    global.get $g1
    struct.new_desc $t1
    drop

    global.get $g1
    struct.new_default_desc $t1
    drop

    global.get $g1sub
    struct.new_default_desc $t1sub
    drop
  )

  (func $test-exact-types
    (local $l_any anyref)
    (local $l_struct structref)
    (local $l_t2 (ref $t2))
    (local $l_t2_e (ref (exact $t2)))
    global.get $g1
    local.set $l_any
    global.get $g1
    local.set $l_struct
    global.get $g1
    local.set $l_t2
    global.get $g1
    local.set $l_t2_e

    global.get $g1sub
    local.set $l_struct
    global.get $g1sub
    local.set $l_t2

    local.get $l_any
    ref.cast (ref null $t2)
    ref.cast (ref (exact $t2))
    local.set $l_t2_e
  )

  (func $test-struct-new-result
    (local (ref (exact $t2)))
    ref.func $f_impl
    struct.new $t2
    local.set 0
  )

  (func $test-get-desc (param $rtt (ref (exact $t2)))
    local.get $rtt
    struct.new_default_desc $t1
    ref.get_desc $t1
    local.set $rtt
  )

  (func $test-get-desc-2 (result (ref (exact $t2)))
    unreachable
    ref.get_desc $t1
  )

  (func $test-cast-desc (param $i (ref $t1)) (param $desc (ref null (exact $t2))) (result (ref null (exact $t1)))
    local.get $i
    local.get $desc
    ref.cast_desc (ref null (exact $t1))
  )

  (func $test-br_on_cast_desc (param $i (ref null $t1sub)) (param $desc (ref null $t2)) (result (ref null $t1))
    local.get $i
    local.get $desc
    br_on_cast_desc 0 (ref null $t1sub) (ref null $t1)
  )

  (func $test-br_on_cast_desc_fail (param $i (ref null $t1sub)) (param $desc (ref null $t2)) (result (ref null $t1))
    local.get $i
    local.get $desc
    br_on_cast_desc_fail 0 (ref null $t1sub) (ref null $t1)
  )
)

(assert_invalid
  (module
    (rec
      (type $t1 (sub (descriptor $t2) (struct (field i32))))
      (type $t2 (sub (describes $t1) (struct (field (ref $f)))))
      (type $t1sub (sub $t1 (descriptor $t2sub) (struct (field i32 f32))))
      (type $t2sub (sub $t2 (describes $t1sub) (struct (field (ref $f) (ref $f)))))
      (type $f (func (result i32)))
    )

    (import "" "rtt1" (global $g1 (ref (exact $t2))))
    (import "" "rtt1sub" (global $g1sub (ref (exact $t2sub))))

    (func $test-struct-new-non-exact-1
      global.get $g1sub
      i32.const 3
      struct.new $t1
      drop
    )
  )
 "type mismatch: expected (ref null (exact"
)

(assert_invalid
  (module
    (rec
      (type $t1 (sub (descriptor $t2) (struct (field i32))))
      (type $t2 (sub (describes $t1) (struct (field (ref $f)))))
      (type $t1sub (sub $t1 (descriptor $t2sub) (struct (field i32 f32))))
      (type $t2sub (sub $t2 (describes $t1sub) (struct (field (ref $f) (ref $f)))))
      (type $f (func (result i32)))
    )

    (import "" "rtt1" (global $g1 (ref (exact $t2))))
    (import "" "rtt1sub" (global $g1sub (ref (exact $t2sub))))

    (func $test-struct-new-non-exact-2 (param (ref $t2))
      local.get 0
      struct.new_default $t1
      drop
    )
  )
  "type mismatch: expected (ref null (exact"
)

(assert_invalid
  (module
    (rec
      (type $t1 (sub (descriptor $t2) (struct (field i32))))
      (type $t2 (sub (describes $t1) (struct (field (ref $f)))))
      (type $t1sub (sub $t1 (descriptor $t2sub) (struct (field i32 f32))))
      (type $t2sub (sub $t2 (describes $t1sub) (struct (field (ref $f) (ref $f)))))
      (type $f (func (result i32)))
    )

    (import "" "rtt1" (global $g1 (ref (exact $t2))))
    (import "" "rtt1sub" (global $g1sub (ref (exact $t2sub))))

    (func $test-struct-new-non-exact-3
      global.get $g1
      i32.const 2
      f32.const 1
      struct.new_desc $t1sub
      drop
    )
  )
  "type mismatch: expected (ref null (exact"
)


(assert_invalid
  (module
    (rec
      (type $t1 (sub (descriptor $t2) (struct (field i32))))
      (type $t2 (sub (describes $t1) (struct (field (ref $f)))))
      (type $t1sub (sub $t1 (descriptor $t2sub) (struct (field i32 f32))))
      (type $t2sub (sub $t2 (describes $t1sub) (struct (field (ref $f) (ref $f)))))
      (type $f (func (result i32)))
    )

    (import "" "rtt1" (global $g1 (ref (exact $t2))))
    (import "" "rtt1sub" (global $g1sub (ref (exact $t2sub))))

    (func $test-br_on_cast_desc-bad-types (param $i (ref null $f)) (param $desc (ref null $t2)) (result (ref null $t1))
      local.get $i
      local.get $desc
      br_on_cast_desc 0 (ref null $f) (ref null $t1)
    )

  )
  "type mismatch:"
)
