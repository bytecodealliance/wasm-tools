;; RUN: wast --assert default --snapshot tests/snapshots % -f cm-async,cm-threading,cm64

(assert_invalid
  (component
    (core func (canon context.set i32 0))
    (core func (canon context.get i64 1))
  )
  "type must match previous context type")

(assert_invalid
  (component
    (core func (canon context.get i64 0))
    (core func (canon context.set i32 1))
  )
  "type must match previous context type")


;; thread.new-indirect: 64-bit closure parameter
(component
  (core type $start (func (param i64)))
  (core module $m (table (export "t") 1 funcref))
  (core instance $i (instantiate $m))
  (alias core export $i "t" (core table $t))
  (core func $new (canon thread.new-indirect $start (core table $t)))
  (core module $use (import "" "new" (func (param i32 i64) (result i32))))
  (core instance (instantiate $use (with "" (instance (export "new" (func $new))))))
)

;; thread.new-indirect: 64-bit table
(component
  (core type $start (func (param i32)))
  (core module $m (table (export "t") i64 1 funcref))
  (core instance $i (instantiate $m))
  (alias core export $i "t" (core table $t))
  (core func $new (canon thread.new-indirect $start (core table $t)))
  (core module $use (import "" "new" (func (param i64 i32) (result i32))))
  (core instance (instantiate $use (with "" (instance (export "new" (func $new))))))
)

;; thread.new-indirect: 64-bit table and closure parameter
(component
  (core type $start (func (param i64)))
  (core module $m (table (export "t") i64 1 funcref))
  (core instance $i (instantiate $m))
  (alias core export $i "t" (core table $t))
  (core func $new (canon thread.new-indirect $start (core table $t)))
  (core module $use (import "" "new" (func (param i64 i64) (result i32))))
  (core instance (instantiate $use (with "" (instance (export "new" (func $new))))))
)

;; the signature follows the table's index type
(assert_invalid
  (component
    (core type $start (func (param i32)))
    (core module $m (table (export "t") i64 1 funcref))
    (core instance $i (instantiate $m))
    (alias core export $i "t" (core table $t))
    (core func $new (canon thread.new-indirect $start (core table $t)))
    (core module $use (import "" "new" (func (param i32 i32) (result i32))))
    (core instance (instantiate $use (with "" (instance (export "new" (func $new))))))
  )
  "type mismatch")

;; other closure parameter types are still rejected
(assert_invalid
  (component
    (core type $start (func (param f64)))
    (core module $m (table (export "t") 1 funcref))
    (core instance $i (instantiate $m))
    (alias core export $i "t" (core table $t))
    (core func $new (canon thread.new-indirect $start (core table $t)))
  )
  "start function must take a single `i32` argument")
