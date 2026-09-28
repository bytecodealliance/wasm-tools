;; RUN: wast --assert default --snapshot tests/snapshots % -f custom-descriptors,stack-switching

(assert_invalid
  (module
    (type $f (func))
    (type $ct (cont $f))
    (func (param contref) (result (ref null $ct))
      (block (result (ref null $ct))
        local.get 0
        br_on_cast 0 contref (ref null $ct)
        drop
        ref.null $ct)))
  "invalid cast: cannot cast to a continuation type")

(assert_invalid
  (module
    (type $f (func))
    (type $ct (cont $f))
    (func (param contref) (result contref)
      (block (result contref)
        local.get 0
        br_on_cast_fail 0 contref (ref null $ct))))
  "invalid cast: cannot cast to a continuation type")

(assert_invalid
  (module
    (type $f (func))
    (type $ct (cont $f))
    (func (param contref) (result (ref null $ct))
      local.get 0
      ref.cast (ref null $ct)))
  "invalid cast: cannot cast to a continuation type")

(assert_invalid
  (module
    (type $f (func))
    (type $ct (cont $f))
    (func (param contref) (result i32)
      local.get 0
      ref.test (ref null $ct)))
  "invalid cast: cannot cast to a continuation type")
