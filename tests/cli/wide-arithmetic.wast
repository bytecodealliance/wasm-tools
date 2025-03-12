;; RUN: wast --assert default --snapshot tests/snapshots % -f wide-arithmetic

(module
  (func (param i64 i64 i64 i64) (result i64 i64)
    local.get 0
    local.get 1
    local.get 2
    local.get 3
    i64.add128)
  (func (param i64 i64 i64 i64) (result i64 i64)
    local.get 0
    local.get 1
    local.get 2
    local.get 3
    i64.sub128)
  (func (param i64 i64) (result i64 i64)
    local.get 0
    local.get 1
    i64.mul_wide_s)
  (func (param i64 i64) (result i64 i64)
    local.get 0
    local.get 1
    i64.mul_wide_u)
)

(assert_invalid
  (module
    (func (param i64 i64 i64 i64) (result i64)
      local.get 0
      local.get 1
      local.get 2
      local.get 3
      i64.add128)
  )
  "type mismatch")
(assert_invalid
  (module
    (func (param i64 i64 i64) (result i64 i64)
      local.get 0
      local.get 1
      local.get 2
      i64.add128)
  )
  "type mismatch")

(assert_invalid
  (module
    (func (param i64 i64 i64 i64) (result i64)
      local.get 0
      local.get 1
      local.get 2
      local.get 3
      i64.sub128)
  )
  "type mismatch")
(assert_invalid
  (module
    (func (param i64 i64 i64) (result i64 i64)
      local.get 0
      local.get 1
      local.get 2
      i64.sub128)
  )
  "type mismatch")

(assert_invalid
  (module
    (func (param i64 i64) (result i64)
      local.get 0
      local.get 1
      i64.mul_wide_s)
  )
  "type mismatch")
(assert_invalid
  (module
    (func (param i64) (result i64 i64)
      local.get 0
      i64.mul_wide_s)
  )
  "type mismatch")

(assert_invalid
  (module
    (func (param i64 i64) (result i64)
      local.get 0
      local.get 1
      i64.mul_wide_u)
  )
  "type mismatch")
(assert_invalid
  (module
    (func (param i64) (result i64 i64)
      local.get 0
      i64.mul_wide_u)
  )
  "type mismatch")
