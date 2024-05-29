;; Test that various return calls must exactly match the callee's returns, not
;; simply leave the operand stack in a state where a `call; return` would
;; otherwise be valid, but with some dangling stack values. Those dangling stack
;; values are valid for regular calls, but not for return calls.

(assert_invalid
  (module
    (func $f (result i32 i32) unreachable)
    (func (result i32)
      return_call $f
    )
  )
  "type mismatch: current function requires result type [i32] but callee returns [i32 i32]"
)

(assert_invalid
  (module
    (type $ty (func (result i32 i32)))
    (import "env" "table" (table $table 0 funcref))
    (func (param i32) (result i32)
      local.get 0
      return_call_indirect $table (type $ty)
    )
  )
  "type mismatch: current function requires result type [i32] but callee returns [i32 i32]"
)

(assert_invalid
  (module
    (type $ty (func (result i32 i32)))
    (func (param funcref) (result i32)
      local.get 0
      return_call_ref $ty
    )
  )
  "type mismatch: current function requires result type [i32] but callee returns [i32 i32]"
)
