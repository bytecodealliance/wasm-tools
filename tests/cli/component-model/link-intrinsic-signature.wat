;; RUN[validate-input]: validate %
;; FAIL: component link --dl-openable %

(module
  (@dylink.0)
  (import "env" "__wasm_get_stack_pointer" (func (param i64) (result i32)))
)
