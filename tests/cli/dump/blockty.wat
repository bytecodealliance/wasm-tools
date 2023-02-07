;; RUN: dump %

(module
  (type $empty (func))
  (type $t (func (result i32)))
  (func
    (block)
    (block (result i32))
    (block (param i32))
    (block (param i32) (result i32))
    (block (param i32) (result i32 i32))
    (block (type $t))
    (block (type $t) (result i32))
    (block (type $empty))
  )
)
