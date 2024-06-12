;; RUN: print %
(module
  (type (func (param i32) (result i32)))
  (func (type 0) (param $l i32) (result i32)
    (local $#local1<l> (@name "l") i32) (local $#local2<l> (@name "l") i32)
    local.get $l
  )
)
