;; RUN: print --indent 4 %

(;@0     ;) (module
(;@b     ;)   (type (;0;) (func (param i32) (result i32)))
(;@1f    ;)   (func (;0;) (type 0) (param i32) (result i32)
(;@20    ;)     local.get 0
              )
(;@17    ;)   (export "f" (func 0))
            )