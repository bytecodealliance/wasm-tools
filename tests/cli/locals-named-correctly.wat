;; RUN: print %

(module
  (type $a (func (param i32)))

  (func (type $a)
    (local $b i32)
  )
)
