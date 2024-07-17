;; RUN: print %
(module
  (func
    block $a
      br $a
    end
    block $a
      br $a
    end
  )
)
