;; RUN: print %

(module
  (type (;0;) (func))
  (func (;0;) (type 0)
    block $a
      block $a
        br 1
      end
    end
  )
)
