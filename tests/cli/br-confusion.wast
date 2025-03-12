;; RUN: wast --assert default --snapshot tests/snapshots %

(module
  (type (;0;) (func))
  (func (;0;) (type 0)
    block $a
      block $a
        br 1 (;@1;)
      end
    end
  )
)
