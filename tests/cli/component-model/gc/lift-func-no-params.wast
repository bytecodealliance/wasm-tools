;; RUN: wast --assert default --snapshot tests/snapshots % -f gc,cm-gc

(component $A
  (core module $m
    (func (export "f"))
  )

  (core instance $i (instantiate $m))

  (func (export "f")
    (canon lift (core func $i "f") gc)
  )
)
