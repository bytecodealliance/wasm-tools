;; RUN: wast --assert default --snapshot tests/snapshots % -f shared-everything-threads

(assert_invalid
  (module
    (rec
      (type (; super ;) (sub (func (result (ref null (shared func))))))
      (type (; sub ;) (sub 0 (func (result (ref null func)))))
    )
  )
  "sub type must match super type"
)

(assert_invalid
  (module
    (rec
      (type (; super ;) (sub (func (param (ref null (shared func))))))
      (type (; sub ;) (sub 0 (func (param (ref null 0)))))
    )
  )
  "sub type must match super type"
)
