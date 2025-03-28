;; RUN: wast --assert default --snapshot tests/snapshots %

(assert_invalid
  (component
    (core module $m (func (export "")))
    (core instance $i (instantiate $m))
    (alias core export $i "" (core tag $t))
  )
  "export `` for core instance 0 is not a tag")

(component
  (core module $m (tag (export "")))
  (core instance $i (instantiate $m))
  (alias core export $i "" (core tag $t))
)

(component
  (core module $m (tag (export "")))
  (core instance $i (instantiate $m))
  (core instance
    (export "" (tag $i ""))))

(assert_invalid
  (component
    (core module $m (func (export "")))
    (core instance $i (instantiate $m))
    (core instance
      (export "" (tag 0)))
  )
  "unknown tag 0")
