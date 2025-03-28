;; RUN: wast --features=-exceptions --assert default --snapshot tests/snapshots %

(assert_invalid
  (component
    (core module $m (func (export "")))
    (core instance $i (instantiate $m))
    (alias core export $i "" (core tag $t))
  )
  "exceptions proposal not enabled")

(assert_invalid
  (component
    (core module $m (func (export "")))
    (core instance $i (instantiate $m))
    (core instance
      (export "" (tag 0)))
  )
  "exceptions proposal not enabled")
