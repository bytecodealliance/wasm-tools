;; RUN: wast --assert default --snapshot tests/snapshots % -f=-acquire-release-atomics

(assert_malformed
  (module (func atomic.fence acqrel))
  "nonzero byte after `atomic.fence`")

(assert_invalid
  (module (func pause))
  "acquire-release atomics support is not enabled")

(module
  (memory 1 1)
  (func
    atomic.fence
    atomic.fence seqcst
    (drop (i32.atomic.load (i32.const 0)))
  )
)
