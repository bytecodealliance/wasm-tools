;; RUN: wast --assert default --snapshot tests/snapshots % -f mvp,reference-types

(assert_invalid
  (module (type (array i32)))
  "array indexed types not supported without the gc feature")
(assert_invalid
  (module (type (array i8)))
  "array indexed types not supported without the gc feature")
(assert_invalid
  (module (type (struct)))
  "struct indexed types not supported without the gc feature")
