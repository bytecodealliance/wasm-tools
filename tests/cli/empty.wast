;; RUN: wast --assert default --snapshot tests/snapshots %

(assert_malformed (module quote "") "x")
(assert_malformed (module quote "; foo") "x")
(assert_malformed (module quote "(; foo ;)") "x")
(assert_malformed (module quote " ") "x")
(assert_malformed (module quote "\t") "x")
