;; RUN: wast % --assert default --snapshot tests/snapshots \
;;        -f=cm-async,-cm-threading

;; thread.*
(assert_invalid
  (component (core func (canon thread.index)))
  "requires the component model threading feature")

(assert_invalid
  (component (core func (canon thread.new-indirect 0 (table 0))))
  "requires the component model threading feature")

(assert_invalid
  (component (core func (canon thread.resume-later)))
  "requires the component model threading feature")

(assert_invalid
  (component (core func (canon thread.suspend)))
  "requires the component model threading feature")

;; this is cm-async gated, not cm-threading gated
(component (core func (canon thread.yield)))

(assert_invalid
  (component (core func (canon thread.suspend-then-resume)))
  "requires the component model threading feature")

(assert_invalid
  (component (core func (canon thread.yield-then-resume)))
  "requires the component model threading feature")

(assert_invalid
  (component (core func (canon thread.suspend-then-promote)))
  "requires the component model threading feature")

(assert_invalid
  (component (core func (canon thread.yield-then-promote)))
  "requires the component model threading feature")
