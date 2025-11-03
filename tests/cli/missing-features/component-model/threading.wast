;; RUN: wast % --assert default --snapshot tests/snapshots \
;;        -f=cm-async,-cm-threading

;; thread.*
(assert_invalid
  (component
    (core func (canon thread.index))
    (core func (canon thread.new-indirect 0 (table 0)))
    (core func (canon thread.switch-to))
    (core func (canon thread.suspend))
    (core func (canon thread.resume-later))
    (core func (canon thread.yield-to)))
  "requires the component model threading feature")