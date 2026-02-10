;; RUN: wast % --assert default --snapshot tests/snapshots \
;;        -f=cm-async,-cm-threading

;; thread.*
(assert_invalid
  (component
    (core func (canon thread.index))
    (core func (canon thread.new-indirect 0 (table 0)))
    (core func (canon thread.suspend-to-suspended))
    (core func (canon thread.suspend))
    (core func (canon thread.suspend-to))
    (core func (canon thread.unsuspend))
    (core func (canon thread.yield-to-suspended)))
  "requires the component model threading feature")