;; RUN: wast % --assert default --snapshot tests/snapshots \
;;        -f=cm-async,-cm-async-builtins

;; {future,stream}.cancel-{read,write}
(assert_invalid
  (component
    (type $t (future))
    (core func (canon future.cancel-read $t async)))
  "requires the component model async builtins feature")
(assert_invalid
  (component
    (type $t (future))
    (core func (canon future.cancel-write $t async)))
  "requires the component model async builtins feature")
(assert_invalid
  (component
    (type $t (stream))
    (core func (canon stream.cancel-read $t async)))
  "requires the component model async builtins feature")
(assert_invalid
  (component
    (type $t (stream))
    (core func (canon stream.cancel-write $t async)))
  "requires the component model async builtins feature")

(component
  (type $f (future))
  (type $s (stream))
  (core func (canon future.cancel-read $f))
  (core func (canon future.cancel-write $f))
  (core func (canon stream.cancel-read $s))
  (core func (canon stream.cancel-write $s))
)

;; async resource.drop
(assert_invalid
  (component
    (type $t (resource (rep i32)))
    (core func (canon resource.drop $t async)))
  "requires the component model async builtins feature")
(component
  (type $t (resource (rep i32)))
  (core func (canon resource.drop $t)))
