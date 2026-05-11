;; RUN: wast --assert default --snapshot tests/snapshots % -f custom-page-sizes

;; explicitly specifying the page size is ok
(component
  (import "f" (func $f (param "x" string)))
  (core module $m
    (memory (export "m") 1 (pagesize 65536))
  )
  (core instance $i (instantiate $m))
  (core func (canon lower (func $f) (memory $i "m")))
)

;; currently the component model requires all memories are a subtype of
;; `(memory 0)`, AKA no compat with custom page sizes. This has not yet been
;; relaxed at the spec level.
(assert_invalid
  (component
    (import "f" (func $f (param "x" string)))
    (core module $m
      (memory (export "m") 1 (pagesize 1))
    )
    (core instance $i (instantiate $m))
    (core func (canon lower (func $f) (memory $i "m")))
  )
  "mismatch in page size for memories"
)

;; subtyping works with custom-page-sizes
(component
  (core module $a (memory (export "m") 1 (pagesize 65536)))
  (core module $b (import "a" "m" (memory 1 (pagesize 65536))))
  (core instance $a (instantiate $a))
  (core instance $b (instantiate $b (with "a" (instance $a))))
)

(component
  (core module $a (memory (export "m") 1 (pagesize 65536)))
  (core module $b (import "a" "m" (memory 1)))
  (core instance $a (instantiate $a))
  (core instance $b (instantiate $b (with "a" (instance $a))))
)

(component
  (core module $a (memory (export "m") 1))
  (core module $b (import "a" "m" (memory 1 (pagesize 65536))))
  (core instance $a (instantiate $a))
  (core instance $b (instantiate $b (with "a" (instance $a))))
)

(assert_invalid
  (component
    (core module $a (memory (export "m") 1 (pagesize 1)))
    (core module $b (import "a" "m" (memory 1 (pagesize 65536))))
    (core instance $a (instantiate $a))
    (core instance $b (instantiate $b (with "a" (instance $a))))
  )
  "mismatch in page size for memories")

(assert_invalid
  (component
    (core module $a (memory (export "m") 1 (pagesize 65536)))
    (core module $b (import "a" "m" (memory 1 (pagesize 1))))
    (core instance $a (instantiate $a))
    (core instance $b (instantiate $b (with "a" (instance $a))))
  )
  "mismatch in page size for memories")

(assert_invalid
  (component
    (core module $a (memory (export "m") 1 (pagesize 1)))
    (core module $b (import "a" "m" (memory 1)))
    (core instance $a (instantiate $a))
    (core instance $b (instantiate $b (with "a" (instance $a))))
  )
  "mismatch in page size for memories")

(assert_invalid
  (component
    (core module $a (memory (export "m") 1))
    (core module $b (import "a" "m" (memory 1 (pagesize 1))))
    (core instance $a (instantiate $a))
    (core instance $b (instantiate $b (with "a" (instance $a))))
  )
  "mismatch in page size for memories")
