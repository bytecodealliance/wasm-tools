;; RUN: wast --assert default --snapshot tests/snapshots % -f cm-async

;; historically these were part of the component-model-async development but
;; they have since been removed.

(assert_invalid
  (component
    (import "[async]f" (func))
  )
  "not in kebab case")

(assert_invalid
  (component
    (import "[async method]f" (func))
  )
  "not in kebab case")

(assert_invalid
  (component
    (import "[async static]f" (func))
  )
  "not in kebab case")


(assert_invalid
  (component
    (import "a" (type $a (sub resource)))
    (import "[constructor]a" (func async (result (own $a)))))
  "constructor function cannot be async")
