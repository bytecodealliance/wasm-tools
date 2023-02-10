;; RUN: dump %

(component
  (import "a" (core module $m
    (import "" "f" (func))
    (import "" "g" (global i32))
    (import "" "t" (table 1 funcref))
    (import "" "m" (memory 1))
  ))
  (type $empty (instance))
)
