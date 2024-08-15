;; RUN: component unbundle --module-dir %tmpdir --wat % --threshold 10

(component
  (core module $not-extracted) ;; not extracted, too small

  (core module $"extract me please"
    (import "a" "b" (func))
  )
)
