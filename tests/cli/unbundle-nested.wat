;; RUN: component unbundle --module-dir %tmpdir --wat % --threshold 0

(component $C
  (core module $outer
    (import "a" "b" (global i32))
  )
  (component $C2
    (core module $inner
      (import "b" "c" (global f32))
    )
  )
)
