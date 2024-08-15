;; RUN: component unbundle --module-dir %tmpdir --wat % --threshold 0

(component $C
  (core type $m (module))
  (import "x" (core module (type $m)))

  (component
  )

  (core module
    (func $f)
    (export "x" (func $f))
  )
)

