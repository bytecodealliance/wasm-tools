;; RUN: component unbundle --module-dir %tmpdir --wat % --threshold 0

(component $C
  (core type $t (func))
  (core type $m (module
    (alias outer $C $t (type))
  ))

  (component $C2
    (alias outer $C $t (core type))
    (core type $t (func))
    (core type $m (module
      (alias outer $C2 $t (type))
    ))
  )

  (core module $m
    (func $f)
    (export "x" (func $f))
  )

  (alias outer $C $m (core module $m2))

  (component
    (alias outer $C $m (core module))
    (alias outer $C $m2 (core module))
  )
)


