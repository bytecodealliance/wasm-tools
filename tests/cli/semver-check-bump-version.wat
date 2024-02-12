;; RUN: component semver-check % --prev prev --new new

(component
  (type $prev (component
    (export "a:b/prev" (component
      (import "a:a/a@1.0.0" (instance
        (export "f" (func))
      ))
    ))
  ))
  (export "prev" (type $prev))

  (type $new (component
    (export "a:b/new" (component
      (import "a:a/a@1.0.1" (instance
        (export "f" (func))
      ))
    ))
  ))
  (export "new" (type $new))
)
