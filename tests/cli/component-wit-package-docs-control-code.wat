;; FAIL: component wit %

(component
    (type (component
        (type (component))
        (export "a:b/w" (component (type 0)))
    ))
    (export "w" (type 0))
    (@custom "package-docs" "\01{\22docs\22:\22a\7fb\22}")
)
