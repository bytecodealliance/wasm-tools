;; FAIL: component wit %

(component
    (type (component
        (type (component))
        (export "a:b/w" (component (type 0)))
    ))
    (export "w" (type 0))
    (@custom "package-docs" "\01{\22worlds\22:{\22w\22:{\22stability\22:{\22unstable\22:{\22feature\22:\22]x\22}}}}}")
)
