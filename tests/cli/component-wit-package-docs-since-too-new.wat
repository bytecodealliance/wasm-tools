;; FAIL: component wit %

(component
    (type (component
        (type (component))
        (export "a:b/w@1.0.0" (component (type 0)))
    ))
    (export "w" (type 0))
    (@custom "package-docs" "\01{\22worlds\22:{\22w\22:{\22stability\22:{\22stable\22:{\22since\22:\222.0.0\22}}}}}")
)
