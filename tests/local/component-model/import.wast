(component
  (import "a" (func))
  (import "b" (instance))
  (import "c" (instance
    (export "a" (func))
  ))
  (import "d" (component
    (import "a" (core module))
    (export "b" (func))
  ))
  (type $t (func))
  (import "e" (type (eq $t)))
)

(assert_invalid
  (component
    (type $f (func))
    (import "a" (instance (type $f)))
  )
  "type index 0 is not an instance type")

(assert_invalid
  (component
    (core type $f (func))
    (import "a" (core module (type $f)))
  )
  "core type index 0 is not a module type")

(assert_invalid
  (component
    (type $f string)
    (import "a" (func (type $f)))
  )
  "type index 0 is not a function type")

;; Disallow duplicate imports for core wasm modules
(assert_invalid
  (component
    (core type (module
      (import "" "" (func))
      (import "" "" (func))
    ))
  )
  "duplicate import name `:`")
(assert_invalid
  (component
    (core module
      (import "" "" (func))
      (import "" "" (func))
    )
  )
  "duplicate import name `:`")
(assert_invalid
  (component
    (core type (module
      (import "" "a" (func))
      (import "" "a" (func))
    ))
  )
  "duplicate import name `:a`")
(assert_invalid
  (component
    (core module
      (import "" "a" (func))
      (import "" "a" (func))
    )
  )
  "duplicate import name `:a`")

(assert_malformed
  (component quote
    "(import \"a\" (func))"
    "(import \"a\" (func))"
  )
  "import name `a` conflicts with previous name `a`")

(assert_malformed
  (component quote
    "(type (component"
      "(import \"a\" (func))"
      "(import \"a\" (func))"
    "))"
  )
  "import name `a` conflicts with previous name `a`")

(assert_invalid
  (component
    (import "a" (func (type 100)))
  )
  "type index out of bounds")

(assert_invalid
  (component
    (core module $m (func (export "")))
    (core instance $i (instantiate $m))
    (func (type 100) (canon lift (core func $i "")))
  )
  "type index out of bounds")

(assert_invalid
  (component
    (import "a" (value string))
  )
  "value index 0 was not used as part of an instantiation, start function, or export")

(component
  (import "a" (value string))
  (export "b" (value 0))
)

(component
  (import (interface "wasi:http/types") (func))
  (import (interface "wasi:http/types@1.0.0") (func))
  (import (interface "wasi:http/types@2.0.0") (func))
  (import (interface "a-b:c-d/e-f@123456.7890.488") (func))
  (import (interface "a:b/c@1.2.3") (func))
  (import (interface "a:b/c@0.0.0") (func))
  (import (interface "a:b/c@0.0.0+abcd") (func))
  (import (interface "a:b/c@0.0.0+abcd-efg") (func))
  (import (interface "a:b/c@0.0.0-abcd+efg") (func))
  (import (interface "a:b/c@0.0.0-abcd.1.2+efg.4.ee.5") (func))
)

(assert_invalid
  (component
    (import (interface "wasi:http/types") (func))
    (import (interface "wasi:http/types") (func))
  )
  "conflicts with previous name")

(assert_invalid
  (component (import (interface "") (func)))
  "failed to find `:` character")
(assert_invalid
  (component (import (interface "wasi") (func)))
  "failed to find `:` character")
(assert_invalid
  (component (import (interface "wasi:") (func)))
  "failed to find `/` character")
(assert_invalid
  (component (import (interface "wasi:/") (func)))
  "not in kebab case")
(assert_invalid
  (component (import (interface ":/") (func)))
  "not in kebab case")
(assert_invalid
  (component (import (interface "wasi/http") (func)))
  "failed to find `:` character")
(assert_invalid
  (component (import (interface "wasi:http/TyPeS") (func)))
  "`TyPeS` is not in kebab case")
(assert_invalid
  (component (import (interface "WaSi:http/types") (func)))
  "`WaSi` is not in kebab case")
(assert_invalid
  (component (import (interface "wasi:HtTp/types") (func)))
  "`HtTp` is not in kebab case")
(assert_invalid
  (component (import (interface "wasi:http/types@") (func)))
  "empty string")
(assert_invalid
  (component (import (interface "wasi:http/types@.") (func)))
  "unexpected character '.'")
(assert_invalid
  (component (import (interface "wasi:http/types@1.") (func)))
  "unexpected end of input")
(assert_invalid
  (component (import (interface "wasi:http/types@a.2") (func)))
  "unexpected character 'a'")
(assert_invalid
  (component (import (interface "wasi:http/types@2.b") (func)))
  "unexpected character 'b'")
(assert_invalid
  (component (import (interface "wasi:http/types@2.0x0") (func)))
  "unexpected character 'x'")
(assert_invalid
  (component (import (interface "wasi:http/types@2.0.0+") (func)))
  "empty identifier segment")
(assert_invalid
  (component (import (interface "wasi:http/types@2.0.0-") (func)))
  "empty identifier segment")

(assert_invalid
  (component
    (import "a" (func $a))
    (export "a" (func $a))
  )
  "export name `a` conflicts with previous name `a`")
