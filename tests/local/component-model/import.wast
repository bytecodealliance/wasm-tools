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
  (import "wasi:http/types" (func))
  (import "wasi:http/types@1.0.0" (func))
  (import "wasi:http/types@2.0.0" (func))
  (import "a-b:c-d/e-f@123456.7890.488" (func))
  (import "a:b/c@1.2.3" (func))
  (import "a:b/c@0.0.0" (func))
  (import "a:b/c@0.0.0+abcd" (func))
  (import "a:b/c@0.0.0+abcd-efg" (func))
  (import "a:b/c@0.0.0-abcd+efg" (func))
  (import "a:b/c@0.0.0-abcd.1.2+efg.4.ee.5" (func))
)

(assert_invalid
  (component
    (import "wasi:http/types" (func))
    (import "wasi:http/types" (func))
  )
  "conflicts with previous name")

(assert_invalid
  (component (import "" (func)))
  "`` is not in kebab case")
(assert_invalid
  (component (import "wasi:" (func)))
  "`` is not in kebab case")
(assert_invalid
  (component (import "wasi:/" (func)))
  "not in kebab case")
(assert_invalid
  (component (import ":/" (func)))
  "not in kebab case")
(assert_invalid
  (component (import "wasi/http" (func)))
  "`wasi/http` is not in kebab case")
(assert_invalid
  (component (import "wasi:http/TyPeS" (func)))
  "`TyPeS` is not in kebab case")
(assert_invalid
  (component (import "WaSi:http/types" (func)))
  "`WaSi` is not in kebab case")
(assert_invalid
  (component (import "wasi:HtTp/types" (func)))
  "`HtTp` is not in kebab case")
(assert_invalid
  (component (import "wasi:http/types@" (func)))
  "empty string")
(assert_invalid
  (component (import "wasi:http/types@." (func)))
  "unexpected character '.'")
(assert_invalid
  (component (import "wasi:http/types@1." (func)))
  "unexpected end of input")
(assert_invalid
  (component (import "wasi:http/types@a.2" (func)))
  "unexpected character 'a'")
(assert_invalid
  (component (import "wasi:http/types@2.b" (func)))
  "unexpected character 'b'")
(assert_invalid
  (component (import "wasi:http/types@2.0x0" (func)))
  "unexpected character 'x'")
(assert_invalid
  (component (import "wasi:http/types@2.0.0+" (func)))
  "empty identifier segment")
(assert_invalid
  (component (import "wasi:http/types@2.0.0-" (func)))
  "empty identifier segment")
(assert_invalid
  (component (import "foo:bar:baz/qux" (func)))
  "expected `/` after package name")
(assert_invalid
  (component (import "foo:bar/baz/qux" (func)))
  "trailing characters found: `/qux`")

(component
  (import "a" (func $a))
  (export "a" (func $a))
)

(component
  (import "unlocked-dep=<a:b>" (func))
  (import "unlocked-dep=<a:b@*>" (func))
  (import "unlocked-dep=<a:b@{>=1.2.3}>" (func))
  (import "unlocked-dep=<a:b@{>=1.2.3-rc}>" (func))
  (import "unlocked-dep=<a:b@{<1.2.3}>" (func))
  (import "unlocked-dep=<a:b@{<1.2.3-rc}>" (func))
  (import "unlocked-dep=<a:b@{>=1.2.3 <1.2.3}>" (func))
  (import "unlocked-dep=<a:b@{>=1.2.3-rc <1.2.3}>" (func))
)

(assert_invalid
  (component (import "unlocked-dep=" (func)))
  "expected `<` at ``")
(assert_invalid
  (component (import "unlocked-dep=<" (func)))
  "`` is not in kebab case")
(assert_invalid
  (component (import "unlocked-dep=<>" (func)))
  "`` is not in kebab case")
(assert_invalid
  (component (import "unlocked-dep=<:>" (func)))
  "`` is not in kebab case")
(assert_invalid
  (component (import "unlocked-dep=<a:>" (func)))
  "`` is not in kebab case")
(assert_invalid
  (component (import "unlocked-dep=<:a>" (func)))
  "`` is not in kebab case")
(assert_invalid
  (component (import "unlocked-dep=<a:a@>" (func)))
  "expected `{` at `>`")
(assert_invalid
  (component (import "unlocked-dep=<a:a@{xyz}>" (func)))
  "expected `>=` or `<` at start of version range")
(assert_invalid
  (component (import "unlocked-dep=<a:a@{<xyz}>" (func)))
  "`xyz` is not a valid semver")
(assert_invalid
  (component (import "unlocked-dep=<a:a@{<1.2.3 >=2.3.4}>" (func)))
  "`1.2.3 >=2.3.4` is not a valid semver")

(component
  (import "locked-dep=<a:b>" (func))
  (import "locked-dep=<a:b@1.2.3>" (func))
  (import "locked-dep=<a:b>,integrity=<sha256-a>" (func))
  (import "locked-dep=<a:b@1.2.3>,integrity=<sha256-a>" (func))
)

(assert_invalid
  (component (import "locked-dep=" (func)))
  "expected `<` at ``")
(assert_invalid
  (component (import "locked-dep=<" (func)))
  "`` is not in kebab case")
(assert_invalid
  (component (import "locked-dep=<:" (func)))
  "`` is not in kebab case")
(assert_invalid
  (component (import "locked-dep=<:>" (func)))
  "`` is not in kebab case")
(assert_invalid
  (component (import "locked-dep=<a:>" (func)))
  "`` is not in kebab case")
(assert_invalid
  (component (import "locked-dep=<:a>" (func)))
  "`` is not in kebab case")
(assert_invalid
  (component (import "locked-dep=<a:a" (func)))
  "expected `>` at ``")
(assert_invalid
  (component (import "locked-dep=<a:a@>" (func)))
  "is not a valid semver")
(assert_invalid
  (component (import "locked-dep=<a:a@1.2.3" (func)))
  "expected `>` at ``")
(assert_invalid
  (component (import "locked-dep=<a:a@1.2.3>," (func)))
  "expected `integrity=<`")
(assert_invalid
  (component (import "locked-dep=<a:a@1.2.3>x" (func)))
  "trailing characters found: `x`")

(component
  (import "url=<>" (func))
  (import "url=<a>" (func))
  (import "url=<a>,integrity=<sha256-a>" (func))
)

(assert_invalid
  (component (import "url=" (func)))
  "expected `<` at ``")
(assert_invalid
  (component (import "url=<" (func)))
  "failed to find `>`")
(assert_invalid
  (component (import "url=<<>" (func)))
  "url cannot contain `<`")

(assert_invalid
  (component
    (import "relative-url=<>" (func))
    (import "relative-url=<a>" (func))
    (import "relative-url=<a>,integrity=<sha256-a>" (func))
  )
  "not a valid extern name")

(assert_invalid
  (component (import "relative-url=" (func)))
  "not a valid extern name")
(assert_invalid
  (component (import "relative-url=<" (func)))
  "not a valid extern name")
(assert_invalid
  (component (import "relative-url=<<>" (func)))
  "not a valid extern name")

(component
  (import "integrity=<sha256-a>" (func))
  (import "integrity=<sha384-a>" (func))
  (import "integrity=<sha512-a>" (func))
  (import "integrity=<sha512-a sha256-b>" (func))
  (import "integrity=< sha512-a sha256-b >" (func))
  (import "integrity=<  sha512-a?abcd  >" (func))
  (import "integrity=<sha256-abcdefghijklmnopqrstuvwxyz>" (func))
  (import "integrity=<sha256-ABCDEFGHIJKLMNOPQRSTUVWXYZ>" (func))
  (import "integrity=<sha256-++++++++++++++++++++==>" (func))
  (import "integrity=<sha256-////////////////////==>" (func))
)
(assert_invalid
  (component (import "integrity=<>" (func)))
  "integrity hash cannot be empty")
(assert_invalid
  (component (import "integrity=<sha256>" (func)))
  "expected `-` after hash algorithm")
(assert_invalid
  (component (import "integrity=<sha256->" (func)))
  "not valid base64")
(assert_invalid
  (component (import "integrity=<sha256-^^^^>" (func)))
  "not valid base64")
(assert_invalid
  (component (import "integrity=<sha256-=========>" (func)))
  "not valid base64")
(assert_invalid
  (component (import "integrity=<sha256-=>" (func)))
  "not valid base64")
(assert_invalid
  (component (import "integrity=<sha256-==>" (func)))
  "not valid base64")
(assert_invalid
  (component (import "integrity=<md5-ABC>" (func)))
  "unrecognized hash algorithm")

;; Prior to WebAssembly/component-model#263 this was a valid component.
;; Specifically the 0x01 prefix byte on the import was valid. Nowadays that's
;; not valid in the spec but it's accepted for backwards compatibility. This
;; tests is here to ensure such compatibility. In the future this test should
;; be changed to `(assert_invalid ...)`
(component binary
  "\00asm" "\0d\00\01\00"   ;; component header

  "\07\05"          ;; type section, 5 bytes large
  "\01"             ;; 1 count
  "\40"             ;; function
  "\00"             ;; parameters, 0 count
  "\01\00"          ;; results, named, 0 count

  "\0a\06"          ;; import section, 6 bytes large
  "\01"             ;; 1 count
  "\01"             ;; prefix byte of 0x01 (invalid by the spec nowadays)
  "\01a"            ;; name = "a"
  "\01\00"          ;; type = func ($type 0)
)
