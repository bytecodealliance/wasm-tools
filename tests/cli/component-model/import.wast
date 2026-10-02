;; RUN: wast --assert default --snapshot tests/snapshots %

(component
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

(assert_invalid
  (component
    (import "a" (func))
    (import "a" (func))
  )
  "import name `a` conflicts with previous name `a`")

(assert_invalid
  (component
    (type (component
      (import "a" (func))
      (import "a" (func))
    ))
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

(component definition
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
  (component
    (import "a" (func $a))
    (export "a" (func $a))
  )
)

;; The `depname`, `urlname` and `hashname` forms of `externname` were removed
;; from the component model in WebAssembly/component-model#672 in favor of the
;; `external-id` attribute, so these are no longer valid names.
(assert_invalid
  (component (import "unlocked-dep=<a:b>" (func)))
  "import name `unlocked-dep=<a:b>` is not a valid extern name")
(assert_invalid
  (component (import "unlocked-dep=<a:b@{>=1.2.3 <1.2.3}>" (func)))
  "not a valid extern name")
(assert_invalid
  (component (import "locked-dep=<a:b@1.2.3>" (func)))
  "import name `locked-dep=<a:b@1.2.3>` is not a valid extern name")
(assert_invalid
  (component (import "locked-dep=<a:b@1.2.3>,integrity=<sha256-a>" (func)))
  "not a valid extern name")
(assert_invalid
  (component (import "url=<a>" (func)))
  "import name `url=<a>` is not a valid extern name")
(assert_invalid
  (component (import "url=<a>,integrity=<sha256-a>" (func)))
  "not a valid extern name")
(assert_invalid
  (component (import "integrity=<sha256-a>" (func)))
  "import name `integrity=<sha256-a>` is not a valid extern name")

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

;; Prior to WebAssembly/component-model#263 this was a valid component.
;; Specifically the 0x01 prefix byte on the import was valid. Nowadays that's
;; not valid in the spec but it's accepted for backwards compatibility. This
;; tests is here to ensure such compatibility. In the future this test should
;; be changed to `(assert_invalid ...)`
(component definition binary
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

(component
  (component $c
    (type $t (instance (type $u u32) (export "t" (type (eq $u)))))
    (import "a" (instance (type $t)))
    (import "b" (instance (type $t)))
  )
  (type $u u32)
  (instance $i (export "t" (type $u)))
  (instance (instantiate $c
    (with "a" (instance $i))
    (with "b" (instance $i))
  ))
)

(component
  (component $c
    (type $inner (instance (type $u u32) (export "t" (type (eq $u)))))
    (type $outer (instance
      (export "a" (instance (type $inner)))
      (export "b" (instance (type $inner)))))
    (import "x" (instance (type $outer))))
  (type $u u32)
  (instance $i (export "t" (type $u)))
  (instance $o (export "a" (instance $i)) (export "b" (instance $i)))
  (instance (instantiate $c (with "x" (instance $o))))
)

(component
  (component $c
    (type $t (instance (type $u u32) (export "t" (type (eq $u)))))
    (import "a" (instance $a (type $t)))
    (import "b" (instance $b (type $t)))
    (alias export $a "t" (type $ta))
    (alias export $b "t" (type $tb))
    (export "ta" (type $ta))
    (export "tb" (type $tb))
    (type $la (list $ta))
    (type $lb (list $tb))
    (export "la" (type $la))
    (export "lb" (type $lb)))
  (type $u1 u32)
  (type $u2 u32)
  (instance $i1 (export "t" (type $u1)))
  (instance $i2 (export "t" (type $u2)))
  (instance $r (instantiate $c
    (with "a" (instance $i1))
    (with "b" (instance $i2))
  ))
  (export "r" (instance $r))
)

(component
  (type $t (instance (type $u u32) (export "t" (type (eq $u)))))
  (type $ct (component
    (import "a" (instance (type $t)))
    (import "b" (instance (type $t))))
  )
  (import "c" (component $c (type $ct)))
  (type $u u32)
  (instance $i (export "t" (type $u)))
  (instance (instantiate $c
    (with "a" (instance $i))
    (with "b" (instance $i))
  ))
)
