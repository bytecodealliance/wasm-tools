;; Test that unnamed types in various types are all detected

(assert_invalid
  (component
    (type $t (record (field "f" u32)))
    (type $f (record (field "f" $t)))
    (export "f" (type $f))
  )
  "type not valid to be used as export")

(assert_invalid
  (component
    (type $t (record (field "f" u32)))
    (type $f (list $t))
    (export "f" (type $f))
  )
  "type not valid to be used as export")

(assert_invalid
  (component
    (type $t (record (field "f" u32)))
    (type $f (tuple $t))
    (export "f" (type $f))
  )
  "type not valid to be used as export")

(assert_invalid
  (component
    (type $t (record (field "f" u32)))
    (type $f (variant (case "c" $t)))
    (export "f" (type $f))
  )
  "type not valid to be used as export")

(assert_invalid
  (component
    (type $t (record (field "f" u32)))
    (type $f (option $t))
    (export "f" (type $f))
  )
  "type not valid to be used as export")

(assert_invalid
  (component
    (type $t (record (field "f" u32)))
    (type $f (result $t))
    (export "f" (type $f))
  )
  "type not valid to be used as export")

;; Test that various types are all flagged as "requires a name"

(assert_invalid
  (component
    (type $t (record (field "f" u32)))
    (type $f (list $t))
    (export "f" (type $f))
  )
  "type not valid to be used as export")

(assert_invalid
  (component
    (type $t (enum "a"))
    (type $f (list $t))
    (export "f" (type $f))
  )
  "type not valid to be used as export")

(assert_invalid
  (component
    (type $t (flags "a"))
    (type $f (list $t))
    (export "f" (type $f))
  )
  "type not valid to be used as export")

(assert_invalid
  (component
    (type $t (variant (case "a")))
    (type $f (list $t))
    (export "f" (type $f))
  )
  "type not valid to be used as export")

(assert_invalid
  (component
    (type $t (resource (rep i32)))
    (type $f (list (own $t)))
    (export "f" (type $f))
  )
  "type not valid to be used as export")

;; Some types don't need names
(component
  (type $t1 (tuple (tuple u32)))
  (export "t1" (type $t1))

  (type $t2 (option (tuple (list u8) (result (list u32) (error (option string))))))
  (export "t2" (type $t2))

  (type $t3 u32)
  (export "t3" (type $t3))
)

(component
  (type $t' (record (field "f" u32)))
  (export $t "t" (type $t'))
  (type $t2 (record (field "x" $t)))
  (export "t2" (type $t2))
)

;; imports are validated as well
(assert_invalid
  (component
    (type $t (record (field "f" u32)))
    (type $t2 (record (field "f" $t)))
    (import "x" (type (eq $t2)))
  )
  "type not valid to be used as import")
(component
  (type $t (record (field "f" u32)))
  (import "t" (type $t' (eq $t)))
  (type $t2 (record (field "f" $t')))
  (import "x" (type (eq $t2)))
)
(assert_invalid
  (component
    (type $t (record (field "f" u32)))
    (type $t2 (record (field "f" $t)))
    (import "x" (func (param "x" $t2)))
  )
  "func not valid to be used as import")

(assert_invalid
  (component
    (type $t (resource (rep i32)))
    (export "t" (type $t))
    (type $f (list (own $t)))
    (export "f" (type $f))
  )
  "type not valid to be used as export")

;; validate within the type context
(assert_invalid
  (component
    (type (component
      (type $t (record (field "f" u32)))
      (export "f" (func (param "x" $t)))
    ))
  )
  "func not valid to be used as export")
(assert_invalid
  (component
    (type (component
      (type $t (record (field "f" u32)))
      (type $f (record (field "t" $t)))
      (export "f" (type (eq $f)))
    ))
  )
  "type not valid to be used as export")

;; instances of unexported types is ok
(component
  (type $t (record (field "f" u32)))
  (type $f (record (field "t" $t)))
  (instance
    (export "f" (type $f))
  )
)
;; .. but exporting them is not
(assert_invalid
  (component
    (type $t (record (field "f" u32)))
    (type $f (record (field "t" $t)))
    (instance $i
      (export "f" (type $f))
    )
    (export "i" (instance $i))
  )
  "instance not valid to be used as export")

;; Can't export a lifted function with unexported types
(assert_invalid
  (component
    (type $t (record (field "f" u32)))
    (type $f (record (field "t" $t)))

    (core module $m (func $f (export "f") (param i32)))
    (core instance $i (instantiate $m))
    (func $f (param "f" $f) (canon lift (core func $i "f")))
    (export "f" (func $f))
  )
  "func not valid to be used as export")

;; Unexported instances don't work
(assert_invalid
  (component
    (type $t' (record (field "f" u32)))
    (instance $i
      (export "t" (type $t'))
    )
    (alias export $i "t" (type $t))

    (core module $m (func $f (export "f") (param i32)))
    (core instance $i (instantiate $m))
    (func $f (param "f" $t) (canon lift (core func $i "f")))
    (export "f" (func $f))
  )
  "func not valid to be used as export")

;; Even through a component it doesn't work
(assert_invalid
  (component
    (component $C
      (type $t (record (field "f" u32)))
      (export "t" (type $t))
    )
    (instance $i (instantiate $C))
    (alias export $i "t" (type $t))

    (core module $m (func $f (export "f") (param i32)))
    (core instance $i (instantiate $m))
    (func $f (param "f" $t) (canon lift (core func $i "f")))
    (export "f" (func $f))
  )
  "func not valid to be used as export")

;; through exported instances is ok though
(component
  (type $t' (record (field "f" u32)))
  (instance $i'
    (export "t" (type $t'))
  )
  (export $i "i" (instance $i'))
  (alias export $i "t" (type $t))

  (core module $m (func $f (export "f") (param i32)))
  (core instance $i (instantiate $m))
  (func $f (param "f" $t) (canon lift (core func $i "f")))
  (export "f" (func $f))
)
(component
  (component $C
    (type $t (record (field "f" u32)))
    (export "t" (type $t))
  )
  (instance $i' (instantiate $C))
  (export $i "i" (instance $i'))
  (alias export $i "t" (type $t))

  (core module $m (func $f (export "f") (param i32)))
  (core instance $i (instantiate $m))
  (func $f (param "f" $t) (canon lift (core func $i "f")))
  (export "f" (func $f))
)

;; a type-ascribed export which is otherwise invalid can become valid
(component
  (type $t (record (field "f" u32)))

  (core module $m (func (export "f") (param i32)))
  (core instance $i (instantiate $m))
  (func $f (param "x" $t) (canon lift (core func $i "f")))

  (export $t' "t" (type $t))
  (export "f" (func $f) (func (param "x" $t')))
)

;; imports can't reference exports
(assert_invalid
  (component
    (type $t1 (record (field "f" u32)))
    (export $t2 "t1" (type $t1))
    (import "i" (func (result $t2)))
  )
  "func not valid to be used as import")

;; exports can reference imports
(component
  (type $t1 (record (field "f" u32)))
  (import "t1" (type $t2 (eq $t1)))
  (export "e-t1" (type $t2))
)
(component
  (type $t1 (record (field "f" u32)))
  (import "t1" (type $t2 (eq $t1)))
  (import "i" (func $f (result $t2)))

  (export "e-i" (func $f))
)

;; outer aliases don't work for imports/exports
(assert_invalid
  (component
    (type $t1 (record (field "f" u32)))
    (import "t1" (type $t2 (eq $t1)))
    (component
      (import "i" (func $f (result $t2)))
    )
  )
  "func not valid to be used as import")
(assert_invalid
  (component
    (type $t1 (record (field "f" u32)))
    (export $t2 "t1" (type $t1))
    (component
      (core module $m (func (export "f") (result i32) unreachable))
      (core instance $i (instantiate $m))
      (func $f (export "i") (result $t2) (canon lift (core func $i "f")))
    )
  )
  "func not valid to be used as export")

;; outer aliases work for components, modules, and resources
(component
  (type $c (component))
  (type (component
    (import "c" (component (type $c)))
  ))
  (component
    (import "c" (component (type $c)))
  )
  (type $i (instance))
  (type (component
    (import "c" (instance (type $i)))
  ))

  (type $r (resource (rep i32)))
  (type (component
    (import "r" (type (eq $r)))
  ))
)

;; reexport of an import is fine
(component
  (import "r" (func $r))
  (export "r2" (func $r))
)
(component
  (type $t (record (field "f" u32)))
  (import "r" (type $r (eq $t)))
  (export "r2" (type $r))
)
(component
  (import "r" (instance $r))
  (export "r2" (instance $r))
)
(component
  (import "r" (type $r (sub resource)))
  (export "r2" (type $r))
)

;; bag of exports cannot be exported by carrying through context that's not
;; otherwise exported
(assert_invalid
  (component
    (component $A
      (type $t (record (field "f" u32)))
      (export $t2 "t" (type $t))
      (core module $m (func (export "f") (result i32) unreachable))
      (core instance $i (instantiate $m))
      (func $f (result $t2) (canon lift (core func $i "f")))

      (instance (export "i")
        (export "f" (func $f))
      )
    )

    (instance $a (instantiate $A))
    ;; this component only exports `f`, not the record type that is the result
    ;; of `f`, so it should be invalid.
    (export "a" (instance $a "i"))
  )
  "instance not valid to be used as export")

;; instance types can be "temporarily invalid", but not if they're attached
;; to a concrete component
(component
  (type (instance
    (type $t (record (field "f" u32)))
    (export "f" (func (param "x" $t)))
  ))
)
(assert_invalid
  (component
    (type $i (instance
      (type $t (record (field "f" u32)))
      (type $f (record (field "t" $t)))
      (export "f" (type (eq $f)))
    ))
    (import "f" (instance (type $i)))
  )
  "instance not valid to be used as import")

;; allow for one import to refer to another
(component $C
  (import "foo" (instance $i
    (type $baz' (record (field "f" u32)))
    (export $baz "baz" (type (eq $baz')))
    (type $bar' (record (field "baz" $baz)))
    (export $bar "bar" (type (eq $bar')))
  ))
  (alias export $i "bar" (type $bar))
  (import "bar" (instance
    (alias outer $C $bar (type $bar'))
    (export $bar "bar" (type (eq $bar')))
    (export $f "a" (func (result $bar)))
  ))
)

;; allow for one import to refer to another
(component
  (type $r' (record (field "f" u32)))
  (import "r" (type $r (eq $r')))
  (component $C
    (type $r' (record (field "f" u32)))
    (import "r" (type $r (eq $r')))
    (type $r2' (record (field "r" $r)))
    (export "r2" (type $r2'))
  )
  (instance $c (instantiate $C (with "r" (type $r))))
  (export "r2" (type $c "r2"))
)

;; types are validated when they are exported
(assert_invalid
  (component
    (type $i (instance
      (type $t (record (field "f" u32)))
      (type $f (record (field "t" $t)))
      (export "f" (type (eq $f)))
    ))
    (import "f" (type (eq $i)))
  )
  "type not valid to be used as import")
(assert_invalid
  (component
    (type $i (instance
      (type $t (record (field "f" u32)))
      (type $f (record (field "t" $t)))
      (export "f" (type (eq $f)))
    ))
    (export "f" (type $i))
  )
  "type not valid to be used as export")

(assert_invalid
  (component
    (type $t (record (field "f" u32)))
    (type $f (func (result $t)))
    (import "f" (type (eq $f)))
  )
  "type not valid to be used as import")
(assert_invalid
  (component
    (type $t (record (field "f" u32)))
    (type $f (func (result $t)))
    (export "f" (type $f))
  )
  "type not valid to be used as export")

(component
  (type (;0;)
    (instance
      (type (;0;) (enum "qux"))
      (export (;1;) "baz" (type (eq 0)))
      (type (;2;) (record (field "bar" 1) ))
      (export (;3;) "foo" (type (eq 2)))
    )
  )
  (import (interface "demo:component/types") (instance (;0;) (type 0)))
  (component
    (type (;0;)
      (instance
        (type (;0;) (enum "qux"))
        (export (;1;) "baz" (type (eq 0)))
        (type (;2;) (record (field "bar" 1) ))
        (export (;3;) "foo" (type (eq 2)))
      )
    )
    (import (interface "demo:component/types") (instance (;0;) (type 0)))
    (component (;0;)
      (type (;0;) (enum "qux"))
      (import "import-type-baz" (type (;1;) (eq 0)))
      (type (;2;) (record (field "bar" 1) ))
      (import "import-type-bar" (type (;3;) (eq 2)))
      (export (;4;) "foo" (type 3))
    )
    (instance (;1;) (instantiate 0
        (with "import-type-baz" (type 0 "baz"))
        (with "import-type-bar" (type 0 "foo"))
      )
    )
    (export (;0;) (interface "demo:component/types") (instance 1))
  )
  (instance (instantiate 0 (with "demo:component/types" (instance 0))))
  (export   (interface "demo:component/types") (instance 1 "demo:component/types"))
)
