(component
  (import "a" (core module $m))
  (core instance $a (instantiate $m))
)

(component
  (import "a" (func $i))
  (import "b" (component $c (import "a" (func))))
  (instance (instantiate $c (with "a" (func $i))))
)

(component
  (import "a" (value $i string))
  (import "b" (component $c (import "a" (value string))))
  (instance (instantiate $c (with "a" (value $i))))
)

(component
  (import "a" (component $i))
  (import "b" (component $c (import "a" (component))))
  (instance (instantiate $c (with "a" (component $i))))
)

(component
  (import "a" (core module $i))
  (import "b" (component $c (import "a" (core module))))
  (instance (instantiate $c (with "a" (core module $i))))
)

(component
  (import "a" (instance $i))
  (import "b" (component $c (import "a" (instance))))
  (instance (instantiate $c (with "a" (instance $i))))
)

(component
  (import "a" (core module $m
    (import "" "a" (func))
    (import "" "b" (global i32))
    (import "" "c" (table 1 funcref))
    (import "" "d" (memory 1))
  ))
  (import "b" (core module $m2
    (export "a" (func))
    (export "b" (global i32))
    (export "c" (table 1 funcref))
    (export "d" (memory 1))
  ))
  (core instance $x (instantiate $m2))
  (core instance (instantiate $m (with "" (instance $x))))
)

(component
  (import "a" (core module $m
    (import "" "d" (func))
    (import "" "c" (global i32))
    (import "" "b" (table 1 funcref))
    (import "" "a" (memory 1))
  ))
  (import "b" (core module $m2
    (export "a" (func))
    (export "b" (global i32))
    (export "c" (table 1 funcref))
    (export "d" (memory 1))
  ))
  (core instance $x (instantiate $m2))

  (core instance (instantiate $m (with "" (instance
    (export "d" (func $x "a"))
    (export "c" (global $x "b"))
    (export "b" (table $x "c"))
    (export "a" (memory $x "d"))
  ))))
)

(component
  (type $t string)
  (import "a" (value (type $t)))
  (component $c (import "a" (value string)) (export "b" (value 0)))
  (instance (instantiate $c (with "a" (value 0))))
)

(component
  (import "a" (component $m
    (import "a" (instance
      (export "a" (core module))
    ))
  ))
  (import "b" (component $m2
    (export "b" (core module))
  ))
  (instance $x (instantiate $m2))

  (instance (instantiate $m (with "a" (instance
    (export "a" (core module $x "b"))
  ))))
)

(component
  (import "a" (component $c
    (import "a" (core module))
    (import "b" (func))
    (import "c" (component))
    (import "d" (instance))
    (import "e" (value string))
  ))
  (core module $m (import "b"))
  (func $f (import "c"))
  (component $c2 (import "d"))
  (instance $i (import "e"))
  (import "f" (value $v string))

  (instance
    (instantiate $c
      (with "a" (core module $m))
      (with "b" (func $f))
      (with "c" (component $c2))
      (with "d" (instance $i))
      (with "e" (value $v))
    )
  )

  (core instance $c (instantiate $m))
  (core instance (instantiate $m))

  ;; inline exports/imports
  (type $empty (instance))
  (instance $d (import "g") (type $empty))
  (instance (import "h"))
  (instance (import "i")
    (export "x" (func)))
  (instance (export "j") (export "k") (import "x"))
)

(assert_invalid
  (component
    (core instance (instantiate 0))
  )
  "unknown module")
(assert_invalid
  (component
    (instance (instantiate 0))
  )
  "unknown component")
(assert_invalid
  (component
    (import "a" (core module))
    (core instance (instantiate 1))
  )
  "unknown module")

(component
  (import "a" (func $f))
  (import "b" (component $c))
  (instance (instantiate $c (with "a" (func $f))))
)
(assert_invalid
  (component
    (import "a" (core module $m (import "" "" (func))))
    (core instance (instantiate $m))
  )
  "missing module instantiation argument")
(assert_invalid
  (component
    (import "a" (component $m (import "a" (func))))
    (instance (instantiate $m))
  )
  "missing import named `a`")

(assert_invalid
  (component
    (import "a" (component $m
      (import "a" (func))
    ))
    (import "b" (component $c))
    (instance $i (instantiate $m (with "a" (component $c))))
  )
  "expected func, found component")

(assert_invalid
  (component
    (import "a" (component $m
      (import "a" (func))
    ))
    (import "b" (func $f (result string)))
    (instance $i (instantiate $m (with "a" (func $f))))
  )
  "expected 0 results, found 1")

(assert_invalid
  (component
    (import "a" (component $m
      (import "a" (func))
    ))
    (import "b" (func (param "i" string)))
    (instance $i (instantiate $m (with "a" (func 0))))
  )
  "expected 0 parameters, found 1")

(assert_invalid
  (component
    (import "a" (component $m
      (import "a" (core module
        (import "" "" (func))
      ))
    ))
    (import "b" (core module $i
      (import "" "" (global i32))
    ))
    (instance $i (instantiate $m (with "a" (core module $i))))
  )
  "type mismatch in import `::`")

(assert_invalid
  (component
    (import "a" (component $m
      (import "a" (core module))
    ))
    (import "b" (core module $i
      (import "" "foobar" (global i32))
    ))
    (instance $i (instantiate $m (with "a" (core module $i))))
  )
  "missing expected import `::foobar`")
(assert_invalid
  (component
    (import "a" (component $m
      (import "a" (core module (export "x" (func))))
    ))
    (import "b" (core module $i))
    (instance $i (instantiate $m (with "a" (core module $i))))
  )
  "missing expected export `x`")

;; it's ok to give a module with fewer imports
(component
  (import "a" (component $m
    (import "a" (core module
      (import "" "" (global i32))
      (import "" "f" (func))
    ))
  ))
  (import "b" (core module $i
    (import "" "" (global i32))
  ))
  (instance $i (instantiate $m (with "a" (core module $i))))
)

;; export subsets
(component
  (import "a" (component $m
    (import "a" (core module
      (export "" (func))
    ))
  ))
  (import "b" (core module $i
    (export "" (func))
    (export "a" (func))
  ))
  (instance $i (instantiate $m (with "a" (core module $i))))
)
(component
  (import "a" (component $m
    (import "a" (instance
      (export "a" (func))
    ))
  ))
  (import "b" (instance $i
    (export "a" (func))
    (export "b" (func))
  ))
  (instance (instantiate $m (with "a" (instance $i))))
)


;; ============================================================================
;; core wasm type checking

(assert_invalid
  (component
    (import "m1" (core module $m1 (import "" "" (func))))
    (import "m2" (core module $m2 (export "" (func (param i32)))))
    (core instance $i (instantiate $m2))
    (core instance (instantiate $m1 (with "" (instance $i))))
  )
  "expected: [] -> []")
(assert_invalid
  (component
    (import "m1" (core module $m1 (import "" "" (func))))
    (import "m2" (core module $m2 (export "" (func (result i32)))))
    (core instance $i (instantiate $m2))
    (core instance (instantiate $m1 (with "" (instance $i))))
  )
  "expected: [] -> []")
(assert_invalid
  (component
    (import "m1" (core module $m1 (import "" "" (global i32))))
    (import "m2" (core module $m2 (export "" (global i64))))
    (core instance $i (instantiate $m2))
    (core instance (instantiate $m1 (with "" (instance $i))))
  )
  "expected global type i32, found i64")
(assert_invalid
  (component
    (import "m1" (core module $m1 (import "" "" (table 1 funcref))))
    (import "m2" (core module $m2 (export "" (table 2 externref))))
    (core instance $i (instantiate $m2))
    (core instance (instantiate $m1 (with "" (instance $i))))
  )
  "expected table element type funcref, found externref")
(assert_invalid
  (component
    (import "m1" (core module $m1 (import "" "" (table 1 2 funcref))))
    (import "m2" (core module $m2 (export "" (table 2 funcref))))
    (core instance $i (instantiate $m2))
    (core instance (instantiate $m1 (with "" (instance $i))))
  )
  "mismatch in table limits")
(assert_invalid
  (component
    (import "m1" (core module $m1 (import "" "" (table 2 2 funcref))))
    (import "m2" (core module $m2 (export "" (table 1 funcref))))
    (core instance $i (instantiate $m2))
    (core instance (instantiate $m1 (with "" (instance $i))))
  )
  "mismatch in table limits")
(assert_invalid
  (component
    (import "m1" (core module $m1 (import "" "" (table 2 2 funcref))))
    (import "m2" (core module $m2 (export "" (table 2 3 funcref))))
    (core instance $i (instantiate $m2))
    (core instance (instantiate $m1 (with "" (instance $i))))
  )
  "mismatch in table limits")
(assert_invalid
  (component
    (import "m1" (core module $m1 (import "" "" (memory 1 2 shared))))
    (import "m2" (core module $m2 (export "" (memory 1))))
    (core instance $i (instantiate $m2))
    (core instance (instantiate $m1 (with "" (instance $i))))
  )
  "mismatch in the shared flag for memories")
(assert_invalid
  (component
    (import "m1" (core module $m1 (import "" "" (memory 1))))
    (import "m2" (core module $m2 (export "" (memory 0))))
    (core instance $i (instantiate $m2))
    (core instance (instantiate $m1 (with "" (instance $i))))
  )
  "mismatch in memory limits")
(assert_invalid
  (component
    (import "m1" (core module $m1 (export "g" (func))))
    (component $c
      (import "m" (core module (export "g" (global i32))))
    )
    (instance (instantiate $c (with "m" (core module $m1))))
  )
  "type mismatch in export `g`")

(assert_invalid
  (component
    (core instance (instantiate 0))
  )
  "unknown module")

(component
  (component $m
    (core module $sub (export "module")
      (func $f (export "") (result i32)
        i32.const 5))
  )
  (instance $a (instantiate $m))
  (alias export $a "module" (core module $sub))
  (core instance $b (instantiate $sub))

  (core module $final
    (import "" "" (func $b (result i32)))
    (func (export "get") (result i32)
      call $b))

  (core instance (instantiate $final (with "" (instance $b))))
)

(assert_invalid
  (component (instance $i (export "" (func 0))))
  "function index out of bounds")

(assert_invalid
  (component (instance $i (export "" (instance 0))))
  "index out of bounds")

(assert_invalid
  (component (instance $i (export "" (component 0))))
  "index out of bounds")

(assert_invalid
  (component (instance $i (export "" (instance 0))))
  "index out of bounds")

(assert_invalid
  (component (instance $i (export "" (core module 0))))
  "index out of bounds")

(assert_invalid
  (component (instance $i (export "" (value 0))))
  "index out of bounds")

(assert_invalid
  (component (core instance (export "" (func 0))))
  "index out of bounds")

(assert_invalid
  (component (core instance (export "" (table 0))))
  "index out of bounds")

(assert_invalid
  (component (core instance (export "" (global 0))))
  "index out of bounds")

(assert_invalid
  (component (core instance (export "" (memory 0))))
  "index out of bounds")

(assert_invalid
  (component
    (core module $m)
    (core instance $i (instantiate $m))
    (core instance (instantiate $m
      (with "" (instance $i))
      (with "" (instance $i))
    ))
  )
  "duplicate module instantiation argument named ``"
)

(assert_invalid
  (component
    (core module $m (func (export "")))
    (core instance $i (instantiate $m))
    (core instance (instantiate $m
      (with "" (instance $i))
      (with "" (instance $i))
    ))
  )
  "duplicate module instantiation argument named ``")

(assert_invalid
  (component
    (core module $m1 (func (export "")))
    (core module $m2 (import "" "" (global i32)))
    (core instance $i (instantiate $m1))
    (core instance (instantiate $m2
      (with "" (instance $i))
    ))
  )
  "expected global, found func")

(assert_invalid
  (component
    (component $m)
    (instance $i (instantiate $m))
    (instance (instantiate $m
      (with "a" (instance $i))
      (with "a" (instance $i))
    ))
  )
  "instantiation argument `a` conflicts with previous argument `a`")

(assert_invalid
  (component
    (component $c (import "a" (func)))
    (instance (instantiate $c
      (with "a" (component $c))
    ))
  )
  "expected func, found component")

(assert_invalid
  (component
    (component $c)
    (instance (instantiate $c
      (with "" (core module 0))
    ))
  )
  "index out of bounds")

(assert_invalid
  (component
    (component $c)
    (instance (instantiate $c
      (with "" (value 0))
    ))
  )
  "index out of bounds")

(assert_invalid
  (component
    (component $c)
    (instance (instantiate $c
      (with "" (instance 0))
    ))
  )
  "index out of bounds")

(assert_invalid
  (component
    (component $c)
    (instance (instantiate $c
      (with "" (func 0))
    ))
  )
  "index out of bounds")

(assert_invalid
  (component
    (component $c)
    (instance (instantiate $c
      (with "" (component 100))
    ))
  )
  "index out of bounds")

(assert_invalid
  (component
    (component $c)
    (instance
      (export "a" (component $c))
      (export "a" (component $c))
    )
  )
  "instance export name `a` conflicts with previous name `a`")

(component
  (import "a" (instance $i))
  (import "b" (func $f))
  (import "c" (component $c))
  (import "d" (core module $m))
  (import "e" (value $v string))
  (instance
    (export "a" (instance $i))
    (export "b" (func $f))
    (export "c" (component $c))
    (export "d" (core module $m))
    (export "e" (value $v))
  )
)

(component
  (core module $m
    (func (export "1"))
    (memory (export "2") 1)
    (table (export "3") 1 funcref)
    (global (export "4") i32 i32.const 0)
  )
  (core instance $i (instantiate $m))
  (core instance
    (export "a" (func $i "1"))
    (export "b" (memory $i "2"))
    (export "c" (table $i "3"))
    (export "d" (global $i "4"))
  )
)

(assert_invalid
  (component
    (core module $m (func (export "")))
    (core instance $i (instantiate $m))
    (core instance
      (export "" (func $i ""))
      (export "" (func $i ""))
    )
  )
  "export name `` already defined")

(assert_invalid
  (component
    (component $c)
    (instance $i (instantiate $c))
    (export "a" (instance $i "a"))
  )
  "no export named `a`")

(assert_invalid
  (component
    (export "a" (instance 100 "a"))
  )
  "index out of bounds")

(assert_invalid
  (component
    (import "a" (core module $libc
      (export "memory" (memory 1))
      (export "table" (table 0 funcref))
      (export "func" (func))
      (export "global" (global i32))
      (export "global mut" (global (mut i64)))
    ))
    (core instance $libc (instantiate $libc))
    (alias core export $libc "memory" (core memory $mem))
    (alias core export $libc "table" (core table $tbl))
    (alias core export $libc "func" (core func $func))
    (alias core export $libc "global" (core global $global))
    (alias core export $libc "global mut" (core global $global_mut))

    (import "x" (core module $needs_libc
      (import "" "memory" (memory 1))
      (import "" "table" (table 0 funcref))
      (import "" "func" (func))
      (import "" "global" (global i32))
      (import "" "global mut" (global (mut i64)))
    ))

    (core instance
      (instantiate $needs_libc
        (with "" (instance (export "memory" (memory $mem))))
      )
    )
  )
  "module instantiation argument `` does not export an item named `table`")

;; Ensure a type can be an instantiation argument
(component
  (type (tuple u32 u32))
  (import "a" (type (eq 0)))
  (component
    (type (tuple u32 u32))
    (import "a" (type (eq 0)))
  )
  (instance (instantiate 0
      (with "a" (type 1))
    )
  )
)

(assert_invalid
  (component
    (type $t (tuple string string))
    (import "a" (type $a (eq $t)))
    (component $c
      (type $t (tuple u32 u32))
      (import "a" (type (eq $t)))
    )
    (instance (instantiate $c
        (with "a" (type $a))
      )
    )
  )
  "expected primitive `u32` found primitive `string`")


;; subtyping for module imports reverses order of imports/exports for the
;; subtyping check
;;
;; Here `C` imports a module, and the module itself imports a table of min size
;; 1. A module import which imports a min-size table of 0, however, is valid to
;; supply for this since it'll already be given at least 1 anyway.
;;
;; Similarly for exports `C` imports a module that exports a table of at least
;; size 1. If it's given a module that exports a larger table that's ok too.
(component
  (core module $a
    (import "" "" (table 0 funcref))
    (table (export "x") 2 funcref)
  )
  (component $C
    (import "a" (core module
      (import "" "" (table 1 funcref))
      (export "x" (table 1 funcref))
    ))
  )
  (instance (instantiate $C (with "a" (core module $a))))
)

;; same as above but for memories
(component
  (core module $a1 (import "" "" (memory 0)))
  (core module $a2 (memory (export "x") 2))
  (component $C
    (import "a1" (core module (import "" "" (memory 1))))
    (import "a2" (core module (export "x" (memory 1))))
  )
  (instance (instantiate $C
    (with "a1" (core module $a1))
    (with "a2" (core module $a2))
  ))
)

(assert_invalid
  (component
    (import "x" (func $x (param "x" u32)))
    (import "y" (component $c
      (import "x" (func (param "y" u32)))
    ))

    (instance (instantiate $c (with "x" (func $x))))
  )
  "expected parameter named `y`, found `x`")
(assert_invalid
  (component
    (import "x" (func $x (param "x" u32)))
    (import "y" (component $c
      (import "x" (func (param "x" s32)))
    ))

    (instance (instantiate $c (with "x" (func $x))))
  )
  "type mismatch in function parameter `x`")
(assert_invalid
  (component
    (import "x" (func $x (result "x" u32)))
    (import "y" (component $c
      (import "x" (func (result "y" u32)))
    ))

    (instance (instantiate $c (with "x" (func $x))))
  )
  "mismatched result names")
(assert_invalid
  (component
    (import "x" (func $x (result "x" u32)))
    (import "y" (component $c
      (import "x" (func (result "x" s32)))
    ))

    (instance (instantiate $c (with "x" (func $x))))
  )
  "type mismatch with result type")

(assert_invalid
  (component
    (import "x" (instance $x (export "a" (func))))
    (import "y" (component $c
      (import "x" (instance $x (export "a" (component))))
    ))

    (instance (instantiate $c (with "x" (instance $x))))
  )
  "type mismatch in instance export `a`")

(assert_invalid
  (component
    (import "y" (component $c
      (type $t u32)
      (import "x" (type (eq $t)))
    ))

    (type $x (record (field "f" u32)))
    (instance (instantiate $c (with "x" (type $x))))
  )
  "expected primitive, found record")

(assert_invalid
  (component
    (import "y" (component $c
      (type $t (record (field "f" u32)))
      (import "x" (type (eq $t)))
    ))

    (type $x u32)
    (instance (instantiate $c (with "x" (type $x))))
  )
  "expected record, found u32")

(assert_invalid
  (component
    (import "y" (component $c
      (type $t (record (field "x" u32)))
      (import "x" (type (eq $t)))
    ))

    (type $f (tuple u8))
    (type $x (record (field "x" $f)))
    (instance (instantiate $c (with "x" (type $x))))
  )
  "expected u32, found tuple")

(assert_invalid
  (component
    (import "y" (component $c
      (type $f (option s32))
      (type $t (record (field "x" $f)))
      (import "x" (type (eq $t)))
    ))

    (type $x (record (field "x" u32)))
    (instance (instantiate $c (with "x" (type $x))))
  )
  "type mismatch in record field `x`")

(assert_invalid
  (component
    (import "y" (component $c
      (type $t (record (field "x" u32)))
      (import "x" (type (eq $t)))
    ))

    (type $x (record (field "y" u32) (field "z" u64)))
    (instance (instantiate $c (with "x" (type $x))))
  )
  "expected 1 fields, found 2")

(assert_invalid
  (component
    (import "y" (component $c
      (type $t (record (field "a" u32)))
      (import "x" (type (eq $t)))
    ))

    (type $x (record (field "b" u32)))
    (instance (instantiate $c (with "x" (type $x))))
  )
  "expected field name `a`, found `b`")

(assert_invalid
  (component
    (import "y" (component $c
      (type $t (variant (case "x" u32)))
      (import "x" (type (eq $t)))
    ))

    (type $x (variant (case "x" u32) (case "y" u32)))
    (instance (instantiate $c (with "x" (type $x))))
  )
  "expected 1 cases, found 2")

(assert_invalid
  (component
    (import "y" (component $c
      (type $t (variant (case "x" u32)))
      (import "x" (type (eq $t)))
    ))

    (type $x (variant (case "y" u32)))
    (instance (instantiate $c (with "x" (type $x))))
  )
  "expected case named `x`, found `y`")

(assert_invalid
  (component
    (import "y" (component $c
      (type $t (variant (case "x" u32)))
      (import "x" (type (eq $t)))
    ))

    (type $x (variant (case "x")))
    (instance (instantiate $c (with "x" (type $x))))
  )
  "expected case `x` to have a type, found none")

(assert_invalid
  (component
    (import "y" (component $c
      (type $t (variant (case "x")))
      (import "x" (type (eq $t)))
    ))

    (type $x (variant (case "x" u32)))
    (instance (instantiate $c (with "x" (type $x))))
  )
  "expected case `x` to have no type")

(assert_invalid
  (component
    (import "y" (component $c
      (type $t (variant (case "x" u32)))
      (import "x" (type (eq $t)))
    ))

    (type $x (variant (case "x" s32)))
    (instance (instantiate $c (with "x" (type $x))))
  )
  "type mismatch in variant case `x`")

(assert_invalid
  (component
    (import "y" (component $c
      (type $t (tuple u8))
      (import "x" (type (eq $t)))
    ))

    (type $x (tuple u32 u32))
    (instance (instantiate $c (with "x" (type $x))))
  )
  "expected 1 types, found 2")

(assert_invalid
  (component
    (import "y" (component $c
      (type $t (tuple u8))
      (import "x" (type (eq $t)))
    ))

    (type $x (tuple u16))
    (instance (instantiate $c (with "x" (type $x))))
  )
  "type mismatch in tuple field 0")

(assert_invalid
  (component
    (import "y" (component $c
      (type $t (flags "a"))
      (import "x" (type (eq $t)))
    ))

    (type $x (flags "x"))
    (instance (instantiate $c (with "x" (type $x))))
  )
  "mismatch in flags elements")

(assert_invalid
  (component
    (import "y" (component $c
      (type $t (enum "a"))
      (import "x" (type (eq $t)))
    ))

    (type $x (enum "x"))
    (instance (instantiate $c (with "x" (type $x))))
  )
  "mismatch in enum elements")

(assert_invalid
  (component
    (import "y" (component $c
      (type $t (result s32))
      (import "x" (type (eq $t)))
    ))

    (type $x (result u32))
    (instance (instantiate $c (with "x" (type $x))))
  )
  "type mismatch in ok variant")

(assert_invalid
  (component
    (import "y" (component $c
      (type $t (result (error s32)))
      (import "x" (type (eq $t)))
    ))

    (type $x (result (error u32)))
    (instance (instantiate $c (with "x" (type $x))))
  )
  "type mismatch in err variant")

(assert_invalid
  (component
    (import "y" (component $c
      (type $t (result))
      (import "x" (type (eq $t)))
    ))

    (type $x (result u32))
    (instance (instantiate $c (with "x" (type $x))))
  )
  "expected ok type to not be present")

(assert_invalid
  (component
    (import "y" (component $c
      (type $t (result u32))
      (import "x" (type (eq $t)))
    ))

    (type $x (result))
    (instance (instantiate $c (with "x" (type $x))))
  )
  "expected ok type, but found none")

(assert_invalid
  (component
    (import "y" (component $c
      (type $t (result))
      (import "x" (type (eq $t)))
    ))

    (type $x (result (error u32)))
    (instance (instantiate $c (with "x" (type $x))))
  )
  "expected err type to not be present")

(assert_invalid
  (component
    (import "y" (component $c
      (type $t (result (error u32)))
      (import "x" (type (eq $t)))
    ))

    (type $x (result))
    (instance (instantiate $c (with "x" (type $x))))
  )
  "expected err type, but found none")
