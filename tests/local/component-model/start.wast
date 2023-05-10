
(assert_invalid
  (component
    (import "a" (func $f (param "p1" string)))
    (start $f)
  )
  "start function requires 1 arguments")

(assert_invalid
  (component
    (import "a" (func $f (param "p" string)))
    (import "b" (value $v string))
    (start $f (value $v) (value $v))
  )
  "start function requires 1 arguments")

(assert_invalid
  (component
    (import "a" (func $f (param "p1" string) (param "p2" string)))
    (import "b" (value $v string))
    (start $f (value $v) (value $v))
  )
  "cannot be used more than once")

(assert_invalid
  (component
    (import "a" (func $f (param "x" string) (param "y" string)))
    (import "b" (value $v string))
    (import "c" (value $v2 u32))
    (start $f (value $v) (value $v2))
  )
  "type mismatch for component start function argument 1")

(component
  (import "a" (func $f (param "z" string) (param "a" string)))
  (import "b" (value $v string))
  (import "c" (value $v2 string))
  (start $f (value $v) (value $v2))
)

(component
  (import "a" (func $f (result string)))
  (start $f (result (value $a)))
  (export "b" (value $a))
)

(component
  (import "a" (func $f (param "a" string) (param "b" string) (result "c" s32) (result "d" s32)))
  (import "b" (value $v string))
  (import "c" (value $v2 string))
  (start $f (value $v) (value $v2) (result (value $c)) (result (value $d)))
  (export "d" (value $c))
  (export "e" (value $d))
)

(assert_invalid
  (component
    (import "a" (func $f (param "a" string) (param "b" string) (result "c" s32) (result "d" s32)))
    (import "b" (value $v string))
    (import "c" (value $v2 string))
    (start $f (value $v) (value $v2) (result (value $c)))
    (export "a" (value $c))
  )
  "component start function has a result count of 1 but the function type has a result count of 2"
)

(assert_invalid
  (component
    (import "a" (func $f))
    (start $f)
    (start $f)
  )
  "cannot have more than one start")

(assert_invalid
  (component binary
    "\00asm" "\0d\00\01\00"   ;; component header

    "\07\05"          ;; type section, 5 bytes large
    "\01"             ;; 1 count
    "\40"             ;; function
    "\00"             ;; parameters, 0 count
    "\01\00"          ;; results, named, 0 count

    "\0a\06"          ;; import section, 6 bytes large
    "\01"             ;; 1 count
    "\00\01a"         ;; name = "a"
    "\01\00"          ;; type = func ($type 0)

    "\09\06"          ;; start section, 6 bytes large
    "\00"             ;; function 0
    "\00"             ;; no arguments
    "\ff\ff\ff\00"    ;; tons of results
  )
  "start function results size is out of bounds")

(assert_invalid
  (component binary
    "\00asm" "\0d\00\01\00"   ;; component header

    "\07\05"          ;; type section, 5 bytes large
    "\01"             ;; 1 count
    "\40"             ;; function
    "\00"             ;; parameters, 0 count
    "\01\00"          ;; results, named, 0 count

    "\0a\06"          ;; import section, 6 bytes large
    "\01"             ;; 1 count
    "\00\01a"         ;; name = "a"
    "\01\00"          ;; type = func ($type 0)

    "\09\04"          ;; start section, 4 bytes large
    "\00"             ;; function 0
    "\00"             ;; no arguments
    "\00"             ;; no results
    "\ff"             ;; trailing garbage byte
  )
  "unexpected content in the component start section")
