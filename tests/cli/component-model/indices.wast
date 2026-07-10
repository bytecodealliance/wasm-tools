;; RUN: wast --assert default --snapshot tests/snapshots % -f cm-threading
;; RUN[lenient]: WAST_STRICT_COMPONENT_INDICES=0 wast --assert default --snapshot tests/snapshots % -f cm-threading

;; test component instantiate parsing
(component
  (component
    (component $C (import "x" (func)))
    (import "f" (func $f))
    (instance (instantiate $C (with "x" (func $f))))
    (instance (instantiate $C (with "x" (func 0))))
    (import "i" (instance $i (export "f" (func))))
    (instance (instantiate $C (with "x" (func $i "f"))))
    (import "j" (instance $j
      (export "a" (instance
        (export "b" (instance
          (export "f" (func))))))))
    (instance (instantiate $C (with "x" (func $j "a" "b" "f"))))
  )

  (component
    (component $C (import "x" (instance)))
    (import "i" (instance $i))
    (instance (instantiate $C (with "x" (instance $i))))
    (import "j" (instance $j (export "k" (instance))))
    (instance (instantiate $C (with "x" (instance $j "k"))))
  )

  (component
    (component $C (import "x" (component)))
    (import "c" (component $c))
    (instance (instantiate $C (with "x" (component $c))))
    (import "i" (instance $i (export "c" (component))))
    (instance (instantiate $C (with "x" (component $i "c"))))
  )

  (component
    (component $C (import "x" (type (sub resource))))
    (import "t" (type $t (sub resource)))
    (instance (instantiate $C (with "x" (type $t))))
    (instance (instantiate $C (with "x" (type 0))))
    (import "i" (instance $i (export "t" (type (sub resource)))))
    (instance (instantiate $C (with "x" (type $i "t"))))
  )

  (component
    (component $C (import "x" (core module)))
    (import "m" (core module $m))
    (instance (instantiate $C (with "x" (core module $m))))
    (instance (instantiate $C (with "x" (core module 0))))
    (import "i" (instance $i (export "m" (core module))))
    (instance (instantiate $C (with "x" (core module $i "m"))))
    (import "j" (instance $j
      (export "a" (instance
        (export "m" (core module))))))
    (instance (instantiate $C (with "x" (core module $j "a" "m"))))
  )
)

;; test core instantiate parsing
(component
  (component
    (core module $M (import "" "" (func)))
    (core module $E (func (export "")))
    (core instance $e (instantiate $E))
    (core instance (instantiate $M (with "" (instance $e))))
    (core instance (instantiate $M (with "" (instance 0))))
    (core instance (instantiate $M
      (with "" (instance (export "" (func $e ""))))))
  )
)

;; test canon lower parsing
(component
  (component
    (import "f" (func $f))
    (import "i" (instance $i (export "f" (func))))
    (import "j" (instance $j
      (export "n" (instance
        (export "f" (func))))))
    (canon lower (func $f) (core func))
    (canon lower (func $i "f") (core func))
    (canon lower (func 0) (core func))
    (canon lower (func $j "n" "f") (core func))
    (core func (canon lower (func 0)))
    (core func (canon lower (func $f)))
    (core func (canon lower (func $i "f")))
    (core func (canon lower (func $j "n" "f")))
  )

  (component
    (import "f" (func $f (param "x" string)))
    (core module $L
      (memory (export "mem") 1)
      (func (export "realloc") (param i32 i32 i32 i32) (result i32) unreachable))
    (core instance $libc (instantiate $L))
    (alias core export $libc "mem" (core memory $mem))
    (alias core export $libc "realloc" (core func $realloc))
    (canon lower (func $f)
      (memory 0) (realloc 0)
      (core func))
    (canon lower (func $f)
      (memory $mem) (realloc $realloc)
      (core func))
    (canon lower (func 0)
      (memory (core memory 0)) (realloc 0)
      (core func))
    (canon lower (func 0)
      (memory 0) (realloc (core func 0))
      (core func))
    (canon lower (func 0)
      (memory (core memory 0)) (realloc (core func 0))
      (core func))
    (canon lower (func $f)
      (memory 0) (realloc (core func $realloc))
      (core func))
    (canon lower (func $f)
      (memory (core memory $mem)) (realloc 0)
      (core func))
    (canon lower (func $f)
      (memory (core memory $mem)) (realloc (core func $realloc))
      (core func))
    (canon lower (func $f)
      (memory (core memory $libc "mem")) (realloc (core func $libc "realloc"))
      (core func))
    (core func (canon lower (func $f)
      (memory 0) (realloc 0)))
    (core func (canon lower (func $f)
      (memory $mem) (realloc $realloc)))
    (core func (canon lower (func 0)
      (memory (core memory 0))
      (realloc (core func 0))))
    (core func (canon lower (func $f)
      (memory (core memory $mem)) (realloc (core func $realloc))))
    (core func (canon lower (func $f)
      (memory (core memory $libc "mem")) (realloc (core func $libc "realloc"))))
  )
)

;; test canon lift parsing
(component
  (component
    (core module $M (func (export "f")))
    (core instance $m (instantiate $M))
    (alias core export $m "f" (core func $f))
    (canon lift (core func $f) (func))
    (canon lift (core func 0) (func))
    (canon lift (core func $m "f") (func))
    (func (canon lift (core func $f)))
    (func (canon lift (core func 0)))
    (func (canon lift (core func $m "f")))
  )

  (component
    (core module $M
      (memory (export "mem") 1)
      (func (export "realloc") (param i32 i32 i32 i32) (result i32) unreachable)
      (func (export "run") (param i32 i32) (result i32) unreachable)
      (func (export "post") (param i32))
    )
    (core instance $m (instantiate $M))
    (alias core export $m "mem" (core memory $mem))
    (alias core export $m "realloc" (core func $realloc))
    (alias core export $m "post" (core func $post))
    (canon lift (core func $m "run")
      (memory 0)
      (realloc 0)
      (post-return 1)
      (func (param "s" string) (result string)))
    (canon lift (core func $m "run")
      (memory $mem)
      (realloc $realloc)
      (post-return $post)
      (func (param "s" string) (result string)))
    (canon lift (core func $m "run")
      (memory (core memory $m "mem"))
      (realloc $realloc)
      (post-return $post)
      (func (param "s" string) (result string)))
    (canon lift (core func $m "run")
      (memory $mem)
      (realloc (core func $m "realloc"))
      (post-return $post)
      (func (param "s" string) (result string)))
    (canon lift (core func $m "run")
      (memory $mem)
      (realloc $realloc)
      (post-return (core func $m "post"))
      (func (param "s" string) (result string)))
    (canon lift (core func $m "run")
      (memory (core memory $m "mem"))
      (realloc (core func $m "realloc"))
      (post-return (core func $m "post"))
      (func (param "s" string) (result string)))
  )
)

;; test resource type parsing
(component
  (component
    (core module $M (func (export "dtor") (param i32)))
    (core instance $m (instantiate $M))
    (alias core export $m "dtor" (core func $dtor))
    (type $R1 (resource (rep i32) (dtor $dtor)))
    (type $R2 (resource (rep i32) (dtor 0)))
    (type $R3 (resource (rep i32) (dtor (core func $dtor))))
    (type $R4 (resource (rep i32) (dtor (core func $m "dtor"))))
  )
)

;; test waitable-set.wait/poll parsing
(component
  (component
    (core module $M (memory (export "mem") 1))
    (core instance $m (instantiate $M))
    (alias core export $m "mem" (core memory $mem))
    (canon waitable-set.wait (memory $mem) (core func))
    (canon waitable-set.wait (memory 0) (core func))
    (canon waitable-set.wait (memory (core memory $mem)) (core func))
    (canon waitable-set.wait (memory (core memory $m "mem")) (core func))
    (canon waitable-set.poll (memory (core memory $mem)) (core func))
    (canon waitable-set.poll (memory (core memory $m "mem")) (core func))
  )
)

;; test thread.new-indirect
(component
  (component
    (core type $ft (func (param i32)))
    (core module $M (table (export "tbl") 1 funcref))
    (core instance $m (instantiate $M))
    (alias core export $m "tbl" (core table $tbl))
    (canon thread.new-indirect 0 0 (core func))
    (canon thread.new-indirect $ft $tbl (core func))
    (canon thread.new-indirect $ft (core table $tbl) (core func))
    (canon thread.new-indirect (core type $ft) $tbl (core func))
    (canon thread.new-indirect (core type $ft) (core table $tbl) (core func))
    (canon thread.new-indirect (core type $ft) (core table $m "tbl") (core func))
  )
)

;; test component-level typeidx in canon built-ins
(component
  (component
    (type $T (future u32))
    (import "i" (instance $i (export "T" (type (eq $T)))))
    (canon future.new $T (core func))
    (canon future.new 0 (core func))
    (canon future.new (type $T) (core func))
    (canon future.new (type 0) (core func))
    (canon future.new (type $i "T") (core func))
  )
)
