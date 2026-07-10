;; RUN: WAST_STRICT_COMPONENT_INDICES=0 wast --assert default --snapshot tests/snapshots % -f cm-async,cm-threading,shared-everything-threads,gc,cm-gc

;; legacy `(memory $i "name")` and `(realloc (func ...))` in canonopts of
;; `canon lower`
(component
  (import "f" (func $f (param "x" string)))
  (core module $libc
    (memory (export "mem") 1)
    (func (export "realloc") (param i32 i32 i32 i32) (result i32) unreachable))
  (core instance $libc (instantiate $libc))
  (alias core export $libc "realloc" (core func $realloc))
  (core func (canon lower (func $f)
    (memory $libc "mem")
    (realloc (func $libc "realloc"))))
  (core func (canon lower (func $f)
    (memory $libc "mem")
    (realloc (func $realloc))))
  (core func (canon lower (func $f)
    (memory $libc "mem")
    (realloc (func 0))))
)

;; legacy `(memory $i "name")`, `(realloc (func ...))` and
;; `(post-return (func ...))` in canonopts of `canon lift`
(component
  (core module $m
    (memory (export "mem") 1)
    (func (export "realloc") (param i32 i32 i32 i32) (result i32) unreachable)
    (func (export "run") (param i32 i32) (result i32) unreachable)
    (func (export "post") (param i32)))
  (core instance $m (instantiate $m))
  (func (export "run") (param "s" string) (result string)
    (canon lift (core func $m "run")
      (memory $m "mem")
      (realloc (func $m "realloc"))
      (post-return (func $m "post"))))
)

;; legacy `(callback (func ...))`
(component
  (core module $m
    (func (export "cb") (param i32 i32 i32) (result i32) unreachable)
    (func (export "f") (result i32) unreachable))
  (core instance $m (instantiate $m))
  (func async
    (canon lift (core func $m "f") async (callback (func $m "cb"))))
)

;; legacy `(dtor (func ...))` in resource types
(component
  (core module $m (func (export "dtor") (param i32)))
  (core instance $m (instantiate $m))
  (alias core export $m "dtor" (core func $dtor))
  (type $R1 (resource (rep i32) (dtor (func $dtor))))
  (type $R2 (resource (rep i32) (dtor (func 0))))
  (type $R3 (resource (rep i32) (dtor (func $m "dtor"))))
)

;; legacy `(table ...)` in `thread.new-indirect`
(component
  (core type $start (func (param $context i32)))
  (core module $libc (table (export "start-table") 1 (ref null func)))
  (core instance $libc (instantiate $libc))
  (alias core export $libc "start-table" (core table $tbl))
  (core func (canon thread.new-indirect $start (table $libc "start-table")))
  (core func (canon thread.new-indirect $start (table $tbl)))
  (core func (canon thread.new-indirect $start (table 0)))
)

;; legacy `(table ...)` in `thread.spawn-indirect`
(component
  (core type $start (shared (func (param $context i32))))
  (core module $libc (table (export "start-table") shared 1 (ref null (shared func))))
  (core instance $libc (instantiate $libc))
  (core func (canon thread.spawn-indirect $start (table $libc "start-table")))
)

;; legacy `(memory $i "name")` in `waitable-set.wait` and `waitable-set.poll`
(component
  (core module $libc (memory (export "memory") 1))
  (core instance $libc (instantiate $libc))
  (core func (canon waitable-set.wait (memory $libc "memory")))
  (core func (canon waitable-set.poll (memory $libc "memory")))
)

;; legacy `(core-type (type ...))`
(component
  (core type $ty (func (param externref)))
  (import "i" (instance $i
                        (export "r" (type $resource (sub resource)))
                        (export "f" (func (param "x" (own $resource))))))
  (core func (canon lower (func $i "f") gc string-encoding=utf8 (core-type (type $ty))))
)
