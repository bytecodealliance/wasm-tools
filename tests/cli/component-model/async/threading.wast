;; RUN: wast --assert default --snapshot tests/snapshots % -f cm-async,cm-threading

;; context.{get,set} 1
(component
  (core func $get1 (canon context.get i32 1))
  (core func $set1 (canon context.set i32 1))

  (core module $m
    (import "" "get1" (func (result i32)))
    (import "" "set1" (func (param i32)))
  )
  (core instance (instantiate $m
    (with "" (instance
      (export "get1" (func $get1))
      (export "set1" (func $set1))
    ))
  ))
)

(assert_invalid
  (component
    (core func (canon context.get i32 2)))
  "immediate must be zero or one: 2")
(assert_invalid
  (component
    (core func (canon context.set i32 2)))
  "immediate must be zero or one: 2")
(assert_invalid
  (component
    (core func (canon context.get i32 100)))
  "immediate must be zero or one: 100")
(assert_invalid
  (component
    (core func (canon context.set i32 100)))
  "immediate must be zero or one: 100")

(assert_invalid
  (component
    (core module $m (import "" "" (func (param i32) (result i32))))
    (core func $f (canon context.get i32 1))
    (core instance $i (instantiate $m (with "" (instance (export "" (func $f))))))
  )
  "found:    (func (result i32))")
(assert_invalid
  (component
    (core module $m (import "" "" (func (param i32) (result i32))))
    (core func $f (canon context.set i32 1))
    (core instance $i (instantiate $m (with "" (instance (export "" (func $f))))))
  )
  "found:    (func (param i32))")

;; thread.new-indirect
(component
  (core type $start (func (param $context i32)))
  (core module $libc (table (export "start-table") 1 (ref null func)))
  (core instance $libc (instantiate $libc))
  (core func $new-indirect (canon thread.new-indirect $start (table $libc "start-table")))
)

(component
  (core type $start (func (param $context i32)))
  (core module $libc (table (export "start-table") 1 (ref null func)))
  (core instance $libc (instantiate $libc))
  (core func $new-indirect (canon thread.new-indirect $start (table $libc "start-table")))

  (core module $m
    (type $new-indirect-ty (func (param i32) (param i32) (result i32)))
    (import "" "thread.new-indirect" (func (type $new-indirect-ty)))
  )

  (core instance (instantiate $m
    (with "" (instance
      (export "thread.new-indirect" (func $new-indirect))
    ))
  ))
)

(assert_invalid
  (component
    (core type $start (func (param i32)))
    ;; Refer to a non-existent table type (i.e., 0); validation
    ;; for `thread.new-indirect` happens first.
    (core func $new-indirect (canon thread.new-indirect $start (table 0)))
  )
  "unknown table 0: table index out of bounds"
)

(assert_invalid
  (component
    (core type $start (func))
    (core module $libc (table (export "start-table") 1 (ref null func)))
    (core instance $libc (instantiate $libc))
    (core func $new-indirect (canon thread.new-indirect $start (table $libc "start-table")))
  )
  "start function must take a single `i32` argument"
)

;; thead.index
(component
  (core module $m
    (import "" "thread.index" (func $thread.index (result i32)))
  )
  (core func $thread.index (canon thread.index))
  (core instance $i (instantiate $m (with "" (instance (export "thread.index" (func $thread.index))))))
)

;; thread.index; incorrect type
(assert_invalid
  (component
    (core module $m
      (import "" "thread.index" (func $thread.index (param i32) (result i32)))
    )
    (core func $thread.index (canon thread.index))
    (core instance $i (instantiate $m (with "" (instance (export "thread.index" (func $thread.index))))))
  )
  "type mismatch for export `thread.index` of module instantiation argument ``"
)

;; thead.resume-later
(component
  (core module $m
    (import "" "thread.resume-later" (func $thread.resume-later (param i32)))
  )
  (core func $thread.resume-later (canon thread.resume-later))
  (core instance $i (instantiate $m (with "" (instance (export "thread.resume-later" (func $thread.resume-later))))))
)

;; thread.resume-later; incorrect type
(assert_invalid
  (component
    (core module $m
      (import "" "thread.resume-later" (func $thread.resume-later (param i32) (result i32)))
    )
    (core func $thread.resume-later (canon thread.resume-later))
    (core instance $i (instantiate $m (with "" (instance (export "thread.resume-later" (func $thread.resume-later))))))
  )
  "type mismatch for export `thread.resume-later` of module instantiation argument ``"
)

;; thead.suspend
(component
  (core module $m
    (import "" "thread.suspend" (func $thread.suspend (result i32)))
  )
  (core func $thread.suspend (canon thread.suspend))
  (core instance $i (instantiate $m (with "" (instance (export "thread.suspend" (func $thread.suspend))))))
)

;; thread.suspend; incorrect type
(assert_invalid
  (component
    (core module $m
      (import "" "thread.suspend" (func $thread.suspend (param i32) (result i32)))
    )
    (core func $thread.suspend (canon thread.suspend))
    (core instance $i (instantiate $m (with "" (instance (export "thread.suspend" (func $thread.suspend))))))
  )
  "type mismatch for export `thread.suspend` of module instantiation argument ``"
)

;; thead.suspend-then-resume
(component
  (core module $m (import "" "" (func (param i32) (result i32))))
  (core func $f (canon thread.suspend-then-resume))
  (core instance (instantiate $m (with "" (instance (export "" (func $f))))))
)

;; thread.suspend-then-resume; incorrect type
(assert_invalid
  (component
    (core module $m (import "" "" (func (param i32))))
    (core func $f (canon thread.suspend))
    (core instance (instantiate $m (with "" (instance (export "" (func $f))))))
  )
  "type mismatch for export `` of module instantiation argument ``"
)

;; thead.yield-then-resume
(component
  (core module $m (import "" "" (func (param i32) (result i32))))
  (core func $f (canon thread.yield-then-resume))
  (core instance (instantiate $m (with "" (instance (export "" (func $f))))))
)

;; thread.yield-then-resume; incorrect type
(assert_invalid
  (component
    (core module $m (import "" "" (func (param i32))))
    (core func $f (canon thread.yield))
    (core instance (instantiate $m (with "" (instance (export "" (func $f))))))
  )
  "type mismatch for export `` of module instantiation argument ``"
)

;; thead.suspend-then-promote
(component
  (core module $m (import "" "" (func (param i32) (result i32))))
  (core func $f (canon thread.suspend-then-promote))
  (core instance (instantiate $m (with "" (instance (export "" (func $f))))))
)

;; thread.suspend-then-promote; incorrect type
(assert_invalid
  (component
    (core module $m (import "" "" (func (param i32))))
    (core func $f (canon thread.suspend))
    (core instance (instantiate $m (with "" (instance (export "" (func $f))))))
  )
  "type mismatch for export `` of module instantiation argument ``"
)

;; thead.yield-then-promote
(component
  (core module $m (import "" "" (func (param i32) (result i32))))
  (core func $f (canon thread.yield-then-promote))
  (core instance (instantiate $m (with "" (instance (export "" (func $f))))))
)

;; thread.yield-then-promote; incorrect type
(assert_invalid
  (component
    (core module $m (import "" "" (func (param i32))))
    (core func $f (canon thread.yield))
    (core instance (instantiate $m (with "" (instance (export "" (func $f))))))
  )
  "type mismatch for export `` of module instantiation argument ``"
)

;; different forms of canonical intrinsics

(component
  (core module $m
    (table (export "start-table") 1 (ref null func))
  )
  (core instance $i (instantiate $m))
  (alias core export $i "start-table" (core table $start-table))

  (core func (canon context.get i32 1))
  (canon context.get i32 1 (core func))
  (core func (canon context.set i32 1))
  (canon context.set i32 1 (core func))

  (core type $start (func (param i32)))
  (core func (canon thread.new-indirect $start (table $start-table)))
  (canon thread.new-indirect $start (table $start-table) (core func))

  (core func (canon thread.resume-later))
  (canon thread.resume-later (core func))

  (core func (canon thread.suspend))
  (canon thread.suspend (core func))
  (core func (canon thread.suspend cancellable))
  (canon thread.suspend cancellable (core func))

  (core func (canon thread.suspend-then-resume))
  (canon thread.suspend-then-resume (core func))
  (core func (canon thread.suspend-then-resume cancellable))
  (canon thread.suspend-then-resume cancellable (core func))

  (core func (canon thread.yield-then-resume))
  (canon thread.yield-then-resume (core func))
  (core func (canon thread.yield-then-resume cancellable))
  (canon thread.yield-then-resume cancellable (core func))

  (core func (canon thread.suspend-then-promote))
  (canon thread.suspend-then-promote (core func))
  (core func (canon thread.suspend-then-promote cancellable))
  (canon thread.suspend-then-promote cancellable (core func))

  (core func (canon thread.yield-then-promote))
  (canon thread.yield-then-promote (core func))
  (core func (canon thread.yield-then-promote cancellable))
  (canon thread.yield-then-promote cancellable (core func))
)

(component
  (canon task.return (result (stream u8)) (core func))
)
