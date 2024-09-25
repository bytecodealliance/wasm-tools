;; dynamic lightweight threads via control/prompt

;; interface to control/prompt
(module $control
  (type $func (func))       ;; [] -> []
  (type $cont (cont $func)) ;; cont ([] -> [])

  (type $cont-func (func (param (ref $cont)))) ;; [cont ([] -> [])] -> []
  (type $cont-cont (cont $cont-func))          ;; cont ([cont ([] -> [])] -> [])

  ;; Implementation of a generic delimited control operator using
  ;; effect handlers.
  ;;
  ;; For lightweight threads we have no payload. More general types
  ;; for control and prompt are:
  ;;
  ;;   control : ([cont ([ta*] -> [tr*])] -> [tr*]) -> [ta*]
  ;;   prompt : cont ([] -> [tr*]) -> [tr*]
  ;;
  ;; (We can also give more refined types if we want to support
  ;; answer-type modification and various flavours of answer-type
  ;; polymorphism - but these are well outside the scope of a Wasm
  ;; proposal!)
  ;;
  ;; (Technically this is control0/prompt0 rather than
  ;; control/prompt.)
  (tag $control (export "control") (param (ref $cont-cont)))    ;; control : ([cont ([] -> [])] -> []) -> []
  (func $prompt (export "prompt") (param $nextk (ref null $cont)) ;; prompt : cont ([] -> []) -> []
    (local $h (ref $cont-cont))
    (local $k (ref $cont))
    (block $on_control (result (ref $cont-cont) (ref $cont))
       (resume $cont (on $control $on_control)
                     (local.get $nextk))
       (return)
    ) ;;   $on_control (param (ref $cont-func) (ref $cont))
    (local.set $k)
    (local.set $h)
    (resume $cont-cont (local.get $k) (local.get $h))
  )
)
(register "control")

;; With control/prompt we use functions for abstracting over yield and
;; fork operations rather than tags.

(module $example
  (type $func (func))       ;; [] -> []
  (type $cont (cont $func)) ;; cont ([] -> [])

  (type $cont-func (func (param (ref $cont)))) ;; [cont ([] -> [])] -> []
  (type $cont-cont (cont $cont-func))          ;; cont ([cont ([] -> [])] -> [])

  (type $func-cont-func-func (func (param (ref $func)) (param (ref $cont-func)))) ;; ([] -> []) -> ([cont ([] -> [])] -> []) -> []
  (type $func-cont-func-cont (cont $func-cont-func-func))                         ;; cont (([] -> []) -> ([cont ([] -> [])] -> []) -> [])

  (func $log (import "spectest" "print_i32") (param i32))

  (elem declare func $main $thread1 $thread2 $thread3)

  (func $main (export "main") (param $yield (ref $func)) (param $fork (ref $cont-func))
    (call $log (i32.const 0))
    (call_ref $cont-func
      (cont.bind $func-cont-func-cont $cont (local.get $yield) (local.get $fork)
        (cont.new $func-cont-func-cont (ref.func $thread1)))
      (local.get $fork))
    (call $log (i32.const 1))
    (call_ref $cont-func
      (cont.bind $func-cont-func-cont $cont (local.get $yield) (local.get $fork)
        (cont.new $func-cont-func-cont (ref.func $thread2)))
      (local.get $fork))
    (call $log (i32.const 2))
    (call_ref $cont-func
      (cont.bind $func-cont-func-cont $cont (local.get $yield) (local.get $fork)
        (cont.new $func-cont-func-cont (ref.func $thread3)))
      (local.get $fork))
    (call $log (i32.const 3))
  )

  (func $thread1 (param $yield (ref $func)) (param $fork (ref $cont-func))
    (call $log (i32.const 10))
    (call_ref $func (local.get $yield))
    (call $log (i32.const 11))
    (call_ref $func (local.get $yield))
    (call $log (i32.const 12))
  )

  (func $thread2 (param $yield (ref $func)) (param $fork (ref $cont-func))
    (call $log (i32.const 20))
    (call_ref $func (local.get $yield))
    (call $log (i32.const 21))
    (call_ref $func (local.get $yield))
    (call $log (i32.const 22))
  )

  (func $thread3 (param $yield (ref $func)) (param $fork (ref $cont-func))
    (call $log (i32.const 30))
    (call_ref $func (local.get $yield))
    (call $log (i32.const 31))
    (call_ref $func (local.get $yield))
    (call $log (i32.const 32))
  )
)
(register "example")

(module $queue
  (type $func (func))
  (type $cont (cont $func))

  (func $log (import "spectest" "print_i32") (param i32))

  ;; Table as simple queue (keeping it simple, no ring buffer)
  (table $queue 0 (ref null $cont))
  (global $qdelta i32 (i32.const 10))
  (global $qback (mut i32) (i32.const 0))
  (global $qfront (mut i32) (i32.const 0))

  (func $queue-empty (export "queue-empty") (result i32)
    (i32.eq (global.get $qfront) (global.get $qback))
  )

  (func $dequeue (export "dequeue") (result (ref null $cont))
    (local $i i32)
    (if (call $queue-empty)
      (then (return (ref.null $cont)))
    )
    (local.set $i (global.get $qfront))
    (global.set $qfront (i32.add (local.get $i) (i32.const 1)))
    (table.get $queue (local.get $i))
  )

  (func $enqueue (export "enqueue") (param $k (ref $cont))
    ;; Check if queue is full
    (if (i32.eq (global.get $qback) (table.size $queue))
      (then
        ;; Check if there is enough space in the front to compact
        (if (i32.lt_u (global.get $qfront) (global.get $qdelta))
          (then
            ;; Space is below threshold, grow table instead
            (drop (table.grow $queue (ref.null $cont) (global.get $qdelta)))
          )
          (else
            ;; Enough space, move entries up to head of table
            (global.set $qback (i32.sub (global.get $qback) (global.get $qfront)))
            (table.copy $queue $queue
              (i32.const 0)         ;; dest = new front = 0
              (global.get $qfront)  ;; src = old front
              (global.get $qback)   ;; len = new back = old back - old front
            )
            (table.fill $queue      ;; null out old entries to avoid leaks
              (global.get $qback)   ;; start = new back
              (ref.null $cont)      ;; init value
              (global.get $qfront)  ;; len = old front = old front - new front
            )
            (global.set $qfront (i32.const 0))
          )
        )
      )
    )
    (table.set $queue (global.get $qback) (local.get $k))
    (global.set $qback (i32.add (global.get $qback) (i32.const 1)))
  )
)
(register "queue")

(module $scheduler
  (type $func (func))       ;; [] -> []
  (type $cont (cont $func)) ;; cont ([] -> [])

  (type $cont-func (func (param (ref $cont)))) ;; [cont ([] -> [])] -> []
  (type $cont-cont (cont $cont-func))          ;; cont ([cont ([] -> [])] -> [])

  (type $func-cont-func-func (func (param (ref $func)) (param (ref $cont-func)))) ;; ([] -> []) -> ([cont ([] -> [])] -> []) -> []
  (type $func-cont-func-cont (cont $func-cont-func-func))                         ;; cont (([] -> []) -> ([cont ([] -> [])] -> []) -> [])

  (type $func-cont-cont (func (param (ref $cont)) (param (ref $cont))))
  (type $cont-cont-func (cont $func-cont-cont))

  (func $log (import "spectest" "print_i32") (param i32))

  ;; queue interface
  (func $queue-empty (import "queue" "queue-empty") (result i32))
  (func $dequeue (import "queue" "dequeue") (result (ref null $cont)))
  (func $enqueue (import "queue" "enqueue") (param $k (ref $cont)))

  (elem declare func
     $handle-yield-sync $handle-yield
     $handle-fork-sync $handle-fork-kt $handle-fork-tk $handle-fork-ykt $handle-fork-ytk
     $yield
     $fork-sync $fork-kt $fork-tk $fork-ykt $fork-ytk)

  ;; control/prompt interface
  (tag $control (import "control" "control") (param (ref $cont-cont)))     ;; control : ([cont ([] -> [])] -> []) -> []
  (func $prompt (import "control" "prompt") (param $nextk (ref null $cont))) ;; prompt : cont ([] -> []) -> []

  ;; generic boilerplate scheduler
  ;;
  ;; with control/prompt the core scheduler loop must be decoupled
  ;; from the implementations of each operation (yield / fork) as the
  ;; latter are passed in as arguments to user code
  (func $scheduler (param $nextk (ref null $cont))
    (loop $loop
      (if (ref.is_null (local.get $nextk)) (then (return)))
      (call $prompt (local.get $nextk))
      (local.set $nextk (call $dequeue))
      (br $loop)
    )
  )

  ;; func.bind is needed in the implementations of fork
  ;;
  ;; More generally func.bind is needed for any operation that
  ;; takes arguments.
  ;;
  ;; One could use another continuation here instead, but constructing
  ;; a new continuation every time an operation is invoked seems
  ;; unnecessarily wasteful.

  ;; synchronous scheduler
  (func $handle-yield-sync (param $k (ref $cont))
    (call $scheduler (local.get $k))
  )
  (func $yield-sync
    (suspend $control (cont.new $cont-cont (ref.func $handle-yield)))
  )
  (func $handle-fork-sync (param $t (ref $cont)) (param $k (ref $cont))
    (call $enqueue (local.get $t))
    (call $scheduler (local.get $k))
  )
  (func $fork-sync (param $t (ref $cont))
    (suspend $control
      (cont.bind $cont-cont-func $cont-cont (local.get $t)
        (cont.new $cont-cont-func (ref.func $handle-fork-sync))))
  )
  (func $sync (export "sync") (param $k (ref $func-cont-func-cont))
    (call $scheduler
      (cont.bind $func-cont-func-cont $cont (ref.func $yield) (ref.func $fork-sync) (local.get $k)))
  )

  ;; asynchronous yield (used by all asynchronous schedulers)
  (func $handle-yield (param $k (ref $cont))
    (call $enqueue (local.get $k))
    (call $scheduler (call $dequeue))
  )
  (func $yield
    (suspend $control (cont.new $cont-cont (ref.func $handle-yield)))
  )
  ;; four asynchronous implementations of fork:
  ;;   * kt and tk don't yield on encountering a fork
  ;;     1) kt runs the continuation, queuing up the new thread for later
  ;;     2) tk runs the new thread first, queuing up the continuation for later
  ;;   * ykt and ytk do yield on encountering a fork
  ;;     3) ykt runs the continuation, queuing up the new thread for later
  ;;     4) ytk runs the new thread first, queuing up the continuation for later

  ;; no yield on fork, continuation first
  (func $handle-fork-kt (param $t (ref $cont)) (param $k (ref $cont))
    (call $enqueue (local.get $t))
    (call $scheduler (local.get $k))
  )
  (func $fork-kt (param $t (ref $cont))
    (suspend $control
      (cont.bind $cont-cont-func $cont-cont (local.get $t)
        (cont.new $cont-cont-func (ref.func $handle-fork-kt))))
  )
  (func $kt (export "kt") (param $k (ref $func-cont-func-cont))
    (call $scheduler
      (cont.bind $func-cont-func-cont $cont (ref.func $yield) (ref.func $fork-kt) (local.get $k)))
  )

  ;; no yield on fork, new thread first
  (func $handle-fork-tk (param $t (ref $cont)) (param $k (ref $cont))
    (call $enqueue (local.get $k))
    (call $scheduler (local.get $t))
  )
  (func $fork-tk (param $t (ref $cont))
    (suspend $control
      (cont.bind $cont-cont-func $cont-cont (local.get $t)
        (cont.new $cont-cont-func (ref.func $handle-fork-tk))))
  )
  (func $tk (export "tk") (param $k (ref $func-cont-func-cont))
    (call $scheduler
      (cont.bind $func-cont-func-cont $cont (ref.func $yield) (ref.func $fork-tk) (local.get $k)))
  )

  ;; yield on fork, continuation first
  (func $handle-fork-ykt (param $t (ref $cont)) (param $k (ref $cont))
    (call $enqueue (local.get $k))
    (call $enqueue (local.get $t))
    (call $scheduler (call $dequeue))
  )
  (func $fork-ykt (param $t (ref $cont))
    (suspend $control
      (cont.bind $cont-cont-func $cont-cont (local.get $t)
        (cont.new $cont-cont-func (ref.func $handle-fork-ykt))))
  )
  (func $ykt (export "ykt") (param $k (ref $func-cont-func-cont))
    (call $scheduler
      (cont.bind $func-cont-func-cont $cont (ref.func $yield) (ref.func $fork-ykt) (local.get $k)))
  )

  ;; yield on fork, new thread first
  (func $handle-fork-ytk (param $t (ref $cont)) (param $k (ref $cont))
    (call $enqueue (local.get $t))
    (call $enqueue (local.get $k))
    (call $scheduler (call $dequeue))
  )
  (func $fork-ytk (param $t (ref $cont))
    (suspend $control
      (cont.bind $cont-cont-func $cont-cont (local.get $t)
        (cont.new $cont-cont-func (ref.func $handle-fork-ytk))))
  )
  (func $ytk (export "ytk") (param $k (ref $func-cont-func-cont))
    (call $scheduler
      (cont.bind $func-cont-func-cont $cont (ref.func $yield) (ref.func $fork-ytk) (local.get $k)))
  )
)
(register "scheduler")

(module
  (type $func (func))       ;; [] -> []
  (type $cont (cont $func)) ;; cont ([] -> [])

  (type $cont-func (func (param (ref $cont)))) ;; [cont ([] -> [])] -> []
  (type $cont-cont (cont $cont-func))          ;; cont ([cont ([] -> [])] -> [])

  (type $func-cont-func-func (func (param (ref $func)) (param (ref $cont-func)))) ;; ([] -> []) -> ([cont ([] -> [])] -> []) -> []
  (type $func-cont-func-cont (cont $func-cont-func-func))                         ;; cont (([] -> []) -> ([cont ([] -> [])] -> []) -> [])

  (func $scheduler-sync (import "scheduler" "sync") (param $nextk (ref $func-cont-func-cont)))
  (func $scheduler-kt (import "scheduler" "kt") (param $nextk (ref $func-cont-func-cont)))
  (func $scheduler-tk (import "scheduler" "tk") (param $nextk (ref $func-cont-func-cont)))
  (func $scheduler-ykt (import "scheduler" "ykt") (param $nextk (ref $func-cont-func-cont)))
  (func $scheduler-ytk (import "scheduler" "ytk") (param $nextk (ref $func-cont-func-cont)))

  (func $log (import "spectest" "print_i32") (param i32))

  (func $main (import "example" "main") (param $yield (ref $func)) (param $fork (ref $cont-func)))

  (elem declare func $main)

  (func $run (export "run")
    (call $log (i32.const -1))
    (call $scheduler-sync (cont.new $func-cont-func-cont (ref.func $main)))
    (call $log (i32.const -2))
    (call $scheduler-kt (cont.new $func-cont-func-cont (ref.func $main)))
    (call $log (i32.const -3))
    (call $scheduler-tk (cont.new $func-cont-func-cont (ref.func $main)))
    (call $log (i32.const -4))
    (call $scheduler-ykt (cont.new $func-cont-func-cont (ref.func $main)))
    (call $log (i32.const -5))
    (call $scheduler-ytk (cont.new $func-cont-func-cont (ref.func $main)))
    (call $log (i32.const -6))
  )
)

(invoke "run")
