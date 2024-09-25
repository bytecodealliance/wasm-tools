;; functional lightweight threads

;; interface to lightweight threads
(module $lwt
  (type $func (func))       ;; [] -> []
  (type $cont (cont $func)) ;; cont ([] -> [])

  (tag $yield (export "yield"))                   ;; [] -> []
  (tag $fork (export "fork") (param (ref $cont))) ;; [cont ([] -> [])] -> []
)
(register "lwt")

(module $example
  (type $func (func))       ;; [] -> []
  (type $cont (cont $func)) ;; cont ([] -> [])

  (tag $yield (import "lwt" "yield"))                   ;; [] -> []
  (tag $fork (import "lwt" "fork") (param (ref $cont))) ;; [cont ([] -> [])] -> []

  (func $log (import "spectest" "print_i32") (param i32))

  (elem declare func $thread1 $thread2 $thread3)

  (func $main (export "main")
    (call $log (i32.const 0))
    (suspend $fork (cont.new $cont (ref.func $thread1)))
    (call $log (i32.const 1))
    (suspend $fork (cont.new $cont (ref.func $thread2)))
    (call $log (i32.const 2))
    (suspend $fork (cont.new $cont (ref.func $thread3)))
    (call $log (i32.const 3))
  )

  (func $thread1
    (call $log (i32.const 10))
    (suspend $yield)
    (call $log (i32.const 11))
    (suspend $yield)
    (call $log (i32.const 12))
  )

  (func $thread2
    (call $log (i32.const 20))
    (suspend $yield)
    (call $log (i32.const 21))
    (suspend $yield)
    (call $log (i32.const 22))
  )

  (func $thread3
    (call $log (i32.const 30))
    (suspend $yield)
    (call $log (i32.const 31))
    (suspend $yield)
    (call $log (i32.const 32))
  )
)
(register "example")

(module $queue
  (type $func (func))       ;; [] -> []
  (type $cont (cont $func)) ;; cont ([] -> [])

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

  (tag $yield (import "lwt" "yield"))                   ;; [] -> []
  (tag $fork (import "lwt" "fork") (param (ref $cont))) ;; [cont ([] -> [])] -> []

  (func $queue-empty (import "queue" "queue-empty") (result i32))
  (func $dequeue (import "queue" "dequeue") (result (ref null $cont)))
  (func $enqueue (import "queue" "enqueue") (param $k (ref $cont)))

  ;; synchronous scheduler (run current thread to completion without
  ;; yielding)
  (func $sync (export "sync") (param $nextk (ref null $cont))
    (if (ref.is_null (local.get $nextk)) (then (return)))
    (block $on_yield (result (ref $cont))
      (block $on_fork (result (ref $cont) (ref $cont))
        (resume $cont
          (on $yield $on_yield)
          (on $fork $on_fork)
          (local.get $nextk)
        )
        (return_call $sync (call $dequeue))
      ) ;;   $on_fork (result (ref $func) (ref $cont))
      (local.set $nextk)
      (call $enqueue)
      (return_call $sync (local.get $nextk))
    ) ;;   $on_yield (result (ref $cont))
    (return_call $sync)
  )

  ;; four different schedulers:
  ;;   * kt and tk don't yield on encountering a fork
  ;;     1) kt runs the continuation, queuing up the new thread for later
  ;;     2) tk runs the new thread first, queuing up the continuation for later
  ;;   * ykt and ytk do yield on encountering a fork
  ;;     3) ykt runs the continuation, queuing up the new thread for later
  ;;     4) ytk runs the new thread first, queuing up the continuation for later

  ;; no yield on fork, continuation first
  (func $kt (export "kt") (param $nextk (ref null $cont))
    (if (ref.is_null (local.get $nextk)) (then (return)))
    (block $on_yield (result (ref $cont))
      (block $on_fork (result (ref $cont) (ref $cont))
        (resume $cont
          (on $yield $on_yield)
          (on $fork $on_fork)
          (local.get $nextk)
        )
        (return_call $tk (call $dequeue))
      ) ;;   $on_fork (result (ref $func) (ref $cont))
      (local.set $nextk)
      (call $enqueue)
      (return_call $tk (local.get $nextk))
    ) ;;   $on_yield (result (ref $cont))
    (call $enqueue)
    (return_call $tk (call $dequeue))
  )

  ;; no yield on fork, new thread first
  (func $tk (export "tk") (param $nextk (ref null $cont))
    (if (ref.is_null (local.get $nextk)) (then (return)))
    (block $on_yield (result (ref $cont))
      (block $on_fork (result (ref $cont) (ref $cont))
        (resume $cont
          (on $yield $on_yield)
          (on $fork $on_fork)
          (local.get $nextk))
        (return_call $kt (call $dequeue))
      ) ;;   $on_fork (result (ref $cont) (ref $cont))
      (return_call $kt (call $enqueue))
    ) ;;   $on_yield (result (ref $cont))
    (call $enqueue)
    (return_call $kt (call $dequeue))
  )

  ;; yield on fork, continuation first
  (func $ykt (export "ykt") (param $nextk (ref null $cont))
    (if (ref.is_null (local.get $nextk)) (then (return)))
    (block $on_yield (result (ref $cont))
      (block $on_fork (result (ref $cont) (ref $cont))
        (resume $cont
          (on $yield $on_yield)
          (on $fork $on_fork)
          (local.get $nextk)
        )
        (return_call $ykt (call $dequeue))
      ) ;;   $on_fork (result (ref $cont) (ref $cont))
      (call $enqueue)
      (call $enqueue)
      (return_call $ykt (call $dequeue))
    ) ;;   $on_yield (result (ref $cont))
    (call $enqueue)
    (return_call $ykt (call $dequeue))
  )

  ;; yield on fork, new thread first
  (func $ytk (export "ytk") (param $nextk (ref null $cont))
    (local $k (ref $cont))
    (if (ref.is_null (local.get $nextk)) (then (return)))
    (block $on_yield (result (ref $cont))
      (block $on_fork (result (ref $cont) (ref $cont))
        (resume $cont (on $yield $on_yield) (on $fork $on_fork) (local.get $nextk))
        (return_call $ytk (call $dequeue))
      ) ;;   $on_fork (result (ref $cont) (ref $cont))
      (local.set $k)
      (call $enqueue)
      (call $enqueue (local.get $k))
      (return_call $ytk (call $dequeue))
    ) ;;   $on_yield (result (ref $cont))
    (call $enqueue)
    (return_call $ytk (call $dequeue))
  )
)

(register "scheduler")

(module
  (type $func (func))       ;; [] -> []
  (type $cont (cont $func)) ;; cont ([] -> [])

  (func $scheduler-sync (import "scheduler" "sync") (param $nextk (ref null $cont)))
  (func $scheduler-kt (import "scheduler" "kt") (param $nextk (ref null $cont)))
  (func $schedule-tk (import "scheduler" "tk") (param $nextk (ref null $cont)))
  (func $scheduler-ykt (import "scheduler" "ykt") (param $nextk (ref null $cont)))
  (func $scheduler-ytk (import "scheduler" "ytk") (param $nextk (ref null $cont)))

  (func $log (import "spectest" "print_i32") (param i32))

  (func $main (import "example" "main"))

  (elem declare func $main)

  (func (export "run")
    (call $log (i32.const -1))
    (call $scheduler-sync (cont.new $cont (ref.func $main)))
    (call $log (i32.const -2))
    (call $scheduler-kt (cont.new $cont (ref.func $main)))
    (call $log (i32.const -3))
    (call $schedule-tk (cont.new $cont (ref.func $main)))
    (call $log (i32.const -4))
    (call $scheduler-ykt (cont.new $cont (ref.func $main)))
    (call $log (i32.const -5))
    (call $scheduler-ytk (cont.new $cont (ref.func $main)))
    (call $log (i32.const -6))
  )
)

(invoke "run")

