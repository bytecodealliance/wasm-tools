;; static lightweight threads

;; interface to a fixed collection of lightweight threads
(module $lwt
  (tag $yield (export "yield"))
)
(register "lwt")

(module $example
  (tag $yield (import "lwt" "yield"))
  (func $log (import "spectest" "print_i32") (param i32))

  (func $thread1 (export "thread1")
    (call $log (i32.const 10))
    (suspend $yield)
    (call $log (i32.const 11))
    (suspend $yield)
    (call $log (i32.const 12))
  )

  (func $thread2 (export "thread2")
    (call $log (i32.const 20))
    (suspend $yield)
    (call $log (i32.const 21))
    (suspend $yield)
    (call $log (i32.const 22))
  )

  (func $thread3 (export "thread3")
    (call $log (i32.const 30))
    (suspend $yield)
    (call $log (i32.const 31))
    (suspend $yield)
    (call $log (i32.const 32))
  )
)
(register "example")

;; queue of threads
(module $queue
  (type $func (func))
  (type $cont (cont $func))

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
  (type $func (func))
  (type $cont (cont $func))

  (tag $yield (import "lwt" "yield"))

  ;; queue interface
  (func $queue-empty (import "queue" "queue-empty") (result i32))
  (func $dequeue (import "queue" "dequeue") (result (ref null $cont)))
  (func $enqueue (import "queue" "enqueue") (param $k (ref $cont)))

  (func $run (export "run")
    (loop $l
      (if (call $queue-empty) (then (return)))
      (block $on_yield (result (ref $cont))
        (resume $cont (on $yield $on_yield)
                      (call $dequeue)
        )
        (br $l)  ;; thread terminated
      ) ;;   $on_yield (result (ref $cont))
      (call $enqueue)  ;; continuation of current thread
      (br $l)
    )
  )
)
(register "scheduler")

(module
  (type $func (func))
  (type $cont (cont $func))

  (func $scheduler (import "scheduler" "run"))
  (func $enqueue (import "queue" "enqueue") (param (ref $cont)))

  (func $log (import "spectest" "print_i32") (param i32))

  (func $thread1 (import "example" "thread1"))
  (func $thread2 (import "example" "thread2"))
  (func $thread3 (import "example" "thread3"))

  (elem declare func $thread1 $thread2 $thread3)

  (func (export "run")
    (call $enqueue (cont.new $cont (ref.func $thread1)))
    (call $enqueue (cont.new $cont (ref.func $thread2)))
    (call $enqueue (cont.new $cont (ref.func $thread3)))

    (call $log (i32.const -1))
    (call $scheduler)
    (call $log (i32.const -2))
  )
)

(invoke "run")
