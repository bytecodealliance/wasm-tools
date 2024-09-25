;; async-await interface
(module $async-await
  (type $i-func (func (param i32)))
  (type $i-cont (cont $i-func))

  ;; We use yield and fulfill to simulate asynchronous operations.
  ;;
  ;; Given a suitable asynchronous I/O API, they needn't be exposed to
  ;; user code.
  ;;
  ;; yield : [] -> []
  ;; fulfill : [i32] -> [i32]
  (tag $yield (export "yield"))
  (tag $fulfill (export "fulfill") (param i32) (param i32))

  ;; async : [cont ([i32] -> [])] -> [i32]
  ;; await : [i32] -> [i32]
  (tag $async (export "async") (param (ref $i-cont)) (result i32))
  (tag $await (export "await") (param i32) (result i32))
)
(register "async-await")

(module $example
  (type $i-func (func (param i32)))
  (type $i-cont (cont $i-func))

  (type $iii-fun (func (param i32 i32 i32)))
  (type $iii-cont (cont $iii-fun))

  ;; yield : [] -> []
  ;; fulfill : [i32] -> [i32]
  ;; async : [cont ([i32] -> [])] -> [i32]
  ;; await : [i32] -> [i32]
  (tag $yield (import "async-await" "yield"))
  (tag $fulfill (import "async-await" "fulfill") (param i32) (param i32))
  (tag $async (import "async-await" "async") (param (ref $i-cont)) (result i32))
  (tag $await (import "async-await" "await") (param i32) (result i32))

  (func $log (import "spectest" "print_i32") (param i32))

  (elem declare func $sum)

  ;; an asynchronous function that computes i + i+1 + ... + j
  ;;
  ;; (instead of computing synchronously, it allows other computations
  ;; to execute each time round the loop)
  ;;
  ;; the final result is written to the promise $p
  (func $sum (param $i i32) (param $j i32) (param $p i32)
     (local $a i32)
     (loop $l
        (call $log (local.get $i))
        (local.set $a (i32.add (local.get $a) (local.get $i)))
        (local.set $i (i32.add (local.get $i) (i32.const 1)))
        (if (i32.le_u (local.get $i) (local.get $j))
           (then (suspend $yield)
                 (br $l))
        )
     )
     (suspend $fulfill (local.get $p) (local.get $a))
  )

  ;; compute p = 1+..+3; q = 5+..+7; r = 10+...+15 asynchronously
  ;; once p and q have finished computing, compute x = p*q
  ;; once r has finished computing, return x+r
  (func $run (export "run")
     (local $p i32)
     (local $q i32)
     (local $r i32)

     (local $x i32)
     (local $y i32)

     (call $log (i32.const -1))
     (local.set $p (suspend $async (cont.bind $iii-cont $i-cont (i32.const 1) (i32.const 3) (cont.new $iii-cont (ref.func $sum)))))
     (call $log (i32.const -2))
     (local.set $q (suspend $async (cont.bind $iii-cont $i-cont (i32.const 5) (i32.const 7) (cont.new $iii-cont (ref.func $sum)))))
     (call $log (i32.const -3))
     (local.set $r (suspend $async (cont.bind $iii-cont $i-cont (i32.const 10) (i32.const 15) (cont.new $iii-cont (ref.func $sum)))))
     (call $log (i32.const -4))

     (local.set $x (i32.mul (suspend $await (local.get $p))
                            (suspend $await (local.get $q))))

     (call $log (i32.const -5))

     (local.set $y (i32.add (suspend $await (local.get $r)) (local.get $x)))

     (call $log (i32.const -6))
     (call $log (local.get $y))
     (call $log (i32.const -7))
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

  (func $enqueue (export "enqueue") (param $k (ref null $cont))
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

;; promises
(module $promise
  (type $func (func))
  (type $cont (cont $func))

  (type $i-func (func (param i32)))
  (type $i-cont (cont $i-func))

  ;; a simplistic implementation of promises that assumes a maximum of
  ;; 1000 promises and a maximum of one observer per promise

  (tag $too-many-promises)
  (tag $too-many-observers)

  (global $num-promises (mut i32) (i32.const 0))
  (global $max-promises i32 (i32.const 1000))
  (table $observers 1000 (ref null $i-cont))  ;; observers waiting for promises to be fulfilled
  (memory 1)                                 ;; promise values

  ;; create and return a new promise
  (func $new (export "new") (result i32)
     (local $offset i32)
     (local $p i32)
     (if (i32.eq (global.get $num-promises) (global.get $max-promises))
        (then (throw $too-many-promises)))
     (local.set $p (global.get $num-promises))
     (local.set $offset (i32.mul (local.get $p) (i32.const 4)))
     (table.set $observers (local.get $p) (ref.null $i-cont))
     (i32.store (local.get $offset) (i32.const -1))
     (global.set $num-promises (i32.add (local.get $p) (i32.const 1)))
     (return (local.get $p))
  )

  ;; check whether promise $p is fulfilled
  (func $fulfilled (export "fulfilled") (param $p i32) (result i32)
    (local $offset i32)
    (local.set $offset (i32.mul (local.get $p) (i32.const 4)))
    (i32.ne (i32.load (local.get $offset)) (i32.const -1))
  )

  ;; current value of promise $p
  (func $read (export "read") (param $p i32) (result i32)
    (local $offset i32)
    (local.set $offset (i32.mul (local.get $p) (i32.const 4)))
    (i32.load (local.get $offset))
  )

  ;; register an observer for when promise $p is fulfilled
  (func $await (export "await") (param $p i32) (param $k (ref $i-cont))
    (if (ref.is_null (table.get $observers (local.get $p)))
       (then (table.set $observers (local.get $p) (local.get $k)))
       (else (throw $too-many-observers))
    )
  )

  ;; fulfill promise $p with value $v
  (func $fulfill (export "fulfill") (param $p i32) (param $v i32) (result (ref null $cont))
    (local $offset i32)
    (local $k (ref null $i-cont))
    (local.set $offset (i32.mul (local.get $p) (i32.const 4)))
    (i32.store (local.get $offset) (local.get $v))
    (local.set $k (table.get $observers (local.get $p)))
    (if (ref.is_null (local.get $k))
      (then (return (ref.null $cont)))
    )
    (return (cont.bind $i-cont $cont (local.get $v) (local.get $k)))
  )
)
(register "promise")

;; async-await scheduler
(module $scheduler
  (type $func (func))
  (type $cont (cont $func))

  (type $i-func (func (param i32)))
  (type $i-cont (cont $i-func))

  ;; async-await interface
  ;;
  ;; yield : [] -> []
  ;; fulfill : [i32] -> [i32]
  ;; async : [cont ([i32] -> [])] -> [i32]
  ;; await : [i32] -> [i32]
  (tag $yield (import "async-await" "yield"))
  (tag $fulfill (import "async-await" "fulfill") (param i32) (param i32))
  (tag $async (import "async-await" "async") (param (ref $i-cont)) (result i32))
  (tag $await (import "async-await" "await") (param i32) (result i32))

  ;; queue interface
  (func $queue-empty (import "queue" "queue-empty") (result i32))
  (func $dequeue (import "queue" "dequeue") (result (ref null $cont)))
  (func $enqueue (import "queue" "enqueue") (param $k (ref null $cont)))

  ;; promise interface
  (func $new-promise (import "promise" "new") (result i32))
  (func $promise-fulfilled (import "promise" "fulfilled") (param $p i32) (result i32))
  (func $promise-value (import "promise" "read") (param $p i32) (result i32))
  (func $await-promise (import "promise" "await") (param $p i32) (param $k (ref $i-cont)))
  (func $fulfill-promise (import "promise" "fulfill") (param $p i32) (param $v i32) (result (ref null $cont)))

  (func $run (export "run") (param $nextk (ref null $cont))
    (local $p i32)
    (local $v i32)
    (local $ik (ref $i-cont))
    (local $ak (ref $i-cont))
    (local $k (ref null $cont))
    (loop $l
      (if (ref.is_null (local.get $nextk)) (then (return)))
      (block $on_yield (result (ref $cont))
        (block $on_fulfill (result i32 i32 (ref $cont))
          (block $on_async (result (ref $i-cont) (ref $i-cont))
            (block $on_await (result i32 (ref $i-cont))
              (resume $cont (on $yield $on_yield)
                            (on $fulfill $on_fulfill)
                            (on $async $on_async)
                            (on $await $on_await)
                            (local.get $nextk)
              )
              (local.set $nextk (call $dequeue))
              (br $l)  ;; thread terminated
            ) ;;   $on_await (result i32 (ref $i-cont))
            (local.set $ik)
            (local.set $p)
            (if (call $promise-fulfilled (local.get $p))
                ;; if promise fulfilled then run continuation partially applied to value
                (then (local.set $nextk (cont.bind $i-cont $cont (call $promise-value (local.get $p)) (local.get $ik))))
                ;; else add continuation to promise and run next continuation from the queue
                (else (call $await-promise (local.get $p) (local.get $ik))
                      (local.set $nextk (call $dequeue)))
            )
            (br $l)
          ) ;;   $on_async (result (ref $i-func) (ref $i-cont))
          (local.set $ik)
          (local.set $ak)
            ;; create new promise
            (call $new-promise)
            (local.set $p)
            ;; enqueue continuation partially applied to promise
            (call $enqueue (cont.bind $i-cont $cont (local.get $p) (local.get $ik)))
            ;; run computation partially applied to promise
            (local.set $nextk (cont.bind $i-cont $cont (local.get $p) (local.get $ak)))
          (br $l)
        ) ;;   $on_fulfill (result i32 i32 (ref $cont))
        (local.set $nextk)
        (local.set $v)
        (local.set $p)
        (call $fulfill-promise (local.get $p) (local.get $v))
        (local.set $k)
        (if (ref.is_null (local.get $k))
          (then)
          (else (call $enqueue (local.get $k)))
        )
        (br $l)
      ) ;;   $on_yield (result (ref $cont))
      (call $enqueue)                    ;; current thread
      (local.set $nextk (call $dequeue)) ;; next thread
      (br $l)
    )
  )
)
(register "scheduler")

(module
  (type $func (func))
  (type $cont (cont $func))

  (func $scheduler (import "scheduler" "run") (param $nextk (ref null $cont)))

  (func $log (import "spectest" "print_i32") (param i32))

  (func $run-example (import "example" "run"))

  (elem declare func $run-example)

  (func (export "run")
    (call $scheduler (cont.new $cont (ref.func $run-example)))
  )
)

(invoke "run")
