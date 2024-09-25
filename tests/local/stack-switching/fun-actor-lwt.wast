;; Actors via lightweight threads - functional version

;; actor interface
(module $actor
  (type $func (func))
  (type $cont (cont $func))

  (tag $self (export "self") (result i32))
  (tag $spawn (export "spawn") (param (ref $cont)) (result i32))
  (tag $send (export "send") (param i32 i32))
  (tag $recv (export "recv") (result i32))
)
(register "actor")

;; a simple example - pass a message through a chain of actors
(module $chain
  (type $func (func))
  (type $cont (cont $func))

  (type $i-func (func (param i32)))
  (type $i-cont (cont $i-func))

  (tag $self (import "actor" "self") (result i32))
  (tag $spawn (import "actor" "spawn") (param (ref $cont)) (result i32))
  (tag $send (import "actor" "send") (param i32 i32))
  (tag $recv (import "actor" "recv") (result i32))

  (elem declare func $next)

  (func $log (import "spectest" "print_i32") (param i32))

  (func $next (param $p i32)
    (local $s i32)
    (local.set $s (suspend $recv))
    (call $log (i32.const -1))
    (suspend $send (local.get $s) (local.get $p))
  )

  ;; send the message 42 through a chain of n actors
  (func $chain (export "chain") (param $n i32)
    (local $s i32)
    (local $p i32)
    (local.set $p (suspend $self))

    (loop $l
      (if (i32.eqz (local.get $n))
        (then (suspend $send (i32.const 42) (local.get $p)))
        (else (local.set $p (suspend $spawn (cont.bind $i-cont $cont (local.get $p) (cont.new $i-cont (ref.func $next)))))
              (local.set $n (i32.sub (local.get $n) (i32.const 1)))
              (br $l))
      )
    )
    (local.set $s (suspend $recv))
    (call $log (local.get $s))
  )
)
(register "chain")

;; interface to lightweight threads
(module $lwt
  (type $func (func))
  (type $cont (cont $func))

  (tag $yield (export "yield"))
  (tag $fork (export "fork") (param (ref $cont)))
)
(register "lwt")

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

;; simple scheduler
(module $scheduler
  (type $func (func))
  (type $cont (cont $func))

  (tag $yield (import "lwt" "yield"))
  (tag $fork (import "lwt" "fork") (param (ref $cont)))

  (func $queue-empty (import "queue" "queue-empty") (result i32))
  (func $dequeue (import "queue" "dequeue") (result (ref null $cont)))
  (func $enqueue (import "queue" "enqueue") (param $k (ref $cont)))

  (func $run (export "run") (param $main (ref $cont))
    (call $enqueue (local.get $main))
    (loop $l
      (if (call $queue-empty) (then (return)))
      (block $on_yield (result (ref $cont))
        (block $on_fork (result (ref $cont) (ref $cont))
          (resume $cont (on $yield $on_yield) (on $fork $on_fork)
            (call $dequeue)
          )
          (br $l)  ;; thread terminated
        ) ;;   $on_fork (result (ref $cont) (ref $cont))
        (call $enqueue) ;; current thread
        (call $enqueue) ;; new thread
        (br $l)
      )
      ;;     $on_yield (result (ref $cont))
      (call $enqueue) ;; current thread
      (br $l)
    )
  )
)
(register "scheduler")

(module $mailboxes
  ;; Stupid implementation of mailboxes that raises an exception if
  ;; there are too many mailboxes or if more than one message is sent
  ;; to any given mailbox.
  ;;
  ;; Sufficient for the simple chain example.

  ;; -1 means empty

  (tag $too-many-mailboxes)
  (tag $too-many-messages)

  (memory 1)

  (global $msize (mut i32) (i32.const 0))
  (global $mmax i32 (i32.const 1024)) ;; maximum number of mailboxes

  (func $init (export "init")
     (memory.fill (i32.const 0) (i32.const -1) (i32.mul (global.get $mmax) (i32.const 4)))
  )

  (func $empty-mb (export "empty-mb") (param $mb i32) (result i32)
    (local $offset i32)
    (local.set $offset (i32.mul (local.get $mb) (i32.const 4)))
    (i32.eq (i32.load (local.get $offset)) (i32.const -1))
  )

  (func $new-mb (export "new-mb") (result i32)
     (local $mb i32)

     (if (i32.ge_u (global.get $msize) (global.get $mmax))
         (then (throw $too-many-mailboxes))
     )

     (local.set $mb (global.get $msize))
     (global.set $msize (i32.add (global.get $msize) (i32.const 1)))
     (return (local.get $mb))
  )

  (func $send-to-mb (export "send-to-mb") (param $v i32) (param $mb i32)
    (local $offset i32)
    (local.set $offset (i32.mul (local.get $mb) (i32.const 4)))
    (if (call $empty-mb (local.get $mb))
      (then (i32.store (local.get $offset) (local.get $v)))
      (else (throw $too-many-messages))
    )
  )

  (func $recv-from-mb (export "recv-from-mb") (param $mb i32) (result i32)
    (local $v i32)
    (local $offset i32)
    (local.set $offset (i32.mul (local.get $mb) (i32.const 4)))
    (local.set $v (i32.load (local.get $offset)))
    (i32.store (local.get $offset) (i32.const -1))
    (local.get $v)
  )
)
(register "mailboxes")

;; actors via lightweight threads
(module $actor-as-lwt
  (type $func (func))
  (type $cont (cont $func))

  (type $i-func (func (param i32)))
  (type $i-cont (cont $i-func))

  (type $icont-func (func (param i32 (ref $cont))))
  (type $icont-cont (cont $icont-func))

  (func $log (import "spectest" "print_i32") (param i32))

  ;; lwt interface
  (tag $yield (import "lwt" "yield"))
  (tag $fork (import "lwt" "fork") (param (ref $cont)))

  ;; mailbox interface
  (func $init (import "mailboxes" "init"))
  (func $empty-mb (import "mailboxes" "empty-mb") (param $mb i32) (result i32))
  (func $new-mb (import "mailboxes" "new-mb") (result i32))
  (func $send-to-mb (import "mailboxes" "send-to-mb") (param $v i32) (param $mb i32))
  (func $recv-from-mb (import "mailboxes" "recv-from-mb") (param $mb i32) (result i32))

  ;; actor interface
  (tag $self (import "actor" "self") (result i32))
  (tag $spawn (import "actor" "spawn") (param (ref $cont)) (result i32))
  (tag $send (import "actor" "send") (param i32 i32))
  (tag $recv (import "actor" "recv") (result i32))

  (elem declare func $act-nullary $act-res)

  ;; resume with $ik applied to $res
  (func $act-res (param $mine i32) (param $res i32) (param $ik (ref $i-cont))
    (local $yours i32)
    (local $k (ref $cont))
    (local $you (ref $cont))
    (block $on_self (result (ref $i-cont))
      (block $on_spawn (result (ref $cont) (ref $i-cont))
        (block $on_send (result i32 i32 (ref $cont))
          (block $on_recv (result (ref $i-cont))
             ;; this should really be a tail call to the continuation
             ;; do we need a 'return_resume' operator?
             (resume $i-cont (on $self $on_self)
                             (on $spawn $on_spawn)
                             (on $send $on_send)
                             (on $recv $on_recv)
                             (local.get $res) (local.get $ik)
             )
             (return)
          ) ;;   $on_recv (result (ref $i-cont))
          (local.set $ik)
          ;; block this thread until the mailbox is non-empty
          (loop $l
            (if (call $empty-mb (local.get $mine))
                (then (suspend $yield)
                      (br $l))
            )
          )
          (call $recv-from-mb (local.get $mine))
          (local.set $res)
          (return_call $act-res (local.get $mine) (local.get $res) (local.get $ik))
        ) ;;   $on_send (result i32 i32 (ref $cont))
        (local.set $k)
        (call $send-to-mb)
        (return_call $act-nullary (local.get $mine) (local.get $k))
      ) ;;   $on_spawn (result (ref $cont) (ref $i-cont))
      (local.set $ik)
      (local.set $you)
      (call $new-mb)
      (local.set $yours)
      (suspend $fork (cont.bind $icont-cont $cont
                                (local.get $yours)
                                (local.get $you)
                                (cont.new $icont-cont (ref.func $act-nullary))))
      (return_call $act-res (local.get $mine) (local.get $yours) (local.get $ik))
    ) ;;   $on_self (result (ref $i-cont))
    (local.set $ik)
    (return_call $act-res (local.get $mine) (local.get $mine) (local.get $ik))
  )

  ;; resume with nullary continuation
  (func $act-nullary (param $mine i32) (param $k (ref $cont))
    (local $res i32)
    (local $ik (ref $i-cont))
    (local $you (ref $cont))
    (local $yours i32)
    (block $on_self (result (ref $i-cont))
      (block $on_spawn (result (ref $cont) (ref $i-cont))
        (block $on_send (result i32 i32 (ref $cont))
          (block $on_recv (result (ref $i-cont))
             ;; this should really be a tail call to the continuation
             ;; do we need a 'return_resume' operator?
             (resume $cont (on $self $on_self)
                           (on $spawn $on_spawn)
                           (on $send $on_send)
                           (on $recv $on_recv)
                           (local.get $k)
             )
             (return)
          ) ;;   $on_recv (result (ref $i-cont))
          (local.set $ik)
          ;; block this thread until the mailbox is non-empty
          (loop $l
            (if (call $empty-mb (local.get $mine))
                (then (suspend $yield)
                      (br $l))
            )
          )
          (call $recv-from-mb (local.get $mine))
          (local.set $res)
          (return_call $act-res (local.get $mine) (local.get $res) (local.get $ik))
        ) ;;   $on_send (result i32 i32 (ref $cont))
        (local.set $k)
        (call $send-to-mb)
        (return_call $act-nullary (local.get $mine) (local.get $k))
      ) ;;   $on_spawn (result (ref $cont) (ref $i-cont))
      (local.set $ik)
      (local.set $you)
      (call $new-mb)
      (local.set $yours)
      (suspend $fork (cont.bind $icont-cont $cont
                                (local.get $yours)
                                (local.get $you)
                                (cont.new $icont-cont (ref.func $act-nullary))))
      (return_call $act-res (local.get $mine) (local.get $yours) (local.get $ik))
    ) ;;   $on_self (result (ref $i-cont))
    (local.set $ik)
    (return_call $act-res (local.get $mine) (local.get $mine) (local.get $ik))
  )

  (func $act (export "act") (param $k (ref $cont))
    (call $init)
    (call $act-nullary (call $new-mb) (local.get $k))
  )
)
(register "actor-as-lwt")

;; composing the actor and scheduler handlers together
(module $actor-scheduler
  (type $func (func))
  (type $cont (cont $func))

  (type $cont-func (func (param (ref $cont))))
  (type $cont-cont (cont $cont-func))

  (type $f-func (func (param (ref $func))))

  (elem declare func $act $scheduler)

  (func $act (import "actor-as-lwt" "act") (param $k (ref $cont)))
  (func $scheduler (import "scheduler" "run") (param $k (ref $cont)))

  (func $run-actor (export "run-actor") (param $k (ref $cont))
    (call $scheduler (cont.bind $cont-cont $cont (local.get $k) (cont.new $cont-cont (ref.func $act))))
  )
)
(register "actor-scheduler")

(module
  (type $func (func))
  (type $cont (cont $func))

  (type $i-func (func (param i32)))
  (type $i-cont (cont $i-func))

  (elem declare func $chain)

  (func $run-actor (import "actor-scheduler" "run-actor") (param $k (ref $cont)))
  (func $chain (import "chain" "chain") (param $n i32))

  (func $run-chain (export "run-chain") (param $n i32)
    (call $run-actor (cont.bind $i-cont $cont (local.get $n) (cont.new $i-cont (ref.func $chain))))
  )
)

(invoke "run-chain" (i32.const 64))
