;; Generators

;; generator interface
(module $generator
  (tag $yield (export "yield") (param i32))
)
(register "generator")

(module $examples
  (type $func (func))
  (type $cont (cont $func))

  (tag $yield (import "generator" "yield") (param i32))

  (func $log (import "spectest" "print_i32") (param i32))

  ;; yields successive natural numbers
  (func $naturals (export "naturals")
    (local $n i32)
    (loop $l
      (suspend $yield (local.get $n))
      (local.set $n (i32.add (local.get $n) (i32.const 1)))
      (br $l)
    )
  )

  ;; yields 1-2-3
  (func $one-two-three (export "one-two-three")
    (suspend $yield (i32.const 1))
    (suspend $yield (i32.const 2))
    (suspend $yield (i32.const 3))
  )

  ;; yields successive Fibonacci numbers
  (func $fibonacci (export "fibonacci")
    (local $a i32)
    (local $b i32)
    (local $t i32)
    (local.set $b (i32.const 1))
    (loop $l
      (suspend $yield (local.get $a))
      (local.set $t (local.get $a))
      (local.set $a (local.get $b))
      (local.set $b (i32.add (local.get $t) (local.get $a)))
      (br $l)
    )
  )

  (func $print-first (export "print-first") (param $n i32) (param $k (ref $cont))
    (loop $l
      (block $on_yield (result i32 (ref $cont))
        (if (local.get $n)
          (then (resume $cont (on $yield $on_yield) (local.get $k)))
        )
        (return)
      ) ;;   $on_yield (result i32 (ref $cont))
      (local.set $k)
      (call $log)
      (local.set $n (i32.add (local.get $n) (i32.const -1)))
      (br $l)
    )
    (unreachable)
  )
  
  (func $sum-first (export "sum-first") (param $n i32) (param $k (ref $cont)) (result i32)
    (local $sum i32)
    (loop $l
      (block $on_yield (result i32 (ref $cont))
        (if (local.get $n)
          (then (resume $cont (on $yield $on_yield) (local.get $k)))
        )
        (return (local.get $sum))
      ) ;;   $on_yield (result i32 (ref $cont))
      (local.set $k)
      (local.set $sum (i32.add (local.get $sum)))
      (local.set $n (i32.add (local.get $n) (i32.const -1)))
      (br $l)
    )
    (unreachable)
  )
)
(register "examples")

;; storing generators in a global table and then accessing them through i32 handles
;; without knowledge of handlers
(module $manager
  (type $func (func))
  (type $cont (cont $func))

  (tag $yield (import "generator" "yield") (param i32))

  (table $active 0 (ref null $cont))

  (func $init (export "init") (param $k (ref $cont)) (result i32)
    (table.grow $active (local.get $k) (i32.const 1))
  )

  (func $next (export "next") (param $g i32) (result i32)
    (local $next_k (ref $cont))
    (local $next_v i32)
    (block $on_yield (result i32 (ref $cont))
      (resume $cont (on $yield $on_yield)
                    (table.get $active (local.get $g))
      )
      (return (i32.const -1))
    ) ;;   $on_yield (result i32 (ref $cont))
    (local.set $next_k)
    (local.set $next_v)
    (table.set (local.get $g) (local.get $next_k))
    (return (local.get $next_v))
  )
)
(register "manager")

(module
  (type $func (func))
  (type $cont (cont $func))

  (elem declare func $naturals $fibonacci $one-two-three)

  (func $log (import "spectest" "print_i32") (param i32))
  (func $naturals (import "examples" "naturals"))
  (func $fibonacci (import "examples" "fibonacci"))
  (func $one-two-three (import "examples" "one-two-three"))
  (func $print-first (import "examples" "print-first") (param $n i32) (param $k (ref $cont)))
  (func $sum-first (import "examples" "sum-first") (param $n i32) (param $k (ref $cont)) (result i32))
  (func $init (import "manager" "init") (param $k (ref $cont)) (result i32))
  (func $next (import "manager" "next") (param i32) (result i32))

  (func $print-with-next (param $n i32) (param $gen i32)
    (loop $l
      (if (i32.eqz (local.get $n)) (then (return)))
      (call $next (local.get $gen))
      (call $log)
      (local.set $n (i32.add (local.get $n) (i32.const -1)))
      (br $l)
    )
  )

  (func $interleave-naturals-and-fib
      (local $gen1 i32)
      (local $gen2 i32)
      (local.set $gen1 (call $init (cont.new $cont (ref.func $naturals))))
      (local.set $gen2 (call $init (cont.new $cont (ref.func $fibonacci))))
      (call $print-with-next (i32.const 5) (local.get $gen1))
      (call $print-with-next (i32.const 5) (local.get $gen2))
      (call $print-with-next (i32.const 5) (local.get $gen1))
      (call $print-with-next (i32.const 5) (local.get $gen2))
      (call $print-with-next (i32.const 5) (local.get $gen1))
      (call $print-with-next (i32.const 5) (local.get $gen2))
      (call $print-with-next (i32.const 5) (local.get $gen1))
      (call $print-with-next (i32.const 5) (local.get $gen2))
  )

  (func $main (export "main")
    (call $interleave-naturals-and-fib)
    (call $print-first (i32.const 42) (cont.new $cont (ref.func $naturals)))
    (call $print-first (i32.const 42) (cont.new $cont (ref.func $fibonacci)))
    (call $sum-first (i32.const 101) (cont.new $cont (ref.func $naturals)))
    (call $log)
    (call $sum-first (i32.const 10) (cont.new $cont (ref.func $one-two-three)))
    (call $log)
  )
)

(invoke "main")
