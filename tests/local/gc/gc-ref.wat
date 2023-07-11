;; --enable-gc

(module
  (type $a (struct))
  (type $b (struct))

  (type $f1 (func (param (ref $a))))
  (type $f2 (func (result (ref $b))))

  (global (ref null $a) (ref.null $a))
  (global (ref null $b) (ref.null $b))

  (func (param (ref $a)))
  (func (param (ref 0)))
  (func (type $f1) (param (ref 0)))
  (func (type $f1) (param (ref $a)))
  (func (result (ref $a)) unreachable)
  (func (local (ref $a)))

  (func
    (select (result (ref $a)) unreachable unreachable (i32.const 0))

    (block (param (ref $a)) unreachable)
    (block (result (ref $b)) unreachable)
    (block $f1 (param (ref $a)) unreachable)
    (block $f2 (result (ref $b)) unreachable)

    (loop (param (ref $a)) unreachable)
    (loop (result (ref $b)) unreachable)
    (loop $f1 (param (ref $a)) unreachable)
    (loop $f2 (result (ref $b)) unreachable)
    drop

    (if (param (ref $a)) (then unreachable) (else unreachable))
    (if (result (ref $b)) (then unreachable) (else unreachable))
    drop
    (if $f1 (param (ref $a)) (then unreachable) (else unreachable))
    (if $f2 (result (ref $b)) (then unreachable) (else unreachable))
    drop
  )
)
