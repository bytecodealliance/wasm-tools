;; --enable-gc

(module
  (rec
    (type (func))
  )
  (rec
    (type (func))
    (type (func))
  )
  (rec
    (type (struct))
  )
  (rec
    (type (struct))
    (type (struct))
  )
  (rec
    (type (array i32))
  )
  (rec
    (type (array i32))
    (type (array i32))
  )
  (rec
    (type (func))
    (type (struct))
    (type (array i32))
  )

  (rec
    (type $a (sub (func)))
    (type (sub $a (func)))
  )
  (rec
    (type (sub $a (func)))
  )
  (rec
    (type (sub final $a (func)))
  )
  (type (sub $a (func)))

  (rec
    (type $t1 (struct (field (ref $t2))))
    (type $t2 (sub (struct (field (ref $t1)))))
  )

  (rec
    (type $t3 (struct (field (ref $t4))))
    (type $t4 (sub $t2 (struct (field (ref $t3)))))
  )

  (rec)
)
