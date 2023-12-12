;; --enable-gc

(assert_invalid
  (module
    (rec
      (type $t1 (struct (field (ref $t2))))
      (type $t2 (struct (field (ref $t3)))) ;; <--- unknown type
    )

    (rec
      (type $t3 (struct (field (ref $t4))))
      (type $t4 (sub $t2 (struct (field (ref $t3)))))
    )
  )
  "unknown type"
)

(assert_invalid
  (module
    (rec
      (type $t1 (struct (field (ref $t2))))
      (type $t2 (sub (struct (field (ref $t1)))))
    )

    (rec
      (type $t3 (struct (field (ref $t4))))
      (type $t4 (sub $t2 (struct (field (ref $t3)))))
    )
  )
  "sub type must match super type")
