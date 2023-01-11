(component
  (import "x" (func $f))

  (export $g "g" (func $f))
  (export $g2 "g2" (func $g))
)

(component
  (type (component
    (type $t u8)
    (import "x" (instance $i (export "t" (type (eq $t)))))
    (alias export $i "t" (type $my-t))
  ))
)

(component
  (type (component
    (type $t u8)
    (import "x" (instance $i
      (export "i" (instance
        (export "t" (type (eq $t)))
      ))
    ))
    (alias export $i "i" (instance $my-i))
    (alias export $my-i "t" (type $my-t))
  ))
)

(assert_invalid
  (component
    (type (instance
      (type $t u8)
      (export $t "t" (type (eq $t)))
    ))
  )
  "duplicate type identifier")

(component
  (type (instance
    (type $t u8)
    (export $t2 "t" (type (eq $t)))
    (export $t3 "t2" (type (eq $t2)))
  ))
)
