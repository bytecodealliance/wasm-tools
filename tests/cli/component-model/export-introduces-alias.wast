;; RUN: wast --assert default --snapshot tests/snapshots %

(component
  (component
    (import "x" (func $f))

    (export $g "g" (func $f))
    (export $g2 "g2" (func $g))
  )
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

(assert_malformed
  (component quote
    "(type (instance"
      "(type $t u8)"
      "(export \"t\" (type $t (eq $t)))"
    "))"
  )
  "duplicate type identifier")

(component
  (type (instance
    (type $t u8)
    (export "t" (type $t2 (eq $t)))
    (export "t2" (type $t3 (eq $t2)))
  ))
)
