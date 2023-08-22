(component
  (type $x (resource (rep i32)))
)

(component
  (type $x (resource (rep i32)))

  (core func (canon resource.new $x))
  (core func (canon resource.rep $x))
  (core func (canon resource.drop $x))
)

(component
  (import "x" (type $x (sub resource)))

  (core func (canon resource.drop $x))
)

(component
  (core module $m
    (func (export "dtor") (param i32))
  )
  (core instance $m (instantiate $m))
  (type $x (resource (rep i32) (dtor (func $m "dtor"))))
  (core func (canon resource.new $x))
)

(component
  (type $x (resource (rep i32)))
  (core func $f1 (canon resource.new $x))
  (core func $f2 (canon resource.rep $x))
  (core func $f3 (canon resource.drop $x))

  (core module $m
    (import "" "f1" (func (param i32) (result i32)))
    (import "" "f2" (func (param i32) (result i32)))
    (import "" "f3" (func (param i32)))
  )

  (core instance (instantiate $m
    (with "" (instance
      (export "f1" (func $f1))
      (export "f2" (func $f2))
      (export "f3" (func $f3))
    ))
  ))
)

(assert_invalid
  (component
    (type $x (resource (rep i64)))
  )
  "resources can only be represented by `i32`")

(assert_invalid
  (component
    (type $x (own 100))
  )
  "type index out of bounds")

(assert_invalid
  (component
    (type $x (borrow 100))
  )
  "type index out of bounds")

(assert_invalid
  (component
    (type $t u8)
    (type $x (borrow $t))
  )
  "not a resource type")

(assert_invalid
  (component
    (type $t u8)
    (type $x (own $t))
  )
  "not a resource type")

(assert_invalid
  (component
    (import "x" (type $x (sub resource)))
    (core func (canon resource.new $x))
  )
  "not a local resource")

(assert_invalid
  (component
    (import "x" (type $x (sub resource)))
    (core func (canon resource.rep $x))
  )
  "not a local resource")

(assert_invalid
  (component
    (type $t (tuple u32))
    (core func (canon resource.drop $t))
  )
  "not a resource type")

(assert_invalid
  (component
    (core func (canon resource.drop 100))
  )
  "type index out of bounds")

(assert_invalid
  (component
    (type (component))
    (core func (canon resource.drop 0))
  )
  "not a resource type")

(assert_invalid
  (component
    (type (component))
    (core func (canon resource.new 0))
  )
  "not a resource type")

(assert_invalid
  (component
    (core module $m
      (func (export "dtor"))
    )
    (core instance $m (instantiate $m))
    (type $x (resource (rep i32) (dtor (func $m "dtor"))))
    (core func (canon resource.new $x))
  )
  "wrong signature for a destructor")

(assert_invalid
  (component
    (type (resource (rep i32) (dtor (func 100))))
  )
  "function index out of bounds")

(assert_invalid
  (component
    (import "x" (type $x (sub resource)))
    (import "y" (type $y (sub resource)))
    (import "z" (func $z (param "x" (own $x)) (param "y" (own $y))))

    (component $c
      (import "x" (type $x (sub resource)))
      (import "z" (func (param "x" (own $x)) (param "y" (own $x))))
    )

    (instance (instantiate $c (with "x" (type $x)) (with "z" (func $z))))
  )
  "resource types are not the same")

(component
  (type (component
    (import "x" (type $x (sub resource)))
    (export "y" (type (eq $x)))
    (export "z" (type (sub resource)))
  ))
)

(assert_invalid
  (component
    (type (component
      (type $x (resource (rep i32)))
    ))
  )
  "resources can only be defined within a concrete component")

(assert_invalid
  (component
    (type (instance
      (type $x (resource (rep i32)))
    ))
  )
  "resources can only be defined within a concrete component")

(component
  (type (component
    (import "x" (instance $i
      (export $t "t" (type (sub resource)))
      (export "f" (func (result "x" (own $t))))
    ))
    (alias export $i "t" (type $t))
    (export "f" (func (result "x" (own $t))))
  ))
)

(component
  (import "fancy-fs" (instance $fancy-fs
    (export $fs "fs" (instance
      (export "file" (type (sub resource)))
    ))
    (alias export $fs "file" (type $file))
    (export "fancy-op" (func (param "f" (borrow $file))))
  ))
)

(component $C
  (type $T (list (tuple string bool)))
  (type $U (option $T))
  (type $G (func (param "x" (list $T)) (result $U)))
  (type $D (component
    (alias outer $C $T (type $C_T))
    (type $L (list $C_T))
    (import "f" (func (param "x" $L) (result (list u8))))
    (import "g" (func (type $G)))
    (export "g2" (func (type $G)))
    (export "h" (func (result $U)))
    (import "T" (type $T (sub resource)))
    (import "i" (func (param "x" (list (own $T)))))
    (export $T' "T2" (type (eq $T)))
    (export $U' "U" (type (sub resource)))
    (export "j" (func (param "x" (borrow $T')) (result (own $U'))))
  ))
)

(component
  (import "T1" (type $T1 (sub resource)))
  (import "T2" (type $T2 (sub resource)))
)

(component $C
  (import "T1" (type $T1 (sub resource)))
  (import "T2" (type $T2 (sub resource)))
  (import "T3" (type $T3 (eq $T2)))
  (type $ListT1 (list (own $T1)))
  (type $ListT2 (list (own $T2)))
  (type $ListT3 (list (own $T3)))
)

(component
  (import "T" (type $T (sub resource)))
  (import "U" (type $U (sub resource)))
  (type $Own1 (own $T))
  (type $Own2 (own $T))
  (type $Own3 (own $U))
  (type $ListOwn1 (list $Own1))
  (type $ListOwn2 (list $Own2))
  (type $ListOwn3 (list $Own3))
  (type $Borrow1 (borrow $T))
  (type $Borrow2 (borrow $T))
  (type $Borrow3 (borrow $U))
  (type $ListBorrow1 (list $Borrow1))
  (type $ListBorrow2 (list $Borrow2))
  (type $ListBorrow3 (list $Borrow3))
)

(component
  (import "C" (component $C
    (export "T1" (type (sub resource)))
    (export $T2 "T2" (type (sub resource)))
    (export "T3" (type (eq $T2)))
  ))
  (instance $c (instantiate $C))
  (alias export $c "T1" (type $T1))
  (alias export $c "T2" (type $T2))
  (alias export $c "T3" (type $T3))
)

(component
  (component $C
    (type $r1 (export "r1") (resource (rep i32)))
    (type $r2 (export "r2") (resource (rep i32)))
  )
  (instance $c1 (instantiate $C))
  (instance $c2 (instantiate $C))
  (alias export $c1 "r1" (type $c1r1))
  (alias export $c1 "r2" (type $c1r2))
  (alias export $c2 "r1" (type $c2r1))
  (alias export $c2 "r2" (type $c2r2))
)

(component
  (type $r (resource (rep i32)))
  (export "r1" (type $r))
  (export "r2" (type $r))
)

(component
  (type (component
    (export "r1" (type (sub resource)))
    (export "r2" (type (sub resource)))
  ))
)

(component
  (type $r (resource (rep i32)))
  (export $r1 "r1" (type $r))
  (export "r2" (type $r1))
)

(component
  (type (component
    (export $r1 "r1" (type (sub resource)))
    (export "r2" (type (eq $r1)))
  ))
)

(component $P
  (import "C1" (component $C1
    (import "T" (type $T (sub resource)))
    (export "foo" (func (param "t" (own $T))))
  ))
  (import "C2" (component $C2
    (import "T" (type $T (sub resource)))
    (import "foo" (func (param "t" (own $T))))
  ))
  (type $R (resource (rep i32)))
  (instance $c1 (instantiate $C1 (with "T" (type $R))))
  (instance $c2 (instantiate $C2
    (with "T" (type $R))
    (with "foo" (func $c1 "foo"))
  ))
)

(component
  (import "C1" (component $C1
    (import "T1" (type $T1 (sub resource)))
    (import "T2" (type $T2 (sub resource)))
    (export "foo" (func (param "t" (tuple (own $T1) (own $T2)))))
  ))
  (import "C2" (component $C2
    (import "T" (type $T (sub resource)))
    (export "foo" (func (param "t" (tuple (own $T) (own $T)))))
  ))
  (type $R (resource (rep i32)))
  (instance $c1 (instantiate $C1
    (with "T1" (type $R))
    (with "T2" (type $R))
  ))
  (instance $c2 (instantiate $C2
    (with "T" (type $R))
    (with "foo" (func $c1 "foo"))
  ))
)

(assert_invalid
  (component
    (component $C
      (type $R (resource (rep i32)))
      (export "R" (type $R))
    )
    (instance $c (instantiate $C))
    (alias export $c "R" (type $R))
    (core func (canon resource.rep $R))
  )
  "not a local resource")

(component
  (component $C
    (type $R (resource (rep i32)))
    (export "R" (type $R))
  )
  (instance $c (instantiate $C))
  (alias export $c "R" (type $R))
  (core func (canon resource.drop $R))
)

(component
  (component $C1
    (import "X" (type (sub resource)))
  )
  (component $C2
    (import "C1" (component
      (import "X" (type (sub resource)))
    ))
  )
  (instance $c (instantiate $C2 (with "C1" (component $C1))))
)

(component
  (component $C1
    (import "X" (type $X (sub resource)))
    (import "f" (func $f (result (own $X))))
    (export "g" (func $f))
  )
  (component $C2
    (import "C1" (component
      (import "X" (type $X (sub resource)))
      (import "f" (func (result (own $X))))
      (export "g" (func (result (own $X))))
    ))
  )
  (instance $c (instantiate $C2 (with "C1" (component $C1))))
)

(component
  (component $C1
    (type $X' (resource (rep i32)))
    (export $X "X" (type $X'))

    (core func $f (canon resource.drop $X))
    (func (export "f") (param "X" (own $X)) (canon lift (core func $f)))
  )
  (instance $c1 (instantiate $C1))

  (component $C2
    (import "X" (type $X (sub resource)))
    (import "f" (func (param "X" (own $X))))
  )
  (instance $c2 (instantiate $C2
    (with "X" (type $c1 "X"))
    (with "f" (func $c1 "f"))
  ))
)

(assert_invalid
  (component
    (component $C1
      (type $X' (resource (rep i32)))
      (export $X "X" (type $X'))

      (core func $f (canon resource.drop $X))
      (func (export "f") (param "X" (own $X)) (canon lift (core func $f)))
    )
    (instance $c1 (instantiate $C1))
    (instance $c2 (instantiate $C1))

    (component $C2
      (import "X" (type $X (sub resource)))
      (import "f" (func (param "X" (own $X))))
    )
    (instance $c3 (instantiate $C2
      (with "X" (type $c1 "X"))
      (with "f" (func $c2 "f"))
    ))
  )
  "resource types are not the same")

(component
  (component $C1
    (type $X (resource (rep i32)))
    (export $X1 "X1" (type $X))
    (export $X2 "X2" (type $X))

    (core func $f (canon resource.drop $X))
    (func (export "f1") (param "X" (own $X1)) (canon lift (core func $f)))
    (func (export "f2") (param "X" (own $X2)) (canon lift (core func $f)))
  )
  (instance $c1 (instantiate $C1))

  (component $C2
    (import "X" (type $X (sub resource)))
    (import "f" (func (param "X" (own $X))))
  )
  (instance $c2 (instantiate $C2
    (with "X" (type $c1 "X1"))
    (with "f" (func $c1 "f1"))
  ))
  (instance $c3 (instantiate $C2
    (with "X" (type $c1 "X2"))
    (with "f" (func $c1 "f2"))
  ))
)

(component
  (component $C1
    (type $X (resource (rep i32)))
    (export $X1 "X1" (type $X))
    (export $X2 "X2" (type $X))

    (core func $f (canon resource.drop $X))
    (func (export "f1") (param "X" (own $X1)) (canon lift (core func $f)))
    (func (export "f2") (param "X" (own $X2)) (canon lift (core func $f)))
  )
  (instance $c1 (instantiate $C1))

  (component $C2
    (import "X" (type $X (sub resource)))
    (import "f" (func (param "X" (own $X))))
  )
  (instance $c2 (instantiate $C2
    (with "X" (type $c1 "X1"))
    (with "f" (func $c1 "f2"))
  ))
  (instance $c3 (instantiate $C2
    (with "X" (type $c1 "X2"))
    (with "f" (func $c1 "f1"))
  ))
)

(assert_invalid
  (component
    (component $c
      (import "x" (type (sub resource)))
    )
    (type $x u32)
    (instance (instantiate $c (with "x" (type $x))))
  )
  "expected resource, found type")

(assert_invalid
  (component
    (component $c
      (type $t u32)
      (import "x" (type (eq $t)))
    )
    (type $x (resource (rep i32)))
    (instance (instantiate $c (with "x" (type $x))))
  )
  "expected type, found resource")

(assert_invalid
  (component
    (component $c
      (import "x1" (type $x1 (sub resource)))
      (import "x2" (type $x2 (eq $x1)))
    )
    (type $x1 (resource (rep i32)))
    (type $x2 (resource (rep i32)))
    (instance (instantiate $c
      (with "x1" (type $x1))
      (with "x2" (type $x2))
    ))
  )
  "resource types are not the same")

(component
  (type $x (resource (rep i32)))
  (component $c
    (import "x" (type $t (sub resource)))
    (export "y" (type $t))
  )
  (instance $c (instantiate $c (with "x" (type $x))))

  (alias export $c "y" (type $x2))
  (core func (canon resource.rep $x2))

)

(assert_invalid
  (component
    (type $r (resource (rep i32)))
    (import "x" (func (result (own $r))))
  )
  "func not valid to be used as import")

(assert_invalid
  (component
    (type (component
      (export $x "x" (type (sub resource)))
      (import "f" (func (result (own $x))))
    ))
  )
  "func not valid to be used as import")

(assert_invalid
  (component
    (type $r (resource (rep i32)))

    (core func $f (canon resource.drop $r))
    (func (export "f") (param "x" (own $r))
      (canon lift (core func $f)))
  )
  "func not valid to be used as export")

;; direct exports count as "explicit in" for resources
(component
  (type $r' (resource (rep i32)))
  (export $r "r" (type $r'))

  (core func $f (canon resource.drop $r))
  (func (export "f") (param "x" (own $r))
    (canon lift (core func $f)))
)

;; instances-as-a-bundle count as "explicit in" for resources
(component
  (type $r' (resource (rep i32)))
  (instance $i'
    (export "r" (type $r'))
  )
  (export $i "i" (instance $i'))
  (alias export $i "r" (type $r))

  (core func $f (canon resource.drop $r))
  (func (export "f") (param "x" (own $r))
    (canon lift (core func $f)))
)

;; Transitive bundles count for "explicit in"
(component
  (type $r' (resource (rep i32)))
  (instance $i'
    (export "r" (type $r'))
  )
  (instance $i2'
    (export "i" (instance $i'))
  )
  (export $i2 "i2" (instance $i2'))
  (alias export $i2 "i" (instance $i))
  (alias export $i "r" (type $r))

  (core func $f (canon resource.drop $r))
  (func (export "f") (param "x" (own $r))
    (canon lift (core func $f)))
)

;; Component instantiations count for "explicit in"
(component
  (type $r' (resource (rep i32)))
  (component $C
    (import "x" (type $x (sub resource)))
    (export "y" (type $x))
  )
  (instance $c' (instantiate $C (with "x" (type $r'))))
  (export $c "c" (instance $c'))
  (alias export $c "y" (type $r))

  (core func $f (canon resource.drop $r))
  (func (export "f") (param "x" (own $r))
    (canon lift (core func $f)))
)

;; Make sure threading things around is valid for "explicit in"
(component
  (type $r' (resource (rep i32)))
  (component $C
    (import "x" (type $x (sub resource)))
    (export "y" (type $x))
  )
  (instance $c (instantiate $C (with "x" (type $r'))))
  (instance $i (export "x" (type $c "y")))

  (component $C2
    (import "x" (instance $i
      (export "i1" (instance
        (export "i2" (type (sub resource)))
      ))
    ))
    (export "y" (type $i "i1" "i2"))
  )

  (instance $i2 (export "i2" (type $i "x")))
  (instance $i1 (export "i1" (instance $i2)))
  (instance $c2 (instantiate $C2
    (with "x" (instance $i1))
  ))
  (export $r "x" (type $c2 "y"))

  (core func $f (canon resource.drop $r))
  (func (export "f") (param "x" (own $r))
    (canon lift (core func $f)))
)

;; Importing-and-exporting instances through instantiation counts for "explicit
;; in"
(component
  (type $r' (resource (rep i32)))
  (component $C
    (import "x" (instance $x (export "t" (type (sub resource)))))
    (export "y" (instance $x))
  )
  (instance $c' (instantiate $C
    (with "x" (instance
      (export "t" (type $r'))
    ))
  ))
  (export $c "c" (instance $c'))
  (alias export $c "y" (instance $y))
  (alias export $y "t" (type $r))

  (core func $f (canon resource.drop $r))
  (func (export "f") (param "x" (own $r))
    (canon lift (core func $f)))
)

(component
  (type $i (instance
    (export $r "r" (type (sub resource)))
    (export "f" (func (result (own $r))))
  ))
  (import "i1" (instance $i1 (type $i)))
  (import "i2" (instance $i2 (type $i)))

  (component $c
    (import "r" (type $t (sub resource)))
    (import "f" (func (result (own $t))))
  )
  (instance (instantiate $c
    (with "r" (type $i1 "r"))
    (with "f" (func $i1 "f"))
  ))
  (instance (instantiate $c
    (with "r" (type $i2 "r"))
    (with "f" (func $i2 "f"))
  ))
)


(assert_invalid
  (component
    (type $i (instance
      (export $r "r" (type (sub resource)))
      (export "f" (func (result (own $r))))
    ))
    (import "i1" (instance $i1 (type $i)))
    (import "i2" (instance $i2 (type $i)))

    (component $c
      (import "r" (type $t (sub resource)))
      (import "f" (func (result (own $t))))
    )
    (instance (instantiate $c
      (with "r" (type $i1 "r"))
      (with "f" (func $i2 "f"))
    ))
  )
  "resource types are not the same")

;; substitution works
(component
  (type $t (resource (rep i32)))
  (component $c
    (import "x" (type $t (sub resource)))
    (export "y" (type $t))
  )
  (instance $c1 (instantiate $c (with "x" (type $t))))
  (instance $c2 (instantiate $c (with "x" (type $t))))

  (component $c2
    (import "x1" (type $t (sub resource)))
    (import "x2" (type (eq $t)))
    (import "x3" (type (eq $t)))
  )
  (instance (instantiate $c2
    (with "x1" (type $t))
    (with "x2" (type $c1 "y"))
    (with "x3" (type $c2 "y"))
  ))
)

;; must supply a resource to instantiation
(assert_invalid
  (component
    (component $c
      (import "x" (type (sub resource)))
    )
    (instance (instantiate $c))
  )
  "missing import named `x`")
(assert_invalid
  (component
    (type $x (resource (rep i32)))
    (component $c
      (import "x" (type (sub resource)))
      (import "y" (type (sub resource)))
    )
    (instance (instantiate $c (with "x" (type $x))))
  )
  "missing import named `y`")

;; supply the wrong resource
(assert_invalid
  (component
    (type $x (resource (rep i32)))
    (type $y (resource (rep i32)))
    (component $c
      (import "x" (type $t (sub resource)))
      (import "y" (type (eq $t)))
    )
    (instance (instantiate $c
      (with "x" (type $x))
      (with "y" (type $y))
    ))
  )
  "resource types are not the same")

;; aliasing outer resources is ok
(component $A
  (type $C (component
    (import "x" (type $x (sub resource)))

    (type $y (component
      (alias outer $C $x (type $my-x))
      (import "x" (type (eq $my-x)))
    ))

    (import "y" (component (type $y)))
    (export "z" (component (type $y)))
  ))

  (type $t (resource (rep i32)))

  (alias outer $A $t (type $other-t))

  (type (instance (export "t" (type (eq $t)))))
  (type (component (export "t" (type (eq $t)))))
  (type (component (import "t" (type (eq $t)))))
)

;; aliasing beyond components, however, is not ok
(assert_invalid
  (component $A
    (type $t (resource (rep i32)))
    (component (alias outer $A $t (type $foo)))
  )
  "refers to resources not defined in the current component")
(assert_invalid
  (component $A
    (type $t (resource (rep i32)))
    (type $u (record (field "x" (own $t))))
    (component (alias outer $A $u (type $foo)))
  )
  "refers to resources not defined in the current component")
(assert_invalid
  (component $A
    (type $t (resource (rep i32)))
    (type $u (borrow $t))
    (component (alias outer $A $u (type $foo)))
  )
  "refers to resources not defined in the current component")
(assert_invalid
  (component $A
    (type $t (resource (rep i32)))
    (type $u (component (export "a" (type (eq $t)))))
    (component (alias outer $A $u (type $foo)))
  )
  "refers to resources not defined in the current component")
(assert_invalid
  (component $A
    (type $t (resource (rep i32)))
    (type $u (component (import "a" (type (eq $t)))))
    (component (alias outer $A $u (type $foo)))
  )
  "refers to resources not defined in the current component")

(assert_invalid
  (component
    (component $X
      (type $t (resource (rep i32)))
      (export "t" (type $t))
    )
    (component $F
      (import "x" (component (export $t "t" (type (sub resource)))))
    )
    (instance $x1 (instantiate $X))
    (instance $f1 (instantiate $F (with "x" (instance $x1))))
  )
  "expected component, found instance")

;; Show that two instantiations of the same component produce unique exported
;; resource types.
(assert_invalid
  (component
    (component $F
      (type $t1 (resource (rep i32)))
      (export "t1" (type $t1))
    )
    (instance $f1 (instantiate $F))
    (instance $f2 (instantiate $F))
    (alias export $f1 "t1" (type $t1))
    (alias export $f2 "t1" (type $t2))
    (component $T
      (import "x" (type $x (sub resource)))
      (import "y" (type (eq $x)))
    )
    (instance $test
      (instantiate $T (with "x" (type $t1)) (with "y" (type $t2))))
  )
  "type mismatch for import `y`")

;; Show that re-exporting imported resources from an imported component doesn't
;; change the identity of that resource.
(component
  (component $X
    (type $t (resource (rep i32)))
    (export "t" (type $t))
  )
  (component $F
    (import "x" (instance $i (export $t "t" (type (sub resource)))))
    (alias export $i "t" (type $t))
    (export "t" (type $t))
  )
  (instance $x1 (instantiate $X))
  (instance $f1 (instantiate $F (with "x" (instance $x1))))
  (instance $f2 (instantiate $F (with "x" (instance $x1))))
  (alias export $f1 "t" (type $t1))
  (alias export $f2 "t" (type $t2))
  (component $T
    (import "x" (type $x (sub resource)))
    (import "y" (type (eq $x)))
  )
  (instance $test
    (instantiate $T (with "x" (type $t1)) (with "y" (type $t2))))
)

(assert_invalid
  (component (import "[static]" (func)))
  "failed to find `.` character")

;; validation of `[constructor]foo`
(assert_invalid
  (component (import "[constructor]" (func)))
  "not in kebab case")
(assert_invalid
  (component (import "[constructor]a" (func)))
  "should return one value")
(assert_invalid
  (component (import "[constructor]a" (func (result u32))))
  "should return `(own $T)`")
(assert_invalid
  (component
    (import "b" (type $a (sub resource)))
    (import "[constructor]a" (func (result (own $a)))))
  "import name `[constructor]a` is not valid")
(assert_invalid
  (component
    (import "b" (type $a (sub resource)))
    (import "[constructor]a" (func (result (own $a)))))
  "function does not match expected resource name `b`")
(component
  (import "a" (type $a (sub resource)))
  (import "[constructor]a" (func (result (own $a)))))
(component
  (import "a" (type $a (sub resource)))
  (import "[constructor]a" (func (param "x" u32) (result (own $a)))))

;; validation of `[method]a.b`
(assert_invalid
  (component (import "[method]" (func)))
  "failed to find `.` character")
(assert_invalid
  (component (import "[method]a" (func)))
  "failed to find `.` character")
(assert_invalid
  (component (import "[method]a." (func)))
  "not in kebab case")
(assert_invalid
  (component (import "[method].a" (func)))
  "not in kebab case")
(assert_invalid
  (component (import "[method]a.b.c" (func)))
  "not in kebab case")
(assert_invalid
  (component (import "[method]a.b" (instance)))
  "is not a func")
(assert_invalid
  (component (import "[method]a.b" (func)))
  "should have at least one argument")
(assert_invalid
  (component (import "[method]a.b" (func (param "x" u32))))
  "should have a first argument called `self`")
(assert_invalid
  (component (import "[method]a.b" (func (param "self" u32))))
  "should take a first argument of `(borrow $T)`")
(assert_invalid
  (component
    (import "b" (type $T (sub resource)))
    (import "[method]a.b" (func (param "self" (borrow $T)))))
  "does not match expected resource name")
(component
  (import "a" (type $T (sub resource)))
  (import "[method]a.b" (func (param "self" (borrow $T)))))

;; validation of `[static]a.b`
(assert_invalid
  (component (import "[static]" (func)))
  "failed to find `.` character")
(assert_invalid
  (component (import "[static]a" (func)))
  "failed to find `.` character")
(assert_invalid
  (component (import "[static]a." (func)))
  "not in kebab case")
(assert_invalid
  (component (import "[static].a" (func)))
  "not in kebab case")
(assert_invalid
  (component (import "[static]a.b.c" (func)))
  "not in kebab case")
(assert_invalid
  (component (import "[static]a.b" (instance)))
  "is not a func")
(assert_invalid
  (component (import "[static]a.b" (func)))
  "static resource name is not known in this context")

(component
  (import "a" (type (sub resource)))
  (import "[static]a.b" (func)))

;; exports/imports are disjoint
(assert_invalid
  (component
    (import "b" (type $T (sub resource)))
    (import "f" (func $f (param "self" (borrow $T))))
    (export "[method]b.foo" (func $f))
  )
  "resource used in function does not have a name in this context")

(component
  (import "b" (type $T (sub resource)))
  (import "f" (func $f (param "self" (borrow $T))))
  (export $c "c" (type $T))
  (export "[method]c.foo" (func $f) (func (param "self" (borrow $c))))
)

;; imports aren't transitive
(assert_invalid
  (component
    (import "i" (instance $i
      (export "t" (type (sub resource)))
    ))
    (alias export $i "t" (type $t))
    (import "[method]t.foo" (func (param "self" (borrow $t))))
  )
  "resource used in function does not have a name in this context")

;; validation happens in a type context
(assert_invalid
  (component
    (type (component
      (import "b" (type $T (sub resource)))
      (import "[constructor]a" (func (result (own $T))))
    ))
  )
  "function does not match expected resource name `b`")

;; bag-of-exports validation
(assert_invalid
  (component
    (type $T (resource (rep i32)))
    (core module $m (func (export "a") (result i32) unreachable))
    (core instance $i (instantiate $m))
    (func $f (result (own $T)) (canon lift (core func $i "a")))
    (instance
      (export "a" (type $T))
      (export "[constructor]a" (func $f))
    )
  )
  "resource used in function does not have a name in this context")

(component
  (component $C)
  (instance (instantiate $C (with "this is not kebab case" (component $C))))
)

;; Test that unused arguments to instantiation are not validated to have
;; appropriate types with respect to kebab naming conventions which require
;; functions/interfaces/etc.
(component
  (component $C)
  (instance (instantiate $C (with "[method]foo.bar" (component $C))))
)

;; thread a resource through a few layers
(component
  (component $C
    (import "in" (type $r (sub resource)))
    (export "out" (type $r))
  )

  (type $r (resource (rep i32)))

  (instance $c1 (instantiate $C (with "in" (type $r))))
  (instance $c2 (instantiate $C (with "in" (type $c1 "out"))))
  (instance $c3 (instantiate $C (with "in" (type $c2 "out"))))
  (instance $c4 (instantiate $C (with "in" (type $c3 "out"))))
  (instance $c5 (instantiate $C (with "in" (type $c4 "out"))))

  (component $C2
    (import "in1" (type $r (sub resource)))
    (import "in2" (type (eq $r)))
    (import "in3" (type (eq $r)))
    (import "in4" (type (eq $r)))
    (import "in5" (type (eq $r)))
    (import "in6" (type (eq $r)))
  )

  (instance (instantiate $C2
    (with "in1" (type $r))
    (with "in2" (type $c1 "out"))
    (with "in3" (type $c2 "out"))
    (with "in4" (type $c3 "out"))
    (with "in5" (type $c4 "out"))
    (with "in6" (type $c5 "out"))
  ))
)

;; exporting an instance type "freshens" resources
(assert_invalid
  (component
    (import "x" (instance $i
      (type $i (instance
        (export "r" (type (sub resource)))
      ))
      (export "a" (instance (type $i)))
      (export "b" (instance (type $i)))
    ))

    (component $C
      (import "x" (type $x (sub resource)))
      (import "y" (type (eq $x)))
    )
    (instance (instantiate $C
      (with "x" (type $i "a" "r"))
      (with "y" (type $i "b" "r"))
    ))
  )
  "resource types are not the same")

(component
  (type (export "x") (component
    (type $t' (instance
      (export "r" (type (sub resource)))
    ))
    (export $t "t" (instance (type $t')))
    (alias export $t "r" (type $r))
    (type $t2' (instance
      (export "r2" (type (eq $r)))
      (export "r" (type (sub resource)))
    ))
    (export "t2" (instance (type $t2')))
  ))
)

(component
  (type (component
    (type (instance
      (export "bar" (type (sub resource)))
      (export "[static]bar.a" (func))
    ))
    (export "x" (instance (type 0)))
  ))
)

(assert_invalid
  (component
    (type $r (resource (rep i32)))
    (type (func (result (borrow $r))))
  )
  "function result cannot contain a `borrow` type")
(assert_invalid
  (component
    (type $r (resource (rep i32)))
    (type (func (result (list (borrow $r)))))
  )
  "function result cannot contain a `borrow` type")
(assert_invalid
  (component
    (type $r (resource (rep i32)))
    (type (func (result (option (borrow $r)))))
  )
  "function result cannot contain a `borrow` type")
(assert_invalid
  (component
    (type $r (resource (rep i32)))
    (type $t (record (field "f" (borrow $r))))
    (type (func (result (option (list $t)))))
  )
  "function result cannot contain a `borrow` type")
