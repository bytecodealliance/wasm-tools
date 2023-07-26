(component
  (import (interface "a:b/type-def") (instance $type-def
    (type $r1' (record (field "f" u32)))
    (export $r1 "r1" (type (eq $r1')))
    (type $r2' (record (field "r1" $r1)))
    (export $r2 "r2" (type (eq $r2')))
  ))
  (alias export $type-def "r1" (type $r1))
  (alias export $type-def "r2" (type $r2))
  (import (interface "a:b/type-use") (instance
    (export "r1" (type (eq $r1)))
    (export "r2" (type (eq $r2)))
  ))

  (import (interface "a:b/type-use-in-same-interface") (instance
    (type $t1' u32)
    (export $t1 "t1" (type (eq $t1')))
    (export $t2 "t2" (type (eq $t1)))
  ))

  (import (interface "a:b/diamond-base") (instance $diamond-base
    (type $t u32)
    (export "t" (type (eq $t)))
  ))
  (alias export $diamond-base "t" (type $t-diamond-base))

  (import (interface "a:b/diamond1") (instance $diamond1
    (export "t" (type (eq $t-diamond-base)))
  ))
  (import (interface "a:b/diamond2") (instance $diamond2
    (export "t" (type (eq $t-diamond-base)))
  ))

  (alias export $diamond1 "t" (type $t-diamond1))
  (alias export $diamond2 "t" (type $t-diamond2))

  (import (interface "a:b/diamond-join") (instance
    (export "t1" (type (eq $t-diamond1)))
    (export "t2" (type (eq $t-diamond2)))
  ))
)
