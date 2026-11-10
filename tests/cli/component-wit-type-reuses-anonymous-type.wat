;; RUN: component wit %
;;
;; Types that reuse an anonymous type from another type's definition, such as
;; the `own` inside a `list<own<r>>`, are defined as that type rather than
;; decoded as a `use` of it.

(component
    ;; In an interface, of a resource it defines.
    (import "i" (instance
        (export "r" (type (sub resource)))
        (type (own 0))
        (type (list 1))
        (export "l" (type (eq 2)))
        (export "t" (type (eq 1)))
    ))

    ;; In an interface, of a resource it `use`s.
    (import "a:x/y" (instance $y (export "t" (type (sub resource)))))
    (alias export $y "t" (type $t))
    (import "a:x/k" (instance
        (alias outer 1 $t (type))
        (export "t" (type (eq 0)))
        (type (own 1))
        (type (list 2))
        (export "l" (type (eq 3)))
        (export "x" (type (eq 2)))
    ))

    ;; In an interface, of an anonymous type the world defines.
    (type $u (list u32))
    (import "f" (func (param "x" $u)))
    (import "a:x/z" (instance
        (alias outer 1 $u (type))
        (export "u" (type (eq 0)))
    ))

    ;; In the world.
    (import "r" (type $r (sub resource)))
    (type $o (own $r))
    (type $l (list $o))
    (import "l" (type (eq $l)))
    (import "t" (type (eq $o)))
)
