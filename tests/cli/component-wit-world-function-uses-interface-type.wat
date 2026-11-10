;; RUN: component wit %
;;
;; The world's functions refer to `a:b/i`'s types directly, without the type
;; imports that a world's `use` encodes as, so the decoded world gets the
;; `use`s.

(component
    (type $ii (instance
        (export "r" (type (sub resource)))
        (type (flags "a"))
        (export "t" (type (eq 1)))
    ))
    (import "a:b/i" (instance $i (type $ii)))
    (alias export $i "r" (type $r))
    (alias export $i "t" (type $t))
    (import "f" (func (param "x" (own $r)) (param "y" $t)))
    (import "g" (func (result (list $t))))
)
