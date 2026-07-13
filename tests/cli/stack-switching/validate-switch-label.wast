;; RUN: wast --assert default --snapshot tests/snapshots % -f stack-switching

(assert_invalid
  (module
    (type $ft (func))
    (type $ct (cont $ft))

    (type $tag_ft (func (param i32)))
    (tag $t (type $tag_ft))

    (func
      block $label
        (resume $ct (on $t switch) (ref.null $ct))
        unreachable
      end
    )
  )
  "type mismatch: switch tag does not match continuation")

(assert_invalid
  (module
    (type $ft (func))
    (type $ct (cont $ft))

    (type $tag_ft (func (result i32)))
    (tag $t (type $tag_ft))

    (func
      block $label
        (resume $ct (on $t switch) (ref.null $ct))
        unreachable
      end
    )
  )
  "type mismatch: switch tag does not match continuation")

(assert_invalid
  (module
    (type $ft (func (result i64)))
    (type $ct (cont $ft))

    (type $tag_ft (func (result i32)))
    (tag $t (type $tag_ft))

    (func
      block $label
        (resume $ct (on $t switch) (ref.null $ct))
        unreachable
      end
    )
  )
  "type mismatch: switch tag does not match continuation")

(module
  (type $ft (func (result i32)))
  (type $ct (cont $ft))

  (type $tag_ft (func (result i32)))
  (tag $t (type $tag_ft))

  (func
    block $label
      (resume $ct (on $t switch) (ref.null $ct))
      unreachable
    end
  )
)

(assert_invalid
  (module
    (type $ft (func (result nullfuncref)))
    (type $ct (cont $ft))

    (type $tag_ft (func (result funcref)))
    (tag $t (type $tag_ft))

    (func
      block $label
        (resume $ct (on $t switch) (ref.null $ct))
        unreachable
      end
    )
  )
  "type mismatch: switch tag does not match continuation")

;; The switch tag's results must be *equivalent* to the continuation's results,
;; not merely a subtype. Here the tag results `[nullfuncref]` are a strict
;; subtype of the continuation results `[funcref]`, so this must be rejected.
(assert_invalid
  (module
    (type $ft (func (result funcref)))
    (type $ct (cont $ft))

    (type $tag_ft (func (result nullfuncref)))
    (tag $t (type $tag_ft))

    (func
      block $label
        (resume $ct (on $t switch) (ref.null $ct))
        unreachable
      end
    )
  )
  "type mismatch: switch tag does not match continuation")

;; Equivalent reference-type results (both `[funcref]`) are accepted.
(module
  (type $ft (func (result funcref)))
  (type $ct (cont $ft))

  (type $tag_ft (func (result funcref)))
  (tag $t (type $tag_ft))

  (func
    block $label
      (resume $ct (on $t switch) (ref.null $ct))
      unreachable
    end
  )
)
