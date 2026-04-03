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
