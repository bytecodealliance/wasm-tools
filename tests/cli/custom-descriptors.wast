;; RUN: wast --assert default --snapshot tests/snapshots % -f custom-descriptors

;; --enable-gc

(module
  (rec
    (type $t1 (descriptor $t2) (struct (field i32)))
    (type $t2 (describes $t1) (struct (field (ref $f))))
    (type $f (func (result i32)))
  )
)

(assert_invalid
  (module
    (rec
      (type $f (func (result i32)))
      (type $t2 (describes $t1) (struct (field (ref $f))))
      (type $t1 (descriptor $t2) (struct (field i32)))
    )
  )
  "forward describes reference"
)

(assert_invalid
  (module
    (rec
      (type $t1 (descriptor $t2) (array f32))
      (type $t2 (describes $t1) (struct (field funcref)))
    )
  )
  "descriptor clause on non-struct type"
)

(assert_invalid
  (module
    (rec
      (type $t1 (descriptor $t2) (struct (field i32)))
      (type $t2 (describes $t1) (func))
    )
  )
  "describes clause on non-struct type"
)

(module
  (rec
    (type $t1 (sub (descriptor $t2) (struct (field i32))))
    (type $t2 (sub (describes $t1) (struct (field (ref $f)))))
    (type $t1sub (sub $t1 (descriptor $t2sub) (struct (field i32 f32))))
    (type $t2sub (sub $t2 (describes $t1sub) (struct (field (ref $f) (ref $f)))))
    (type $f (func (result i32)))
  )
)

(assert_invalid
  (module
    (rec
      (type $t1 (sub (descriptor $t2) (struct (field i32))))
      (type $t2 (sub (describes $t1) (struct (field (ref $f)))))
      (type $t1sub (sub $t1 (descriptor $t2sub) (struct (field i32 f32))))
      (type $t2sub (sub (describes $t1sub) (struct (field (ref $f) (ref $f)))))
      (type $f (func (result i32)))
    )
  )
  "supertype of described type must be described by supertype of descriptor"
)

(assert_invalid
  (module
    (rec
      (type $t1 (sub (descriptor $t2) (struct (field i32))))
      (type $t2 (sub (describes $t1) (struct (field (ref $f)))))
      (type $t1n (sub (descriptor $t2n) (struct (field i64))))
      (type $t2n (sub (describes $t1n) (struct (field (ref $f)))))
      (type $t1sub (sub $t1 (descriptor $t2sub) (struct (field i32 f32))))
      (type $t2sub (sub $t2n (describes $t1sub) (struct (field (ref $f) (ref $f)))))
      (type $f (func (result i32)))
    )
  )
  "supertype of described type must be described by supertype of descriptor"
)

(assert_invalid
  (module
    (rec
      (type $t1 (sub (descriptor $t2) (struct (field i32))))
      (type $t2 (sub (describes $t1) (struct (field (ref $f)))))
      (type $t1sub (sub $t1 (struct (field i32 f32))))
      (type $t2sub (sub $t2 (describes $t1sub) (struct (field (ref $f) (ref $f)))))
      (type $f (func (result i32)))
    )
  )
  "supertype of type without descriptor cannot have descriptor"
)

(assert_invalid
  (module
    (rec
      (type $t1 (sub (descriptor $t2) (struct (field i32))))
      (type $t2 (sub (describes $t1) (struct (field (ref $f)))))
      (type $t1sub (sub $t1 (descriptor $t2sub) (struct (field i32 f32))))
      (type $t2sub (sub $t2 (describes $t1sub) (struct (field (ref $f) (ref $f)))))
      (type $t2sub-n (sub $t2sub (struct (field (ref $f) (ref $f) (ref $f)))))
      (type $f (func (result i32)))
    )
  )
  "supertype of non-descriptor type cannot be a descriptor"
)

(assert_invalid
  (module
    (rec
      (type $t0 (sub (struct (field (ref $f)))))
      (type $t1 (sub (descriptor $t2) (struct (field i32))))
      (type $t2 (sub $t0 (describes $t1) (struct (field (ref $f)))))
      (type $f (func (result i32)))
    )
  )
  "supertype of descriptor must be a descriptor"
)

(assert_invalid
  (module
    (rec
      (type $f (func (result i32)))
      (type $t1 (descriptor $t2) (struct (field i32)))
      (type $t2 (struct (field (ref $f))))
    )
  )
  "descriptor with no matching describes"
)

(assert_invalid
  (module
    (rec
      (type $f (func (result i32)))
      (type $t1 (descriptor $t2) (struct (field i32)))
      (type $t1b (descriptor $t2) (struct (field i32)))
      (type $t2 (describes $t1b) (struct (field (ref $f))))
    )
  )
  "descriptor with no matching describes"
)


(assert_invalid
  (module
    (rec
      (type $f (func (result i32)))
      (type $t1 (struct (field i32)))
      (type $t2 (describes $t1) (struct (field (ref $f))))
    )
  )
  "describes with no matching descriptor"
)

(assert_invalid
  (module
    (rec
      (type $f (func (result i32)))
      (type $t1 (descriptor $t2) (struct (field i32)))
      (type $t2 (describes $t1) (struct (field (ref $f))))
      (type $t2b (describes $t1) (struct (field (ref $f))))
    )
  )
  "describes with no matching descriptor"
)

(assert_invalid
  (module
    (type $f (func))
    (type $g (func))
    (rec
      (type $A (descriptor $A.desc) (struct))
      (type $A.desc (describes $A) (struct)))
    (func (param anyref) (param (ref null $A)) (result (ref null $A))
      local.get 0
      local.get 1
      ref.cast_desc_eq (ref null $A)))
  "type mismatch")

;; `ref.get_desc` on a bottom operand in unreachable code produces an exact
;; descriptor reference, including when the operand is `(ref bot)` as produced
;; by `ref.as_non_null` or `br_on_null`.
(module
  (rec
    (type $t (descriptor $d) (struct))
    (type $d (describes $t) (struct)))

  (func (result (ref (exact $d)))
    unreachable
    ref.get_desc $t)

  (func (result (ref (exact $d)))
    unreachable
    ref.as_non_null
    ref.get_desc $t)

  (func (result (ref (exact $d)))
    block
      unreachable
      br_on_null 0
      ref.get_desc $t
      return
    end
    unreachable)
)
