;; RUN: dump %

(component
  (type $t string)

  (type (component
    (type (func (result $t)))
  ))
  (component
    (type (func (result $t)))
  )

  ;; should only have one alias in here
  (component
    (type (func (result $t)))
    (type (func (result $t)))
  )

  ;; can work with later types too
  (type $t2 u8)
  (component
    (type (func (result $t2)))
    (type (func (result $t2)))
  )
)
