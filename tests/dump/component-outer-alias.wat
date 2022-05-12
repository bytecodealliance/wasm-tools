(component
  (type $t string)

  (type (component
    (type (value $t))
  ))
  (component
    (type (value $t))
  )

  ;; should only have one alias in here
  (component
    (type (value $t))
    (type (value $t))
  )

  ;; can work with later types too
  (type $t2 u8)
  (component
    (type (value $t2))
    (type (value $t2))
  )
)
