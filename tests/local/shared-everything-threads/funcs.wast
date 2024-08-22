;; Check that shared functions are valid WAT.
(module
  (type $f (shared (func)))
  (func (import "spectest" "shared-func") (type $f))
  (func (type $f))
)

;; Check that unshared functions cannot be called from shared ones.
(assert_invalid
  (module
    (type $shared (shared (func)))
    (type $unshared (func))
    (func (type $shared)
      call $unshared)
    (func $unshared (type $unshared))
  )
  "shared functions cannot call unshared functions")
