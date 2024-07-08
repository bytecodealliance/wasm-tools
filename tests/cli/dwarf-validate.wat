;; FAIL: validate -g %

(module
  (memory 1)
  (func $user-name
    i32.load
  )
)
