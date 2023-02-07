;; RUN: strip % -t

(module
  (func $foo)

  (@custom "foo" "...")
)
