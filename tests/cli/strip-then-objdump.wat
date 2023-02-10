;; RUN: strip % | objdump

(module
  (func $name)

  (@custom "some-custom" "hello")
)
