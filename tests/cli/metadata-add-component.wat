;; RUN: metadata add --language foo=1 % | metadata show
(component $foo
  (core module
    (func $foo)
  )
)
