;; RUN: metadata add --language foo % | metadata show
(component $foo
  (core module
    (func $foo)
  )
)
