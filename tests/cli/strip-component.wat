;; RUN: strip % -t -a
(component $the-component
  (core module
    (func $foo)
  )
)
