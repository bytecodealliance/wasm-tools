;; RUN: metadata show %
(component $my-name
  (core module $submodule)

  (core module (@name "another submodule"))
)
