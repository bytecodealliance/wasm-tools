;; RUN: metadata add % --description updated | metadata add --description updated | print

(component
  (core module
    (@custom "description" "nested")
  )
  (@custom "description" "original")
  (@custom "version" "1.0")
)
