;; --enable-exceptions

(module $m
  (func (try (do) (catch drop)))
  (func (try (do) (catch rethrow)))
  (func (result i32)
    (try (result i32)
      (do (i32.const 42))
      (catch drop (i32.const 42)))))
