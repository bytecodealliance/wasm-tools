;; --enable-exceptions

(module $m
  (type (func))
  (tag $exn (type 0))
  (func (try (do)))
  (func (try (do) (catch $exn)))
  (func (try (do) (catch $exn rethrow 0)))
  (func (try (do) (catch_all rethrow 0)))
  (func (try (do) (catch $exn) (catch_all rethrow 0)))
  (func (try (do (try (do) (delegate 0))) (catch $exn)))
  (func (result i32)
    (try (result i32)
      (do (i32.const 42))
      (catch $exn (i32.const 42)))))
