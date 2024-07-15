;; --enable-legacy-exceptions
(module
  (type (;0;) (func))
  (func (;0;) (type 0)
    try ;; label = @1
      try ;; label = @2
        try ;; label = @3
          throw 0
        catch_all
          rethrow 0 (;@3;)
        end
      delegate 0 (;@2;)
    catch 0
    end
  )
  (tag (;0;) (type 0))
)
