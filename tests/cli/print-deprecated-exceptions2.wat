;; RUN: print %

(module $m
  (tag)
  (func
    try
    end
  )
  (func
    try
    catch 0
    end
  )
  (func
    try
    catch 0
      rethrow 0
    end
  )
  (func
    try
    catch_all
      rethrow 0
    end
  )
  (func
    try
    catch 0
    catch_all
      rethrow 0
    end
  )
  (func
    try
      try
      delegate 0
    catch 0
    end
  )
  (func (result i32)
    try (result i32)
      i32.const 42
    catch 0
      i32.const 42
    end
  )
)
