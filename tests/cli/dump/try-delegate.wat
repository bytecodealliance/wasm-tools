;; RUN: dump %

(module
  (func
    try $l
      try
      delegate $l
    end
  )
)
