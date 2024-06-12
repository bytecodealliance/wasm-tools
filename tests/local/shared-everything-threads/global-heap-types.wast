;; Check shared attributes for global heap types.

;; Concrete heap types cannot be marked shared yet (TODO: this is only possible
;; once composite types can be marked shared).
;;
;; (assert_invalid
;;   (module
;;     (type $t (shared anyref))
;;     (global (shared (ref null (shared $t))))))
;;   "shared value type")

;; `func` references.
(module
  ;; Imported (long/short forms, mut, null).
  (global (import "spectest" "global_funcref") (shared (shared funcref)))
  (global (import "spectest" "global_mut_funcref") (shared mut (shared funcref)))
  (global (import "spectest" "global_ref_null_func") (shared (ref null (shared func))))
  (global (import "spectest" "global_mut_ref_null_func") (shared mut (ref null (shared func))))
  (global (import "spectest" "global_ref_func") (shared (ref (shared func))))
  (global (import "spectest" "global_mut_ref_func") (shared mut (ref (shared func))))

  ;; Initialized (long/short form).
  (global (shared (shared funcref)) (ref.null (shared func)))
  (global (shared (ref null (shared func))) (ref.null (shared func)))
)

(assert_invalid
  (module (global (shared funcref)))
  "shared globals must have a shared value type")
(assert_invalid
  (module (global (shared (ref null func))))
  "shared globals must have a shared value type")
(assert_invalid
  (module (global (ref (shared func))))
  "type mismatch")

;; `extern` references.
(module
  ;; Imported (long/short forms, mut, null).
  (global (import "spectest" "global_externref") (shared (shared externref)))
  (global (import "spectest" "global_mut_externref") (shared mut (shared externref)))
  (global (import "spectest" "global_ref_null_extern") (shared (ref null (shared extern))))
  (global (import "spectest" "global_mut_ref_null_extern") (shared mut (ref null (shared extern))))
  (global (import "spectest" "global_ref_extern") (shared (ref (shared extern))))
  (global (import "spectest" "global_mut_ref_extern") (shared mut (ref (shared extern))))

  ;; Initialized (long/short form).
  (global (shared (shared externref)) (ref.null (shared extern)))
  (global (shared (ref null (shared extern))) (ref.null (shared extern)))
)

(assert_invalid
  (module (global (shared externref)))
  "shared globals must have a shared value type")
(assert_invalid
  (module (global (shared (ref null extern))))
  "shared globals must have a shared value type")
(assert_invalid
  (module (global (ref (shared extern))))
  "type mismatch")

;; `exn` references.
(module
  ;; Imported (long/short forms, mut, null).
  (global (import "spectest" "global_exnref") (shared (shared exnref)))
  (global (import "spectest" "global_mut_exnref") (shared mut (shared exnref)))
  (global (import "spectest" "global_ref_null_exn") (shared (ref null (shared exn))))
  (global (import "spectest" "global_mut_ref_null_exn") (shared mut (ref null (shared exn))))
  (global (import "spectest" "global_ref_exn") (shared (ref (shared exn))))
  (global (import "spectest" "global_mut_ref_exn") (shared mut (ref (shared exn))))

  ;; Initialized (long/short form).
  (global (shared (shared exnref)) (ref.null (shared exn)))
  (global (shared (ref null (shared exn))) (ref.null (shared exn)))
)

(assert_invalid
  (module (global (shared exnref)))
  "shared globals must have a shared value type")
(assert_invalid
  (module (global (shared (ref null exn))))
  "shared globals must have a shared value type")
(assert_invalid
  (module (global (ref (shared exn))))
  "type mismatch")

;; `any` references.
(module
  ;; Imported (long/short forms, mut, null).
  (global (import "spectest" "global_anyref") (shared (shared anyref)))
  (global (import "spectest" "global_mut_anyref") (shared mut (shared anyref)))
  (global (import "spectest" "global_ref_null_any") (shared (ref null (shared any))))
  (global (import "spectest" "global_mut_ref_null_any") (shared mut (ref null (shared any))))
  (global (import "spectest" "global_ref_any") (shared (ref (shared any))))
  (global (import "spectest" "global_mut_ref_any") (shared mut (ref (shared any))))

  ;; Initialized (long/short form).
  (global (shared (shared anyref)) (ref.null (shared any)))
  (global (shared (ref null (shared any))) (ref.null (shared any)))
)

(assert_invalid
  (module (global (shared anyref)))
  "shared globals must have a shared value type")
(assert_invalid
  (module (global (shared (ref null any))))
  "shared globals must have a shared value type")
(assert_invalid
  (module (global (ref (shared any))))
  "type mismatch")

;; `eq` references.
(module
  ;; Imported (long/short forms, mut, null).
  (global (import "spectest" "global_eqref") (shared (shared eqref)))
  (global (import "spectest" "global_mut_eqref") (shared mut (shared eqref)))
  (global (import "spectest" "global_ref_null_eq") (shared (ref null (shared eq))))
  (global (import "spectest" "global_mut_ref_null_eq") (shared mut (ref null (shared eq))))
  (global (import "spectest" "global_ref_eq") (shared (ref (shared eq))))
  (global (import "spectest" "global_mut_ref_eq") (shared mut (ref (shared eq))))

  ;; Initialized (long/short form).
  (global (shared (shared eqref)) (ref.null (shared eq)))
  (global (shared (ref null (shared eq))) (ref.null (shared eq)))
)

(assert_invalid
  (module (global (shared eqref)))
  "shared globals must have a shared value type")
(assert_invalid
  (module (global (shared (ref null eq))))
  "shared globals must have a shared value type")
(assert_invalid
  (module (global (ref (shared eq))))
  "type mismatch")

;; `struct` references.
(module
  ;; Imported (long/short forms, mut, null).
  (global (import "spectest" "global_structref") (shared (shared structref)))
  (global (import "spectest" "global_mut_structref") (shared mut (shared structref)))
  (global (import "spectest" "global_ref_null_struct") (shared (ref null (shared struct))))
  (global (import "spectest" "global_mut_ref_null_struct") (shared mut (ref null (shared struct))))
  (global (import "spectest" "global_ref_struct") (shared (ref (shared struct))))
  (global (import "spectest" "global_mut_ref_struct") (shared mut (ref (shared struct))))

  ;; Initialized (long/short form).
  (global (shared (shared structref)) (ref.null (shared struct)))
  (global (shared (ref null (shared struct))) (ref.null (shared struct)))
)

(assert_invalid
  (module (global (shared structref)))
  "shared globals must have a shared value type")
(assert_invalid
  (module (global (shared (ref null struct))))
  "shared globals must have a shared value type")
(assert_invalid
  (module (global (ref (shared struct))))
  "type mismatch")

;; `array` references.
(module
  ;; Imported (long/short forms, mut, null).
  (global (import "spectest" "global_arrayref") (shared (shared arrayref)))
  (global (import "spectest" "global_mut_arrayref") (shared mut (shared arrayref)))
  (global (import "spectest" "global_ref_null_array") (shared (ref null (shared array))))
  (global (import "spectest" "global_mut_ref_null_array") (shared mut (ref null (shared array))))
  (global (import "spectest" "global_ref_array") (shared (ref (shared array))))
  (global (import "spectest" "global_mut_ref_array") (shared mut (ref (shared array))))

  ;; Initialized (long/short form).
  (global (shared (shared arrayref)) (ref.null (shared array)))
  (global (shared (ref null (shared array))) (ref.null (shared array)))
)

(assert_invalid
  (module (global (shared arrayref)))
  "shared globals must have a shared value type")
(assert_invalid
  (module (global (shared (ref null array))))
  "shared globals must have a shared value type")
(assert_invalid
  (module (global (ref (shared array))))
  "type mismatch")

;; `i31` references.
(module
  ;; Imported (long/short forms, mut, null).
  (global (import "spectest" "global_i31ref") (shared (shared i31ref)))
  (global (import "spectest" "global_mut_i31ref") (shared mut (shared i31ref)))
  (global (import "spectest" "global_ref_null_i31") (shared (ref null (shared i31))))
  (global (import "spectest" "global_mut_ref_null_i31") (shared mut (ref null (shared i31))))
  (global (import "spectest" "global_ref_i31") (shared (ref (shared i31))))
  (global (import "spectest" "global_mut_ref_i31") (shared mut (ref (shared i31))))

  ;; Initialized (long/short form).
  (global (shared (shared i31ref)) (ref.null (shared i31)))
  (global (shared (ref null (shared i31))) (ref.null (shared i31)))
)

(assert_invalid
  (module (global (shared i31ref)))
  "shared globals must have a shared value type")
(assert_invalid
  (module (global (shared (ref null i31))))
  "shared globals must have a shared value type")
(assert_invalid
  (module (global (ref (shared i31))))
  "type mismatch")

;; `nofunc` references.
(module
  ;; Imported (long/short forms, mut, null).
  (global (import "spectest" "global_nullfuncref") (shared (shared nullfuncref)))
  (global (import "spectest" "global_mut_nullfuncref") (shared mut (shared nullfuncref)))
  (global (import "spectest" "global_ref_null_nofunc") (shared (ref null (shared nofunc))))
  (global (import "spectest" "global_mut_ref_null_nofunc") (shared mut (ref null (shared nofunc))))
  (global (import "spectest" "global_ref_nofunc") (shared (ref (shared nofunc))))
  (global (import "spectest" "global_mut_ref_nofunc") (shared mut (ref (shared nofunc))))

  ;; Initialized (long/short form).
  (global (shared (shared nullfuncref)) (ref.null (shared nofunc)))
  (global (shared (ref null (shared nofunc))) (ref.null (shared nofunc)))
)

(assert_invalid
  (module (global (shared nullfuncref)))
  "shared globals must have a shared value type")
(assert_invalid
  (module (global (shared (ref null nofunc))))
  "shared globals must have a shared value type")
(assert_invalid
  (module (global (ref (shared nofunc))))
  "type mismatch")

;; `noextern` references.
(module
  ;; Imported (long/short forms, mut, null).
  (global (import "spectest" "global_nullexternref") (shared (shared nullexternref)))
  (global (import "spectest" "global_mut_nullexternref") (shared mut (shared nullexternref)))
  (global (import "spectest" "global_ref_null_noextern") (shared (ref null (shared noextern))))
  (global (import "spectest" "global_mut_ref_null_noextern") (shared mut (ref null (shared noextern))))
  (global (import "spectest" "global_ref_noextern") (shared (ref (shared noextern))))
  (global (import "spectest" "global_mut_ref_noextern") (shared mut (ref (shared noextern))))

  ;; Initialized (long/short form).
  (global (shared (shared nullexternref)) (ref.null (shared noextern)))
  (global (shared (ref null (shared noextern))) (ref.null (shared noextern)))
)

(assert_invalid
  (module (global (shared nullexternref)))
  "shared globals must have a shared value type")
(assert_invalid
  (module (global (shared (ref null noextern))))
  "shared globals must have a shared value type")
(assert_invalid
  (module (global (ref (shared noextern))))
  "type mismatch")

;; `noexn` references.
(module
  ;; Imported (long/short forms, mut, null).
  (global (import "spectest" "global_nullexnref") (shared (shared nullexnref)))
  (global (import "spectest" "global_mut_nullexnref") (shared mut (shared nullexnref)))
  (global (import "spectest" "global_ref_null_noexn") (shared (ref null (shared noexn))))
  (global (import "spectest" "global_mut_ref_null_noexn") (shared mut (ref null (shared noexn))))
  (global (import "spectest" "global_ref_noexn") (shared (ref (shared noexn))))
  (global (import "spectest" "global_mut_ref_noexn") (shared mut (ref (shared noexn))))

  ;; Initialized (long/short form).
  (global (shared (shared nullexnref)) (ref.null (shared noexn)))
  (global (shared (ref null (shared noexn))) (ref.null (shared noexn)))
)

(assert_invalid
  (module (global (shared nullexnref)))
  "shared globals must have a shared value type")
(assert_invalid
  (module (global (shared (ref null noexn))))
  "shared globals must have a shared value type")
(assert_invalid
  (module (global (ref (shared noexn))))
  "type mismatch")

;; `none` references.
(module
  ;; Imported (long/short forms, mut, null).
  (global (import "spectest" "global_nullref") (shared (shared nullref)))
  (global (import "spectest" "global_mut_nullref") (shared mut (shared nullref)))
  (global (import "spectest" "global_ref_null_none") (shared (ref null (shared none))))
  (global (import "spectest" "global_mut_ref_null_none") (shared mut (ref null (shared none))))
  (global (import "spectest" "global_ref_none") (shared (ref (shared none))))
  (global (import "spectest" "global_mut_ref_none") (shared mut (ref (shared none))))

  ;; Initialized (long/short form).
  (global (shared (shared nullref)) (ref.null (shared none)))
  (global (shared (ref null (shared none))) (ref.null (shared none)))
)

(assert_invalid
  (module (global (shared nullref)))
  "shared globals must have a shared value type")
(assert_invalid
  (module (global (shared (ref null none))))
  "shared globals must have a shared value type")
(assert_invalid
  (module (global (ref (shared none))))
  "type mismatch")

