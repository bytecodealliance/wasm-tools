;; Check the `shared` attribute on tables.

(module
  ;; Imported.
  (table (import "spectest" "table_ref") shared 1 (ref null (shared func)))
  (table (import "spectest" "table_ref_with_max") shared 1 1 (ref null (shared func)))

  ;; Normal.
  (table shared 1 (ref null (shared func)))
  (table shared 1 1 (ref null (shared func)))

  ;; Inlined.
  (table shared (ref null (shared func)) (elem (ref.null (shared func))))
)

;; Note that shared elements can live within an unshared table.
(module
  (table (import "spectest" "table_ref") 1 (ref null (shared func)))
)

(assert_malformed
  (module quote "(table 1 shared funcref)")
  "unexpected token")

(assert_malformed
  (module quote "(table 1 funcref shared)")
  "unexpected token")

;; The proposal creates too much ambiguity to allow this syntax: the parser
;; would need to lookahead multiple tokens.
(assert_malformed
  (module quote "(table shared i64 (ref null (shared func)) (elem (ref.null (shared func))))")
  "unexpected token")

(assert_invalid
  (module (table (import "spectest" "table_ref") shared 0 funcref))
  "shared tables must have a shared element type")

(assert_invalid
  (module
    (type $t (func))
    (table shared 0 (ref $t)))
  "shared tables must have a shared element type")
