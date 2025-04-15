;; FAIL: dump %
;; This fails because wasmprinter can't (yet) print an invalid multi-value select.
;; Can be changed back to a "RUN" test once that code lands.

(module
    (func
        select
        select (result)
        select (result i32)
        select (result i32 i32)
    )
)
