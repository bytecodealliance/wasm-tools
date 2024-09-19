;; RUN: print % | dump

;; test that all forms of element segments can be round-tripped from
;; text-to-binary.
(module
  (table 1 1 funcref)

  (func $f)

  (elem (i32.const 0) func)                   ;; 0x00
  (elem func)                                 ;; 0x01
  (elem (table 0) (i32.const 0) func)         ;; 0x02
  (elem declare func)                         ;; 0x03
  (elem (i32.const 0) funcref)                ;; 0x04
  (elem funcref)                              ;; 0x05
  (elem (table 0) (i32.const 0) funcref)      ;; 0x06
  (elem declare funcref)                      ;; 0x07
)
