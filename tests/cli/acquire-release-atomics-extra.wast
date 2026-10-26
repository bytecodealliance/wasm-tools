;; RUN: wast --assert default --snapshot tests/snapshots % -f acquire-release-atomics

(module
  (memory 1 1)
  (memory $m 1 1 shared)
  (func
    (drop (i32.atomic.load offset=8 acqrel (i32.const 0)))
    (drop (i32.atomic.load offset=8 align=4 seqcst (i32.const 0)))
    (drop (i32.atomic.load $m offset=8 acqrel (i32.const 0)))
    (i64.atomic.store $m offset=16 acqrel (i32.const 0) (i64.const 0))
    (drop (i32.atomic.rmw.add $m offset=4 acqrel (i32.const 0) (i32.const 0)))
    (drop (i64.atomic.rmw.cmpxchg $m acqrel (i32.const 0) (i64.const 0) (i64.const 0)))
    pause
  )
)

;; The ordering immediate comes after the memory index.
(module binary
  "\00asm\01\00\00\00"
  "\01\04\01\60\00\00"                  ;; type section
  "\03\02\01\00"                        ;; function section
  "\05\0a\03"                           ;; memory section
    "\01\01\01"                         ;; (memory 1 1)
    "\01\01\01"                         ;; (memory 1 1)
    "\03\01\01"                         ;; (memory 1 1 shared)
  "\0a\0d\01\0b\00"                     ;; code section
    "\41\00"                            ;; i32.const 0
    "\fe\10\52\02\01\08"                ;; i32.atomic.load 2 offset=8 acqrel
    "\1a"                               ;; drop
  "\0b"                                 ;; end
)

;; Loads and stores only use the low 4 bits of the ordering immediate, and it's
;; malformed to use the high bits.
(assert_malformed
  (module binary
    "\00asm\01\00\00\00"
    "\01\04\01\60\00\00"                  ;; type section
    "\03\02\01\00"                        ;; function section
    "\05\04\01\01\01\01"                  ;; memory section
    "\0a\0c\01\0a\00"                     ;; code section
      "\41\00"                            ;; i32.const 0
      "\fe\10\12\11\00"                   ;; i32.atomic.load with ordering 0x11
      "\1a"                               ;; drop
    "\0b"                                 ;; end
  )
  "invalid atomic consistency ordering")

;; Read-modify-write instructions currently must use the same read and write
;; orderings, and other encodings are malformed.
(assert_malformed
  (module binary
    "\00asm\01\00\00\00"
    "\01\04\01\60\00\00"                  ;; type section
    "\03\02\01\00"                        ;; function section
    "\05\04\01\01\01\01"                  ;; memory section
    "\0a\0e\01\0c\00"                     ;; code section
      "\41\00\41\00"                      ;; i32.const 0, i32.const 0
      "\fe\1e\12\01\00"                   ;; i32.atomic.rmw.add with ordering 0x01
      "\1a"                               ;; drop
    "\0b"                                 ;; end
  )
  "invalid atomic consistency ordering")
(assert_malformed
  (module binary
    "\00asm\01\00\00\00"
    "\01\04\01\60\00\00"                  ;; type section
    "\03\02\01\00"                        ;; function section
    "\05\04\01\01\01\01"                  ;; memory section
    "\0a\0e\01\0c\00"                     ;; code section
      "\41\00\41\00"                      ;; i32.const 0, i32.const 0
      "\fe\1e\12\10\00"                   ;; i32.atomic.rmw.add with ordering 0x10
      "\1a"                               ;; drop
    "\0b"                                 ;; end
  )
  "invalid atomic consistency ordering")

;; Non-atomic instructions can't have an ordering immediate
(assert_malformed
  (module binary
    "\00asm\01\00\00\00"
    "\01\04\01\60\00\00"                  ;; type section
    "\03\02\01\00"                        ;; function section
    "\05\04\01\01\01\01"                  ;; memory section
    "\0a\0b\01\09\00"                     ;; code section
      "\41\00"                            ;; i32.const 0
      "\28\12\01\00"                      ;; i32.load with ordering 0x01
      "\1a"                               ;; drop
    "\0b"                                 ;; end
  )
  "cannot specify atomic ordering on non-atomic instruction")

;; `memory.atomic.{notify,wait*}` aren't extended with an ordering immediate.
(assert_malformed
  (module binary
    "\00asm\01\00\00\00"
    "\01\04\01\60\00\00"                  ;; type section
    "\03\02\01\00"                        ;; function section
    "\05\04\01\03\01\01"                  ;; memory section, shared
    "\0a\0e\01\0c\00"                     ;; code section
      "\41\00\41\00"                      ;; i32.const 0, i32.const 0
      "\fe\00\12\01\00"                   ;; memory.atomic.notify with ordering 0x01
      "\1a"                               ;; drop
    "\0b"                                 ;; end
  )
  "cannot specify atomic ordering on non-atomic instruction")
(assert_malformed
  (module quote
    "(memory 1 1 shared) (func (drop (memory.atomic.notify acqrel (i32.const 0) (i32.const 0))))"
  )
  "unexpected token")

;; Unknown orderings are malformed.
(assert_malformed
  (module binary
    "\00asm\01\00\00\00"
    "\01\04\01\60\00\00"                  ;; type section
    "\03\02\01\00"                        ;; function section
    "\05\04\01\01\01\01"                  ;; memory section
    "\0a\0c\01\0a\00"                     ;; code section
      "\41\00"                            ;; i32.const 0
      "\fe\10\12\02\00"                   ;; i32.atomic.load with ordering 0x02
      "\1a"                               ;; drop
    "\0b"                                 ;; end
  )
  "invalid atomic consistency ordering 0x2")
(assert_malformed
  (module binary
    "\00asm\01\00\00\00"
    "\01\04\01\60\00\00"                  ;; type section
    "\03\02\01\00"                        ;; function section
    "\0a\07\01\05\00"                     ;; code section
      "\fe\03\02"                         ;; atomic.fence with ordering 0x02
    "\0b"                                 ;; end
  )
  "invalid atomic consistency ordering 2")
