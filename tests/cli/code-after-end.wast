;; RUN: wast --assert default --snapshot tests/snapshots %

(assert_malformed
  (module quote
    "(func end)")
  "operators remaining after end of function")

;; binary version of previous test
(assert_malformed
  (module binary
    "\00asm\01\00\00\00\01\04\01\60\00\00\03\02\01\00\0a\05\01\03\00\0b\0b")
  "operators remaining after end of function")

(assert_malformed
  (module quote
    "(func end block)")
  "operators remaining after end of function")

;; binary version of previous test
(assert_malformed
  (module binary
    "\00asm\01\00\00\00\01\04\01\60\00\00\03\02\01\00\0a\07\01\05\00\0b\02\40\0b")
  "operators remaining after end of function")

(assert_malformed
  (module quote
    "(func end i32.add)")
  "operators remaining after end of function")

;; binary version of previous test
(assert_malformed
  (module binary
    "\00asm\01\00\00\00\01\04\01\60\00\00\03\02\01\00\0a\06\01\04\00\0b\6a\0b")
  "operators remaining after end of function")

(assert_malformed
  (module quote
    "(func end unreachable)")
  "operators remaining after end of function")

;; binary version of previous test
(assert_malformed
  (module binary
    "\00asm\01\00\00\00\01\04\01\60\00\00\03\02\01\00\0a\06\01\04\00\0b\00\0b")
  "operators remaining after end of function")

(assert_malformed
  (module quote
    "(func end br 0)")
  "operators remaining after end of function")

;; binary version of previous test
(assert_malformed
  (module binary
    "\00asm\01\00\00\00\01\04\01\60\00\00\03\02\01\00\0a\07\01\05\00\0b\0c\00\0b")
  "operators remaining after end of function")

(assert_malformed
  (module quote
    "(func end return)")
  "operators remaining after end of function")

;; binary version of previous test
(assert_malformed
  (module binary
    "\00asm\01\00\00\00\01\04\01\60\00\00\03\02\01\00\0a\06\01\04\00\0b\0f\0b")
  "operators remaining after end of function")

(assert_malformed
  (module quote
    "(func end return_call 0)")
  "operators remaining after end of function")

;; binary version of previous test
(assert_malformed
  (module binary
    "\00asm\01\00\00\00\01\04\01\60\00\00\03\02\01\00\0a\07\01\05\00\0b\12\00\0b")
  "operators remaining after end of function")