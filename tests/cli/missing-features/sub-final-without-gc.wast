;; RUN: wast --assert default --snapshot tests/snapshots % -f wasm2

;; The `sub` and `sub final` prefixes are part of the GC proposal, even when
;; the type has no supertypes.

(assert_malformed
  (module binary
    "\00asm" "\01\00\00\00"
    "\01\06\01"          ;; type section, 1 rec group
    "\4f\00"             ;; sub final, 0 supertypes
    "\60\00\00"          ;; func [] -> []
  )
  "gc proposal must be enabled to use subtypes")

(assert_malformed
  (module binary
    "\00asm" "\01\00\00\00"
    "\01\06\01"          ;; type section, 1 rec group
    "\50\00"             ;; sub, 0 supertypes
    "\60\00\00"          ;; func [] -> []
  )
  "gc proposal must be enabled to use subtypes")

;; A plain function type is still fine.
(module binary
  "\00asm" "\01\00\00\00"
  "\01\04\01"          ;; type section, 1 rec group
  "\60\00\00"          ;; func [] -> []
)
