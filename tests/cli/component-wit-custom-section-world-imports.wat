;; FAIL: component wit %
;;
;; The custom section holds the binary of:
;;
;;     (component (type $c (component (import "a:b/c" (instance)) (export "a:b/w" (component)))) (export "w" (type $c)))

(module
    (@custom "component-type:x" "\00\61\73\6d\0d\00\01\00\07\1d\01\41\04\01\42\00\03\00\05\61\3a\62\2f\63\05\00\01\41\00\04\00\05\61\3a\62\2f\77\04\01\0b\07\01\00\01\77\03\00\00\00\16\0e\63\6f\6d\70\6f\6e\65\6e\74\2d\6e\61\6d\65\01\05\03\01\00\01\63")
)
