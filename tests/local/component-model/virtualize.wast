;; FIXME(#588) this test should be valid and the rest of the file should roughly
;; be updated to use a similar scheme.
(assert_invalid
(component $child
  (import "wasi-file" (instance $wasi-file
    (export "read" (func $read (param u32) (result (list u8))))
    (export "write" (func $write (param (list u8)) (result u32)))
  ))

  (module $libc
    (memory (export "memory") 1)
  )
  (module $m
    (import "wasi-file" "read" (func $read (param i32 i32)))
    (import "libc" "memory" (memory 1))
    (func $play (export "play")
      i32.const 0
      i32.const 0
      call $read
    )
  )

  (instance $libc (instantiate (module $libc)))
  (func $wasi_file_read
    (canon.lower (into $libc) (func $wasi-file "read"))
  )
  (instance $i (instantiate (module $m)
    (with "libc" (instance $libc))
    (with "wasi-file" (instance
      (export "read" (func $wasi_file_read))
    ))
  ))
  (func (export "play")
    (canon.lift (func) (func $i "play"))
  )
)
"instance 0 is not a module instance")


(; (module $virtualize ;)
(;   (import "wasi_file" (instance $wasi-file ;)
(;     (export "read" (func $read (param i32 i32 i32) (result i32))) ;)
(;     (export "write" (func $write (param i32 i32 i32) (result i32))) ;)
(;   )) ;)
(;   (export "read" (func $wasi-file "read")) ;)
(;   (export "write" (func $wasi-file "write")) ;)
(; ) ;)


(; ;; parent.wat ;)
(; (module ;)
(;   (type $WasiFile (instance ;)
(;     (export "read" (func (param i32 i32 i32) (result i32))) ;)
(;     (export "write" (func (param i32 i32 i32) (result i32))) ;)
(;   )) ;)
(;   (import "wasi_file" (instance $real-wasi (type $WasiFile))) ;)
(;   (import "./virtualize.wasm" (module $VIRTUALIZE ;)
(;     (import "wasi_file" (instance (type $WasiFile))) ;)
(;     (export "read" (func (param i32 i32 i32) (result i32))) ;)
(;     (export "write" (func (param i32 i32 i32) (result i32))) ;)
(;   )) ;)
(;   (import "./child.wasm" (module $CHILD ;)
(;     (import "wasi_file" (instance (type $WasiFile))) ;)
(;     (export "play" (func $play)) ;)
(;   )) ;)
(;   (instance $virt-wasi (instantiate $VIRTUALIZE (import "wasi_file" (instance $real-wasi)))) ;)
(;   (instance $child (instantiate $CHILD (import "wasi_file" (instance $virt-wasi)))) ;)

(;   (func (export "work") ;)
(;     call (func $child "play") ;)
(;   ) ;)
(; ) ;)


(; ;; bundled.wat ;)
(; (module ;)
(;   (type $WasiFile (instance ;)
(;     (export "read" (func $read (param i32 i32 i32) (result i32))) ;)
(;     (export "write" (func $write (param i32 i32 i32) (result i32))) ;)
(;   )) ;)
(;   (import "wasi_file" (instance $real-wasi (type $WasiFile))) ;)

(;   (module $CHILD ;)
(;     (import "wasi_file" (instance $wasi-file (type outer 0 $WasiFile))) ;)
(;     (func $play (export "play") ;)
(;       i32.const 0 ;)
(;       i32.const 0 ;)
(;       i32.const 0 ;)
(;       call (func $wasi-file "read") ;)
(;       drop ;)
(;     ) ;)
(;   ) ;)


(;   (module $VIRTUALIZE ;)
(;     (import "wasi_file" (instance $wasi-file (type outer 0 $WasiFile))) ;)
(;     (func (export "read") (param i32 i32 i32) (result i32) ;)
(;       i32.const 0 ;)
(;     ) ;)
(;     (func (export "write") (param i32 i32 i32) (result i32) ;)
(;       i32.const 0 ;)
(;     ) ;)
(;   ) ;)

(;   (instance $virt-wasi (instantiate $VIRTUALIZE (import "wasi_file" (instance $real-wasi)))) ;)
(;   (instance $child (instantiate $CHILD (import "wasi_file" (instance $virt-wasi)))) ;)
(;   (func (export "work") ;)
(;     call (func $child "play") ;)
(;   ) ;)
(; ) ;)
