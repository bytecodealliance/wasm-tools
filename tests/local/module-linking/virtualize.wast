(module $child
  (import "wasi_file" (instance $wasi-file
    (export "read" (func $read (param i32 i32 i32) (result i32)))
    (export "write" (func $write (param i32 i32 i32) (result i32)))
  ))
  ;; TODO: alias sugar
  (alias $wasi-file "read" (func $wasi-file.$read))
  (func $play (export "play")
    i32.const 0
    i32.const 0
    i32.const 0
    call $wasi-file.$read
    drop
  )
)


(module $virtualize
  (import "wasi_file" (instance $wasi-file
    (export "read" (func $read (param i32 i32 i32) (result i32)))
    (export "write" (func $write (param i32 i32 i32) (result i32)))
  ))
  ;; TODO alias sugar
  (alias $wasi-file "read" (func $wasi-file.read))
  (alias $wasi-file "write" (func $wasi-file.write))
  (export "read" (func $wasi-file.read))
  (export "write" (func $wasi-file.write))
)


;; parent.wat
(module
  (type $WasiFile (instance
    (export "read" (func (param i32 i32 i32) (result i32)))
    (export "write" (func (param i32 i32 i32) (result i32)))
  ))
  (import "wasi_file" (instance $real-wasi (type $WasiFile)))
  (import "./virtualize.wasm" (module $VIRTUALIZE
    (import "wasi_file" (instance (type $WasiFile)))
    (export "read" (func (param i32 i32 i32) (result i32)))
    (export "write" (func (param i32 i32 i32) (result i32)))
  ))
  (import "./child.wasm" (module $CHILD
    (import "wasi_file" (instance (type $WasiFile)))
    (export "play" (func $play))
  ))
  (instance $virt-wasi (instantiate $VIRTUALIZE (arg "wasi_file" (instance $real-wasi))))
  (instance $child (instantiate $CHILD (arg "wasi_file" (instance $virt-wasi))))

  ;; TODO alias sugar
  (alias $child "play" (func $child.$play))
  (func (export "work")
    call $child.$play
  )
)


;; bundled.wat
(module
  (type $WasiFile (instance
    (export "read" (func $read (param i32 i32 i32) (result i32)))
    (export "write" (func $write (param i32 i32 i32) (result i32)))
  ))
  (import "wasi_file" (instance $real-wasi (type $WasiFile)))

  (module $CHILD
    ;; TODO: alias sugar
    (alias parent $WasiFile (type $WasiFile))
    (import "wasi_file" (instance $wasi-file (type $WasiFile)))
    ;; TODO: alias sugar
    (alias $wasi-file "read" (func $wasi-file.$read))
    (func $play (export "play")
      i32.const 0
      i32.const 0
      i32.const 0
      call $wasi-file.$read
      drop
    )
  )


  (module $VIRTUALIZE
    ;; TODO: alias sugar
    (alias parent $WasiFile (type $WasiFile))
    (import "wasi_file" (instance $wasi-file (type $WasiFile)))
    (func (export "read") (param i32 i32 i32) (result i32)
      i32.const 0
    )
    (func (export "write") (param i32 i32 i32) (result i32)
      i32.const 0
    )
  )

  (instance $virt-wasi (instantiate $VIRTUALIZE (arg "wasi_file" (instance $real-wasi))))
  (instance $child (instantiate $CHILD (arg "wasi_file" (instance $virt-wasi))))
  ;; TODO: alias sugar
  (alias $child "play" (func $child.$play))
  (func (export "work")
    call $child.$play
  )
)
