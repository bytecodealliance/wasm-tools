(module $child
  (import "wasi_file" (instance $wasi-file
    (export "read" (func $read (param i32 i32 i32) (result i32)))
    (export "write" (func $write (param i32 i32 i32) (result i32)))
  ))
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
  (export $wasi-file)
)


;; parent.wat
(module
  (type $WasiFile (instance
    (export "read" (func $read (param i32 i32 i32) (result i32)))
    (export "write" (func $write (param i32 i32 i32) (result i32)))
  ))
  (import "wasi_file" (instance $real-wasi (type $WasiFile)))
  (import "./virtualize.wasm" (module $VIRTUALIZE
    (import "wasi_file" (instance (type $WasiFile)))
    (export $WasiFile)
  ))
  (import "./child.wasm" (module $CHILD
    (import "wasi_file" (instance (type $WasiFile)))
    (export "play" (func $play))
  ))
  (instance $virt-wasi (instantiate $VIRTUALIZE (instance $real-wasi)))
  (instance $child (instantiate $CHILD (instance $virt-wasi)))
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
    (import "wasi_file" (instance $wasi-file (type $WasiFile)))
    (alias $wasi-file.$read (instance $wasi-file) (func 0))
    (func $play (export "play")
      call $wasi-file.$read
    )
  )


  (module $VIRTUALIZE
    (import "wasi_file" (instance $wasi-file (type $WasiFile)))
    (func (export "read") (param i32 i32 i32) (result i32)
      i32.const 0
    )
    (func (export "write") (param i32 i32 i32) (result i32)
      i32.const 0
    )
  )

  (instance $virt-wasi (instantiate $VIRTUALIZE (instance $real-wasi)))
  (instance $child (instantiate $CHILD (instance $virt-wasi)))
  (func (export "work")
    call $child.$play
  )
)
