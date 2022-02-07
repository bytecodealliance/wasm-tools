(module $child
  (import "wasi_file" (instance $wasi-file
    (export "read" (func $read (param i32 i32 i32) (result i32)))
    (export "write" (func $write (param i32 i32 i32) (result i32)))
  ))
  (func $play (export "play")
    i32.const 0
    i32.const 0
    i32.const 0
    call (func $wasi-file "read")
    drop
  )
)


(module $virtualize
  (import "wasi_file" (instance $wasi-file
    (export "read" (func $read (param i32 i32 i32) (result i32)))
    (export "write" (func $write (param i32 i32 i32) (result i32)))
  ))
  (export "read" (func $wasi-file "read"))
  (export "write" (func $wasi-file "write"))
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
  (instance $virt-wasi (instantiate $VIRTUALIZE (import "wasi_file" (instance $real-wasi))))
  (instance $child (instantiate $CHILD (import "wasi_file" (instance $virt-wasi))))

  (func (export "work")
    call (func $child "play")
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
    (import "wasi_file" (instance $wasi-file (type outer 0 $WasiFile)))
    (func $play (export "play")
      i32.const 0
      i32.const 0
      i32.const 0
      call (func $wasi-file "read")
      drop
    )
  )


  (module $VIRTUALIZE
    (import "wasi_file" (instance $wasi-file (type outer 0 $WasiFile)))
    (func (export "read") (param i32 i32 i32) (result i32)
      i32.const 0
    )
    (func (export "write") (param i32 i32 i32) (result i32)
      i32.const 0
    )
  )

  (instance $virt-wasi (instantiate $VIRTUALIZE (import "wasi_file" (instance $real-wasi))))
  (instance $child (instantiate $CHILD (import "wasi_file" (instance $virt-wasi))))
  (func (export "work")
    call (func $child "play")
  )
)
