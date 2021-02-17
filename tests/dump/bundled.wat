(module
  (type $WasiFile (instance
    (export "read" (func $read (param i32 i32 i32) (result i32)))
    (export "write" (func $write (param i32 i32 i32) (result i32)))
  ))
  (import "wasi_file" (instance $real-wasi (type $WasiFile)))

  (module $CHILD
    (import "wasi_file" (instance $wasi-file (type outer 0 $WasiFile)))
    (func $play (export "play")
      call (func $wasi-file "read")
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
