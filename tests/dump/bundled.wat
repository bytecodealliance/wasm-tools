(module
  (type $WasiFile (instance
    (export "read" (func $read (param i32 i32 i32) (result i32)))
    (export "write" (func $write (param i32 i32 i32) (result i32)))
  ))
  (import "wasi_file" (instance $real-wasi (type $WasiFile)))

  (module $CHILD
    (import "wasi_file" (instance $wasi-file (type $WasiFile)))
    (func $play (export "play")
      ;; TODO: implement this
      ;;call $wasi-file.$read
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
