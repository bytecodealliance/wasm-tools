(module
  (type $WasiFile (instance
    (export "read" (func $read (param i32 i32 i32) (result i32)))
    (export "write" (func $write (param i32 i32 i32) (result i32)))
  ))
  (import "wasi_file" (instance $real-wasi (type $WasiFile)))

  (module $CHILD
    ;; TODO implement implicit parent aliases
    (alias parent $WasiFile (type $WasiFile))
    (import "wasi_file" (instance $wasi-file (type $WasiFile)))
    (func $play (export "play")
      ;; TODO: implement this
      ;;call $wasi-file.$read
    )
  )


  (module $VIRTUALIZE
    ;; TODO implement implicit parent aliases
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

  ;; TODO implement implicit child aliases
  (alias $child "play" (func $child.$play))

  (func (export "work")
    call $child.$play
  )
)
