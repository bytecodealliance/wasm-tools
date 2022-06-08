(component
  (type $WasiFile (instance
      (export "read" (func $read (param u32) (result (list u8))))
      (export "write" (func $write (param (list u8)) (result u32)))
    ))
  (import "wasi_file" (instance $real-wasi (type $WasiFile)))

  (core module $libc
    (memory (export "mem") 0)
    (func (export "realloc") (param i32 i32 i32 i32) (result i32)
      unreachable
    )
  )

  (core instance $libc (instantiate $libc))

  (core module $CHILD
    (import "wasi_file" "read" (func $wasi-file (param i32 i32)))
    (func $play (export "play")
      unreachable
    )
  )

  (core module $VIRTUALIZE
    (import "wasi_file" "read" (func (param i32 i32)))
    (func (export "read") (param i32 i32)
      unreachable
    )
    (func (export "write") (param i32 i32 i32)
      unreachable
    )
  )

  (core func $real-wasi-read
    (canon lower (func $real-wasi "read")
      (memory $libc "mem")
      (realloc (func $libc "realloc"))
    )
  )

  (core instance $virt-wasi (instantiate $VIRTUALIZE (with "wasi_file" (instance (export "read" (func $real-wasi-read))))))
  (core instance $child (instantiate $CHILD (with "wasi_file" (instance $virt-wasi))))
  (func (export "work")
    (canon lift (core func $child "play")
      (memory $libc "mem")
      (realloc (func $libc "realloc"))
    )
  )
)
