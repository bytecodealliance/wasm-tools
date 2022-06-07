(component
  (core module $libc
    (memory (export "mem") 0)
    (func (export "realloc") (param i32 i32 i32 i32) (result i32)
      unreachable
    )
  )
  (core instance $libc (instantiate $libc))

  (component $child
    (import "wasi-file" (instance $wasi-file
      (export "read" (func $read (param u32) (result (list u8))))
      (export "write" (func $write (param (list u8)) (result u32)))
    ))

    (core instance $libc (instantiate $libc))

    (core module $m
      (import "wasi-file" "read" (func $read (param i32 i32)))
      (func $play (export "play")
        unreachable
      )
    )

    (core func $wasi_file_read
      (canon lower (func $wasi-file "read")
        (memory (core memory $libc "mem"))
        (realloc (core func $libc "realloc"))
      )
    )

    (core instance $i (instantiate $m
      (with "wasi-file" (instance
        (export "read" (func $wasi_file_read))
      ))
    ))

    (func (export "play")
      (canon lift (core func $i "play"))
    )
  )

  (component $virtualize
    (import "wasi-file" (instance $wasi-file
      (export "read" (func $read (param u32) (result (list u8))))
      (export "write" (func $write (param (list u8)) (result u32)))
    ))
    (export "read" (func $wasi-file "read"))
    (export "write" (func $wasi-file "write"))
  )

  (component
    (type $WasiFile (instance
      (export "read" (func $read (param u32) (result (list u8))))
      (export "write" (func $write (param (list u8)) (result u32)))
    ))
    (import "wasi_file" (instance $real-wasi (type $WasiFile)))
    (import "./virtualize.wasm" (component $VIRTUALIZE
      (import "wasi_file" (instance (type $WasiFile)))
        (export "read" (func $read (param u32) (result (list u8))))
        (export "write" (func $write (param (list u8)) (result u32)))
      ))
      (import "./child.wasm" (component $CHILD
        (import "wasi_file" (instance (type $WasiFile)))
        (export "play" (func $play))
      )
    )

    (instance $virt-wasi (instantiate $VIRTUALIZE (with "wasi_file" (instance $real-wasi))))
    (instance $child (instantiate $CHILD (with "wasi_file" (instance $virt-wasi))))

    (export "work" (func $child "play"))
  )

  (component
    (type $WasiFile (instance
      (export "read" (func $read (param u32) (result (list u8))))
      (export "write" (func $write (param (list u8)) (result u32)))
    ))
    (import "wasi_file" (instance $real-wasi (type $WasiFile)))

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
        (memory (core memory $libc "mem"))
        (realloc (core func $libc "realloc"))
      )
    )

    (core instance $virt-wasi (instantiate $VIRTUALIZE (with "wasi_file" (instance (export "read" (func $real-wasi-read))))))
    (core instance $child (instantiate $CHILD (with "wasi_file" (instance $virt-wasi))))
    (func (export "work")
      (canon lift (core func $child "play")
        (memory (core memory $libc "mem"))
        (realloc (core func $libc "realloc"))
      )
    )
  )
)
