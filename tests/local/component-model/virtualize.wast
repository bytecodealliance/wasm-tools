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
      (export "read" (func (param "count" u32) (result (list u8))))
      (export "write" (func (param "bytes" (list u8)) (result u32)))
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
        (memory $libc "mem")
        (realloc (func $libc "realloc"))
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
      (export "read" (func (param "len" u32) (result (list u8))))
      (export "write" (func (param "buf" (list u8)) (result u32)))
    ))
    (export "read" (func $wasi-file "read"))
    (export "write" (func $wasi-file "write"))
  )

  (component
    (type $WasiFile (instance
      (export "read" (func (param "len" u32) (result (list u8))))
      (export "write" (func (param "buf" (list u8)) (result u32)))
    ))
    (import "wasi-file" (instance $real-wasi (type $WasiFile)))
    (import "virtualize" (component $VIRTUALIZE
      (import "wasi-file" (instance (type $WasiFile)))
        (export "read" (func (param "len" u32) (result (list u8))))
        (export "write" (func (param "buf" (list u8)) (result u32)))
      ))
      (import "child" (component $CHILD
        (import "wasi-file" (instance (type $WasiFile)))
        (export "play" (func))
      )
    )

    (instance $virt-wasi (instantiate $VIRTUALIZE (with "wasi-file" (instance $real-wasi))))
    (instance $child (instantiate $CHILD (with "wasi-file" (instance $virt-wasi))))

    (export "work" (func $child "play"))
  )

  (component
    (type $WasiFile (instance
      (export "read" (func (param "len" u32) (result (list u8))))
      (export "write" (func (param "buf" (list u8)) (result u32)))
    ))
    (import "wasi-file" (instance $real-wasi (type $WasiFile)))

    (core instance $libc (instantiate $libc))

    (core module $CHILD
      (import "wasi-file" "read" (func $wasi-file (param i32 i32)))
      (func $play (export "play")
        unreachable
      )
    )

    (core module $VIRTUALIZE
      (import "wasi-file" "read" (func (param i32 i32)))
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

    (core instance $virt-wasi (instantiate $VIRTUALIZE (with "wasi-file" (instance (export "read" (func $real-wasi-read))))))
    (core instance $child (instantiate $CHILD (with "wasi-file" (instance $virt-wasi))))
    (func (export "work")
      (canon lift (core func $child "play")
        (memory $libc "mem")
        (realloc (func $libc "realloc"))
      )
    )
  )
)
