;; RUN: wast --assert default --snapshot tests/snapshots %

(memory
  (data (i8 1 2) (i16 3 4))
)

(data (i32.const 0)
  (i8 0 -128 255)
  (i16 0 -32768 65535)
  (i32 0 -2147483648 4294967295)
  (i64 0 -9223372036854775808 18446744073709551615)
  (f32 0.1 0.2 -0.2 nan)
  (f64 0.1 0.2 -0.2 nan)
  (v128 i32x4 0 0 0 0)
)
