(module
  (type (;0;) (func (result v128)))
  (func (;0;) (type 0) (result v128)
    i32.const 4
    v128.load)
  (func (;1;) (type 0) (result v128)
    i32.const 4
    v128.const i32x4 0x11223344 0x55667788 0x99aabbcc 0xddeeff00
    v128.store
    i32.const 4
    v128.load)
  (func (;2;) (type 0) (result v128)
    v128.const i32x4 0x0000789a 0xff880330 0x0000ffff 0x0000017f
    f64.const 0x1.2p+2 (;=4.5;)
    f64x2.replace_lane 0)
  (func (;3;) (type 0) (result v128)
    v128.const i32x4 0xff00ff01 0xff00ff0f 0xff00ffff 0xff00ff7f
    v128.const i32x4 0x00550055 0x00550055 0x00550055 0x00550155
    v8x16.shuffle 16 1 18 3 20 5 22 7 24 9 26 11 28 13 30 15)
  (memory (;0;) 1)
  (export "v128_load_0" (func 0))
  (export "v128_store_0" (func 1))
  (export "func_f64x2_replace_lane_0" (func 2))
  (export "func_v8x16_shuffle_0" (func 3))
  (data (;0;) (i32.const 0) "\ff\ff\ff\ff")
  (data (;1;) (i32.const 4) "\00\00\ceA")
  (data (;2;) (i32.const 8) "\00\00\00\00\00\ff\8f@")
  (data (;3;) (i32.const 16) "\ff\ff\ff\ff\ff\ff\ff\ff"))