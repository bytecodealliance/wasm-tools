;; RUN: dump %

;; This test case was generated with this Rust source file:
;;
;;    fn main() {}
;;
;; compiled with Rust 1.78.0 with:
;;
;;    rustc foo.rs --emit obj --target wasm32-wasip1
;;
;; and then the output of `wasm-tools print` at the time over `foo.o` was
;; pasted below. This works around how the linking and reloc custom sections do
;; not have a text format at this time.

(module
  (type (;0;) (func (param i32)))
  (type (;1;) (func (param i32 i32 i32 i32) (result i32)))
  (type (;2;) (func (param i32 i32 i32 i32 i32) (result i32)))
  (type (;3;) (func (param i32) (result i32)))
  (type (;4;) (func (result i32)))
  (type (;5;) (func))
  (import "env" "__linear_memory" (memory (;0;) 1))
  (import "env" "__stack_pointer" (global (;0;) (mut i32)))
  (import "env" "_ZN3std2rt19lang_start_internal17h8f61396649e22e0eE" (func (;0;) (type 2)))
  (import "env" "__indirect_function_table" (table (;0;) 4 funcref))
  (func (;1;) (type 0) (param i32)
    local.get 0
    call 2
    return
  )
  (func (;2;) (type 0) (param i32)
    (local i32 i32 i32 i32 i32)
    global.get 0
    local.set 1
    i32.const 16
    local.set 2
    local.get 1
    local.get 2
    i32.sub
    local.set 3
    local.get 3
    global.set 0
    local.get 0
    call_indirect (type 5)
    i32.const 16
    local.set 4
    local.get 3
    local.get 4
    i32.add
    local.set 5
    local.get 5
    global.set 0
    return
  )
  (func (;3;) (type 1) (param i32 i32 i32 i32) (result i32)
    (local i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32)
    global.get 0
    local.set 4
    i32.const 16
    local.set 5
    local.get 4
    local.get 5
    i32.sub
    local.set 6
    local.get 6
    global.set 0
    local.get 6
    local.get 0
    i32.store offset=12
    i32.const 12
    local.set 7
    local.get 6
    local.get 7
    i32.add
    local.set 8
    local.get 8
    local.set 9
    i32.const 0
    local.set 10
    local.get 9
    local.get 10
    local.get 1
    local.get 2
    local.get 3
    call 0
    local.set 11
    local.get 6
    local.get 11
    i32.store offset=8
    local.get 6
    i32.load offset=8
    local.set 12
    i32.const 16
    local.set 13
    local.get 6
    local.get 13
    i32.add
    local.set 14
    local.get 14
    global.set 0
    local.get 12
    return
  )
  (func (;4;) (type 3) (param i32) (result i32)
    (local i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32)
    global.get 0
    local.set 1
    i32.const 16
    local.set 2
    local.get 1
    local.get 2
    i32.sub
    local.set 3
    local.get 3
    global.set 0
    local.get 0
    i32.load
    local.set 4
    local.get 4
    call 1
    call 5
    local.set 5
    i32.const 1
    local.set 6
    local.get 5
    local.get 6
    i32.and
    local.set 7
    local.get 3
    local.get 7
    i32.store8 offset=15
    local.get 3
    i32.load8_u offset=15
    local.set 8
    i32.const 1
    local.set 9
    local.get 8
    local.get 9
    i32.and
    local.set 10
    i32.const 16
    local.set 11
    local.get 3
    local.get 11
    i32.add
    local.set 12
    local.get 12
    global.set 0
    local.get 10
    return
  )
  (func (;5;) (type 4) (result i32)
    (local i32 i32 i32)
    i32.const 0
    local.set 0
    i32.const 1
    local.set 1
    local.get 0
    local.get 1
    i32.and
    local.set 2
    local.get 2
    return
  )
  (func (;6;) (type 3) (param i32) (result i32)
    (local i32 i32 i32 i32 i32 i32 i32)
    global.get 0
    local.set 1
    i32.const 16
    local.set 2
    local.get 1
    local.get 2
    i32.sub
    local.set 3
    local.get 3
    global.set 0
    local.get 0
    i32.load
    local.set 4
    local.get 4
    call 7
    local.set 5
    i32.const 16
    local.set 6
    local.get 3
    local.get 6
    i32.add
    local.set 7
    local.get 7
    global.set 0
    local.get 5
    return
  )
  (func (;7;) (type 3) (param i32) (result i32)
    (local i32 i32 i32 i32 i32 i32 i32 i32 i32)
    global.get 0
    local.set 1
    i32.const 16
    local.set 2
    local.get 1
    local.get 2
    i32.sub
    local.set 3
    local.get 3
    global.set 0
    local.get 3
    local.get 0
    i32.store offset=8
    i32.const 8
    local.set 4
    local.get 3
    local.get 4
    i32.add
    local.set 5
    local.get 5
    local.set 6
    local.get 6
    call 4
    local.set 7
    i32.const 16
    local.set 8
    local.get 3
    local.get 8
    i32.add
    local.set 9
    local.get 9
    global.set 0
    local.get 7
    return
  )
  (func (;8;) (type 0) (param i32)
    return
  )
  (func (;9;) (type 5)
    return
  )
  (func (;10;) (type 4) (result i32)
    (local i32 i32 i32 i32)
    i32.const 1
    local.set 0
    i32.const 0
    local.set 1
    i32.const 0
    local.set 2
    local.get 0
    local.get 1
    local.get 1
    local.get 2
    call 3
    local.set 3
    local.get 3
    return
  )
  (elem (;0;) (i32.const 1) func 9 8 6 4)
  (data (;0;) (i32.const 0) "\02\00\00\00\04\00\00\00\04\00\00\00\03\00\00\00\04\00\00\00\04\00\00\00")
  (@custom "linking" (after data) "\02\08\c9\85\80\80\00\0d\00\02\01O_ZN3std10sys_common9backtrace28__rust_begin_short_backtrace17hf1ecc3b2b910465aE\00\02\02:_ZN4core3ops8function6FnOnce9call_once17h06fc313c2ecf3d24E\00\04\03*_ZN3std2rt10lang_start17h385f18abde4350daE\02\10\00\01\02\0d.L__unnamed_1\00\00\18\00\10\00\00\02\04H_ZN3std2rt10lang_start28_$u7b$$u7b$closure$u7d$$u7d$17h24681ec0ade90745E\00\02\05V_ZN54_$LT$$LP$$RP$$u20$as$u20$std..process..Termination$GT$6report17haa5bf2f79896d941E\00\02\06Z_ZN4core3ops8function6FnOnce40call_once$u7b$$u7b$vtable.shim$u7d$$u7d$17hc804a9e31dd3115cE\00\02\07:_ZN4core3ops8function6FnOnce9call_once17h20fe7bf2b7692f9bE\00\02\08w_ZN4core3ptr85drop_in_place$LT$std..rt..lang_start$LT$$LP$$RP$$GT$..$u7b$$u7b$closure$u7d$$u7d$$GT$17h26d303f198cad6c2E\00\02\09 _ZN3foo4main17h694cf023eeafda77E\00\04\0a\0b__main_void\05\99\80\80\80\00\01\15.rodata..L__unnamed_1\02\00")
  (@custom "reloc.CODE" (after data) "\05\19\00\06\01\07\12\03\07'\03\06/\05\07C\03\07O\03\07d\03\04\80\01\04\00\00\92\01\05\07\b5\01\03\07\c3\01\03\07\d8\01\03\00\e7\01\00\00\ed\01\07\07\a6\02\03\07\cb\02\03\07\e0\02\03\00\ef\02\09\07\84\03\03\07\92\03\03\07\a7\03\03\00\c5\03\06\07\da\03\03\01\f0\03\0b\00\88\04\02")
  (@custom "reloc.DATA" (after data) "\06\04\02\06\0a\02\12\08\02\16\06\02\1a\06")
  (@producers
    (processed-by "rustc" "1.78.0 (9b00956e5 2024-04-29)")
  )
  (@custom "target_features" (after data) "\02+\0fmutable-globals+\08sign-ext")
)
