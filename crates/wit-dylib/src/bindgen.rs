use crate::Adapter;
use crate::metadata;
use std::collections::HashMap;
use std::slice;
use wasm_encoder::{BlockType, Function, InstructionSink, MemArg, ValType};
use wit_parser::abi::{AbiVariant, FlatTypes, WasmSignature, WasmType};
use wit_parser::{FlagsRepr, Handle, Int, Resolve, Type, TypeDefKind, TypeId};

macro_rules! intrinsics {
    ($(
        $name:ident : [ $($params:tt)* ] -> [ $($results:tt)* ] = $str:tt,
    )*) => (
        pub struct WitInterpreterIntrinsics {
            $(pub $name: u32,)*
        }

        impl WitInterpreterIntrinsics {
            pub fn new(adapter: &mut crate::Adapter) -> Self {
                Self {
                    $(
                        $name: {
                            let ty = adapter.define_ty([$($params)*], [$($results)*]);
                            adapter.import_func("env", $str, ty)
                        },
                    )*
                }
            }
        }
    )
}

intrinsics! {
    initialize : [ValType::I32] -> [] = "wit_dylib_initialize",
    cabi_realloc : [ValType::I32; 4] -> [ValType::I32] = "cabi_realloc",
    dealloc_bytes : [ValType::I32; 3] -> [] = "wit_dylib_dealloc_bytes",

    export_start : [ValType::I32] -> [ValType::I32] = "wit_dylib_export_start",
    export_call : [ValType::I32; 2] -> [] = "wit_dylib_export_call",
    export_finish : [ValType::I32; 2] -> [] = "wit_dylib_export_finish",
    resource_dtor : [ValType::I32; 2] -> [] = "wit_dylib_resource_dtor",

    pop_u8 : [ValType::I32] -> [ValType::I32] = "wit_dylib_pop_u8",
    pop_u16 : [ValType::I32] -> [ValType::I32] = "wit_dylib_pop_u16",
    pop_u32 : [ValType::I32] -> [ValType::I32] = "wit_dylib_pop_u32",
    pop_u64 : [ValType::I32] -> [ValType::I64] = "wit_dylib_pop_u64",
    pop_s8 : [ValType::I32] -> [ValType::I32] = "wit_dylib_pop_s8",
    pop_s16 : [ValType::I32] -> [ValType::I32] = "wit_dylib_pop_s16",
    pop_s32 : [ValType::I32] -> [ValType::I32] = "wit_dylib_pop_s32",
    pop_s64 : [ValType::I32] -> [ValType::I64] = "wit_dylib_pop_s64",
    pop_bool : [ValType::I32] -> [ValType::I32] = "wit_dylib_pop_bool",
    pop_char : [ValType::I32] -> [ValType::I32] = "wit_dylib_pop_char",
    pop_f32 : [ValType::I32] -> [ValType::F32] = "wit_dylib_pop_f32",
    pop_f64 : [ValType::I32] -> [ValType::F64] = "wit_dylib_pop_f64",
    pop_flags : [ValType::I32; 2] -> [ValType::I32] = "wit_dylib_pop_flags",
    pop_enum : [ValType::I32; 2] -> [ValType::I32] = "wit_dylib_pop_enum",
    pop_borrow : [ValType::I32; 2] -> [ValType::I32] = "wit_dylib_pop_borrow",
    pop_own : [ValType::I32; 2] -> [ValType::I32] = "wit_dylib_pop_own",
    pop_future : [ValType::I32; 2] -> [ValType::I32] = "wit_dylib_pop_future",
    pop_stream : [ValType::I32; 2] -> [ValType::I32] = "wit_dylib_pop_stream",
    pop_record : [ValType::I32; 2] -> [] = "wit_dylib_pop_record",
    pop_tuple : [ValType::I32; 2] -> [] = "wit_dylib_pop_tuple",
    pop_variant : [ValType::I32; 2] -> [ValType::I32] = "wit_dylib_pop_variant",
    pop_option : [ValType::I32; 2] -> [ValType::I32] = "wit_dylib_pop_option",
    pop_result : [ValType::I32; 2] -> [ValType::I32] = "wit_dylib_pop_result",
    pop_string : [ValType::I32; 2] -> [ValType::I32] = "wit_dylib_pop_string",

    pop_list : [ValType::I32; 3] -> [ValType::I32] = "wit_dylib_pop_list",
    pop_iter_next : [ValType::I32; 2] -> [] = "wit_dylib_pop_iter_next",
    pop_iter : [ValType::I32; 2] -> [] = "wit_dylib_pop_iter",

    push_bool : [ValType::I32; 2] -> [] = "wit_dylib_push_bool",
    push_char : [ValType::I32; 2] -> [] = "wit_dylib_push_char",
    push_u8 : [ValType::I32; 2] -> [] = "wit_dylib_push_u8",
    push_s8 : [ValType::I32; 2] -> [] = "wit_dylib_push_s8",
    push_u16 : [ValType::I32; 2] -> [] = "wit_dylib_push_u16",
    push_s16 : [ValType::I32; 2] -> [] = "wit_dylib_push_s16",
    push_u32 : [ValType::I32; 2] -> [] = "wit_dylib_push_u32",
    push_s32 : [ValType::I32; 2] -> [] = "wit_dylib_push_s32",
    push_u64 : [ValType::I32, ValType::I64] -> [] = "wit_dylib_push_u64",
    push_s64 : [ValType::I32, ValType::I64] -> [] = "wit_dylib_push_s64",
    push_f32 : [ValType::I32, ValType::F32] -> [] = "wit_dylib_push_f32",
    push_f64 : [ValType::I32, ValType::F64] -> [] = "wit_dylib_push_f64",
    push_string : [ValType::I32; 3] -> [] = "wit_dylib_push_string",
    push_record : [ValType::I32; 2] -> [] = "wit_dylib_push_record",
    push_tuple : [ValType::I32; 2] -> [] = "wit_dylib_push_tuple",
    push_flags : [ValType::I32; 3] -> [] = "wit_dylib_push_flags",
    push_enum : [ValType::I32; 3] -> [] = "wit_dylib_push_enum",
    push_borrow : [ValType::I32; 3] -> [] = "wit_dylib_push_borrow",
    push_own : [ValType::I32; 3] -> [] = "wit_dylib_push_own",
    push_future : [ValType::I32; 3] -> [] = "wit_dylib_push_future",
    push_stream : [ValType::I32; 3] -> [] = "wit_dylib_push_stream",
    push_variant : [ValType::I32; 3] -> [] = "wit_dylib_push_variant",
    push_option : [ValType::I32; 3] -> [] = "wit_dylib_push_option",
    push_result : [ValType::I32; 3] -> [] = "wit_dylib_push_result",
    push_list : [ValType::I32; 4] -> [ValType::I32] = "wit_dylib_push_list",
    list_append : [ValType::I32; 2] -> [] = "wit_dylib_list_append",
}

pub fn import(
    adapter: &mut Adapter,
    resolve: &Resolve,
    func: &wit_parser::Function,
    abi: AbiVariant,
    import_idx: u32,
) -> Function {
    let sig = resolve.wasm_signature(abi, func);
    let mut c = FunctionCompiler::new(adapter, resolve, 1);
    let frame = c.stack_frame(func, &sig, abi);
    let fp = c.allocate_stack_frame(&frame);
    c.ctx = Some(TempLocal::new(0, ValType::I32));

    c.lower_import_params(func, &sig, &frame, fp.as_ref());
    c.ins().call(import_idx);
    c.lift_import_results(func, &sig, &frame, fp.as_ref());

    c.deallocate_lowered_lists();
    c.deallocate_stack_frame(&frame, fp);

    c.finish()
}

pub fn export(
    adapter: &mut Adapter,
    resolve: &Resolve,
    func: &wit_parser::Function,
    abi: AbiVariant,
    func_metadata_index: usize,
) -> (Function, bool) {
    let sig = resolve.wasm_signature(abi, func);
    let mut c = FunctionCompiler::new(adapter, resolve, sig.params.len() as u32);
    let frame = c.stack_frame(func, &sig, abi);
    let fp = c.allocate_stack_frame(&frame);

    let export_start = c.adapter.intrinsics().export_start;
    let export_call = c.adapter.intrinsics().export_call;

    c.ins().i32_const(func_metadata_index.try_into().unwrap());
    c.ins().call(export_start);
    c.ctx = Some(c.local_set_new_tmp(ValType::I32));

    c.lift_export_params(func, &sig);

    c.local_get_ctx();
    c.ins().i32_const(func_metadata_index.try_into().unwrap());
    c.ins().call(export_call);

    c.lower_export_results(func, &sig, &frame, fp.as_ref());

    // Note that this function intentionally does not deallocate the stack space
    // allocated above in `allocate_stack_frame`. That is the responsibility of
    // the post-return function to ensure that the owned value on the stack, the
    // return value, can get cleaned up properly. Additionally any dynamically
    // allocated lists will be stored on the stack and will get cleaned up
    // during post-return.
    //
    // Instead this actually allocates *more* stack space to save off `c.ctx`
    // into the stack-local memory. This also stores the dynamic number of lists
    // to deallocate if that was generated during this function.

    let sp = c.adapter.stack_pointer();
    c.ins().global_get(sp);
    c.ins().i32_const(8);
    c.ins().i32_sub();
    let l_scratch = c.local_tee_new_tmp(ValType::I32);
    c.ins().global_set(sp);

    let l_ctx = c.ctx.take().unwrap();
    c.ins().local_get(l_scratch.idx);
    c.ins().local_get(l_ctx.idx);
    c.ins().i32_store(MemArg {
        memory_index: 0,
        align: 2,
        offset: 0,
    });
    c.free_temp_local(l_ctx);

    let may_have_dynamic_lists_to_free = c.num_dynamic_lists_to_free.is_some();
    if let Some(l_num) = c.num_dynamic_lists_to_free.take() {
        c.ins().local_get(l_scratch.idx);
        c.ins().local_get(l_num.idx);
        c.ins().i32_store(MemArg {
            memory_index: 0,
            align: 2,
            offset: 4,
        });
        c.free_temp_local(l_num);
    }

    c.free_temp_local(l_scratch);
    if let Some(fp) = fp {
        c.free_temp_local(fp);
    }

    (c.finish(), may_have_dynamic_lists_to_free)
}

pub fn post_return(
    adapter: &mut Adapter,
    resolve: &Resolve,
    func: &wit_parser::Function,
    abi: AbiVariant,
    func_metadata_index: usize,
    may_have_dynamic_lists_to_free: bool,
) -> Function {
    let sig = resolve.wasm_signature(abi, func);
    let mut c = FunctionCompiler::new(adapter, resolve, sig.results.len() as u32);
    let frame = c.stack_frame(func, &sig, abi);
    let sp = c.adapter.stack_pointer();

    c.ins().global_get(sp);
    let l_sp = c.local_set_new_tmp(ValType::I32);

    // Load the `ctx` pointer for the export allocated during the originally
    // invoked function.
    c.ins().local_get(l_sp.idx);
    c.ins().i32_load(MemArg {
        memory_index: 0,
        align: 2,
        offset: 0,
    });
    c.ctx = Some(c.local_set_new_tmp(ValType::I32));

    // If the main exported function may have dynamically allocated lists during
    // lowering then the metadata for what to deallocate is at the bottom of the
    // stack. The last element is the number of lists to free, and then there's
    // N entries of what to free. Load the number here, then use
    // deallocate_lowered_lists` later to handle the actual deallocation and
    // restoration of the stack pointer.
    if may_have_dynamic_lists_to_free {
        c.ins().local_get(l_sp.idx);
        c.ins().i32_load(MemArg {
            memory_index: 0,
            align: 2,
            offset: 4,
        });
        c.num_dynamic_lists_to_free = Some(c.local_set_new_tmp(ValType::I32));
    }

    // Bump sp by 8 bytes to restore it to just before the end of the export
    // call originally.
    c.ins().local_get(l_sp.idx);
    c.free_temp_local(l_sp);
    c.ins().i32_const(8);
    c.ins().i32_add();
    c.ins().global_set(sp);

    // Restore the stack by discarding all lists only needed for the canonical
    // ABI which are no longer necessary.
    if c.num_dynamic_lists_to_free.is_some() {
        c.deallocate_lowered_lists();
    }

    // Tell the interpreter that the export has now finished.
    let export_finish = c.adapter.intrinsics().export_finish;
    c.local_get_ctx();
    c.ins().i32_const(func_metadata_index.try_into().unwrap());
    c.ins().call(export_finish);

    let ctx = c.ctx.take().unwrap();
    c.free_temp_local(ctx);

    // And finally deallocate the stack frame, if present.
    if frame.size > 0 {
        c.ins().global_get(sp);
        let fp = c.local_set_new_tmp(ValType::I32);
        c.deallocate_stack_frame(&frame, Some(fp));
    }
    c.finish()
}

struct FunctionCompiler<'a> {
    adapter: &'a mut Adapter,
    resolve: &'a Resolve,
    bytecode: Vec<u8>,
    locals: Vec<(u32, ValType)>,
    free_locals: HashMap<ValType, Vec<u32>>,
    nlocals: u32,
    using_temp_stack: u32,

    num_dynamic_lists_to_free: Option<TempLocal>,
    ctx: Option<TempLocal>,
}

impl<'a> FunctionCompiler<'a> {
    fn new(adapter: &'a mut Adapter, resolve: &'a Resolve, nlocals: u32) -> FunctionCompiler<'a> {
        FunctionCompiler {
            adapter,
            resolve,
            bytecode: Vec::new(),
            locals: Vec::new(),
            free_locals: HashMap::new(),
            nlocals,
            num_dynamic_lists_to_free: None,
            using_temp_stack: 0,
            ctx: None,
        }
    }

    fn ins(&mut self) -> InstructionSink<'_> {
        InstructionSink::new(&mut self.bytecode)
    }

    /// Generates a new local in this function of the `ty` specified,
    /// initializing it with the top value on the current wasm stack.
    ///
    /// The returned `TempLocal` must be freed after it is finished with
    /// `free_temp_local`.
    fn local_tee_new_tmp(&mut self, ty: ValType) -> TempLocal {
        let local = self.gen_temp_local(ty);
        self.ins().local_tee(local.idx);
        local
    }

    /// Same as `local_tee_new_tmp` but initializes the local with `LocalSet`
    /// instead of `LocalTee`.
    fn local_set_new_tmp(&mut self, ty: ValType) -> TempLocal {
        let local = self.gen_temp_local(ty);
        self.ins().local_set(local.idx);
        local
    }

    fn local_get_ctx(&mut self) {
        let idx = self.ctx.as_ref().unwrap().idx;
        self.ins().local_get(idx);
    }

    fn gen_temp_local(&mut self, ty: ValType) -> TempLocal {
        // First check to see if any locals are available in this function which
        // were previously generated but are no longer in use.
        if let Some(idx) = self.free_locals.get_mut(&ty).and_then(|v| v.pop()) {
            return TempLocal {
                ty,
                idx,
                needs_free: true,
            };
        }

        // Failing that generate a fresh new local.
        let locals = &mut self.locals;
        match locals.last_mut() {
            Some((cnt, prev_ty)) if ty == *prev_ty => *cnt += 1,
            _ => locals.push((1, ty)),
        }
        self.nlocals += 1;
        let idx = self.nlocals - 1;
        TempLocal {
            ty,
            idx,
            needs_free: true,
        }
    }

    /// Used to release a `TempLocal` from a particular lexical scope to allow
    /// its possible reuse in later scopes.
    fn free_temp_local(&mut self, mut local: TempLocal) {
        assert!(local.needs_free);
        self.free_locals
            .entry(local.ty)
            .or_insert(Vec::new())
            .push(local.idx);
        local.needs_free = false;
    }

    fn finish(self) -> Function {
        assert!(self.num_dynamic_lists_to_free.is_none());
        assert!(self.using_temp_stack == 0);
        let mut func = Function::new(self.locals);
        func.raw(self.bytecode);
        func.instructions().end();
        func
    }

    fn deallocate_lowered_lists(&mut self) {
        let dealloc_bytes = self.adapter.intrinsics().dealloc_bytes;

        let Some(l_dynamic) = self.num_dynamic_lists_to_free.take() else {
            return;
        };
        let sp = self.adapter.stack_pointer();
        self.ins().global_get(sp);
        let l_sp = self.local_set_new_tmp(ValType::I32);
        let memarg = |offset| MemArg {
            memory_index: 0,
            offset,
            align: 2,
        };
        self.ins().loop_(BlockType::Empty);
        {
            // if the counter is > 0 then keep going.
            self.ins().local_get(l_dynamic.idx);
            self.ins().if_(BlockType::Empty);
            {
                // Load ptr/len/align and deallocate
                self.ins().local_get(l_sp.idx);
                self.ins().i32_load(memarg(0));
                self.ins().local_get(l_sp.idx);
                self.ins().i32_load(memarg(4));
                self.ins().local_get(l_sp.idx);
                self.ins().i32_load(memarg(8));
                self.ins().call(dealloc_bytes);

                // Bump l_sp
                self.ins().local_get(l_sp.idx);
                self.ins().i32_const(16);
                self.ins().i32_add();
                self.ins().local_set(l_sp.idx);

                // Decrement l_dynamic
                self.ins().local_get(l_dynamic.idx);
                self.ins().i32_const(1);
                self.ins().i32_sub();
                self.ins().local_set(l_dynamic.idx);

                self.ins().br(1);
            }
            self.ins().end();
        }
        self.ins().end();

        self.ins().local_get(l_sp.idx);
        self.ins().global_set(sp);

        self.free_temp_local(l_sp);
        self.free_temp_local(l_dynamic);
    }

    fn deallocate_stack_frame(&mut self, frame: &StackFrame, fp: Option<TempLocal>) {
        if frame.size == 0 {
            return;
        }
        let fp = fp.unwrap();
        let sp = self.adapter.stack_pointer();
        self.ins().local_get(fp.idx);
        self.ins().i32_const(frame.size as i32);
        self.ins().i32_add();
        self.ins().global_set(sp);
        self.free_temp_local(fp);
    }

    fn stack_frame(
        &self,
        func: &wit_parser::Function,
        sig: &WasmSignature,
        abi: AbiVariant,
    ) -> StackFrame {
        // If a return pointer is required then allocate space for one. Note
        // that this is used for both imported and exported functions. The way
        // exports work is that we defer stack cleanup to happening in
        // post-return.
        let ret_size = if sig.retptr {
            let info = self.adapter.sizes.record(&func.result);
            assert!(info.align.align_wasm32() <= 8);

            info.size.size_wasm32().try_into().unwrap()
        } else {
            0
        };

        // Allocate space for the in-memory abi parameters for imports. Note
        // that exports are heap-allocated by the caller but for imports it's
        // the responsibility of the caller to provide an appropriate parameter.
        let abi_param_size = match abi {
            AbiVariant::GuestImport if sig.indirect_params => {
                let info = self
                    .adapter
                    .sizes
                    .params(func.params.iter().map(|(_, ty)| ty));
                info.size.size_wasm32().try_into().unwrap()
            }
            AbiVariant::GuestImport => 0,
            AbiVariant::GuestExport => 0,
            AbiVariant::GuestImportAsync => todo!("async"),
            AbiVariant::GuestExportAsync => todo!("async"),
            AbiVariant::GuestExportAsyncStackful => todo!("async"),
        };

        let size = align_up(align_up(ret_size, 4) + align_up(abi_param_size, 4), 8);
        let mut offset = 0;
        let abi_param_offset = if abi_param_size > 0 {
            let ret = offset;
            offset += align_up(abi_param_size, 4);
            Some(ret.try_into().unwrap())
        } else {
            None
        };
        let retptr_offset = if ret_size > 0 {
            let ret = offset;
            offset += align_up(ret_size, 4);
            Some(ret.try_into().unwrap())
        } else {
            None
        };

        let _ = offset;

        StackFrame {
            size: size.try_into().unwrap(),
            abi_param_offset,
            retptr_offset,
        }
    }

    fn allocate_stack_frame(&mut self, frame: &StackFrame) -> Option<TempLocal> {
        if frame.size == 0 {
            return None;
        }
        assert!(frame.size % 8 == 0);
        let sp = self.adapter.stack_pointer();
        self.ins().global_get(sp);
        self.ins().i32_const(frame.size as i32);
        self.ins().i32_sub();
        let ret = self.local_tee_new_tmp(ValType::I32);
        self.ins().global_set(sp);
        Some(ret)
    }

    fn lower_import_params(
        &mut self,
        func: &wit_parser::Function,
        sig: &WasmSignature,
        frame: &StackFrame,
        fp: Option<&TempLocal>,
    ) {
        let tys = func.params.iter().map(|(_, ty)| ty);

        if sig.indirect_params {
            let fp = fp.unwrap();
            let offset = frame.abi_param_offset.unwrap();
            let dsts = self.record_field_locs(
                &AbiLoc::Memory(Memory {
                    addr: TempLocal::new(fp.idx, fp.ty),
                    offset,
                }),
                tys.clone(),
            );
            for (param, dst) in tys.zip(dsts) {
                self.lower(*param, &dst);
            }

            self.ins().local_get(fp.idx);
            self.ins().i32_const(offset.try_into().unwrap());
            self.ins().i32_add();
        } else {
            let mut locals = sig
                .params
                .iter()
                .map(|ty| self.gen_temp_local(self.adapter.map_wasm_type(*ty)))
                .collect::<Vec<_>>();
            if sig.retptr {
                self.free_temp_local(locals.pop().unwrap());
            }
            let dsts = self.record_field_locs(
                &AbiLoc::Stack(&locals),
                func.params.iter().map(|(_, ty)| ty),
            );
            for (param, dst) in tys.zip(dsts) {
                self.lower(*param, &dst);
            }

            for local in locals {
                self.ins().local_get(local.idx);
                self.free_temp_local(local);
            }
        }

        if sig.retptr {
            let AbiLoc::Memory(mem) = frame.abi_loc(fp.unwrap()).unwrap() else {
                unreachable!()
            };
            self.ins().local_get(mem.addr.idx);
            self.ins().i32_const(mem.offset as i32);
            self.ins().i32_add();
        }
    }

    fn lift_import_results(
        &mut self,
        func: &wit_parser::Function,
        sig: &WasmSignature,
        frame: &StackFrame,
        fp: Option<&TempLocal>,
    ) {
        let ty = match func.result {
            Some(ty) => ty,
            None => {
                assert!(!sig.retptr);
                return;
            }
        };
        if sig.retptr {
            assert_eq!(sig.results.len(), 0);
            let src = frame.abi_loc(fp.unwrap()).unwrap();
            self.lift(&src, ty);
        } else {
            assert_eq!(sig.results.len(), 1);
            let tmp_ty = self.adapter.map_wasm_type(sig.results[0]);
            let tmp = self.local_set_new_tmp(tmp_ty);
            let src = AbiLoc::Stack(slice::from_ref(&tmp));
            self.lift(&src, ty);
            self.free_temp_local(tmp);
        }
    }

    fn lift_export_params(&mut self, func: &wit_parser::Function, sig: &WasmSignature) {
        let temps;
        let src = if sig.indirect_params {
            AbiLoc::Memory(Memory {
                addr: TempLocal::new(0, ValType::I32),
                offset: 0,
            })
        } else {
            temps = sig
                .params
                .iter()
                .enumerate()
                .map(|(i, ty)| TempLocal::new(i as u32, self.adapter.map_wasm_type(*ty)))
                .collect::<Vec<_>>();
            AbiLoc::Stack(&temps)
        };

        let srcs = self.record_field_locs(&src, func.params.iter().map(|(_, ty)| ty));
        for ((_, param), src) in func.params.iter().zip(srcs) {
            self.lift(&src, *param);
        }

        // If parameters were passed indirectly than the caller of this export
        // made a dynamic allocation with `cabi_realloc`. Cleanup said
        // allocation here after all the parameters were lowered onto the stack.
        if let AbiLoc::Memory(mem) = &src {
            assert!(sig.indirect_params);
            let info = self
                .adapter
                .sizes
                .record(func.params.iter().map(|(_, ty)| ty));
            self.ins().local_get(mem.addr.idx);
            assert_eq!(mem.offset, 0);
            self.ins()
                .i32_const(info.size.size_wasm32().try_into().unwrap());
            self.ins()
                .i32_const(info.align.align_wasm32().try_into().unwrap());
            let dealloc_bytes = self.adapter.intrinsics().dealloc_bytes;
            self.ins().call(dealloc_bytes);
        }
    }

    fn lower_export_results(
        &mut self,
        func: &wit_parser::Function,
        sig: &WasmSignature,
        frame: &StackFrame,
        fp: Option<&TempLocal>,
    ) {
        let ty = match func.result {
            Some(ty) => ty,
            None => {
                assert!(!sig.retptr);
                return;
            }
        };

        if sig.retptr {
            assert_eq!(sig.results.len(), 1);
            let dst = frame.abi_loc(fp.unwrap()).unwrap();
            self.lower(ty, &dst);

            let AbiLoc::Memory(mem) = dst else {
                unreachable!()
            };
            self.ins().local_get(mem.addr.idx);
            self.ins().i32_const(mem.offset as i32);
            self.ins().i32_add();
        } else {
            assert_eq!(sig.results.len(), 1);
            let tmp_ty = self.adapter.map_wasm_type(sig.results[0]);
            let tmp = self.gen_temp_local(tmp_ty);
            let dst = AbiLoc::Stack(slice::from_ref(&tmp));
            self.lower(ty, &dst);

            self.ins().local_get(tmp.idx);
            self.free_temp_local(tmp);
        }
    }

    fn lower(&mut self, ty: Type, dest: &AbiLoc<'_>) {
        let i = self.adapter.intrinsics();
        match ty {
            Type::Bool => self.pop_scalar(dest, ValType::I32, i.pop_bool, 0),
            Type::Char => self.pop_scalar(dest, ValType::I32, i.pop_char, 2),
            Type::U8 => self.pop_scalar(dest, ValType::I32, i.pop_u8, 0),
            Type::S8 => self.pop_scalar(dest, ValType::I32, i.pop_s8, 0),
            Type::U16 => self.pop_scalar(dest, ValType::I32, i.pop_u16, 1),
            Type::S16 => self.pop_scalar(dest, ValType::I32, i.pop_s16, 1),
            Type::U32 => self.pop_scalar(dest, ValType::I32, i.pop_u32, 2),
            Type::S32 => self.pop_scalar(dest, ValType::I32, i.pop_s32, 2),
            Type::U64 => self.pop_scalar(dest, ValType::I64, i.pop_u64, 3),
            Type::S64 => self.pop_scalar(dest, ValType::I64, i.pop_s64, 3),
            Type::F32 => self.pop_scalar(dest, ValType::F32, i.pop_f32, 2),
            Type::F64 => self.pop_scalar(dest, ValType::F64, i.pop_f64, 3),
            Type::ErrorContext => todo!("error-context"),
            Type::String => {
                let pop_string = i.pop_string;
                self.with_temp_stack(4, |me, addr| {
                    me.local_get_ctx();
                    me.ins().local_get(addr.idx);
                    me.ins().call(pop_string);
                    me.ins().local_get(addr.idx);
                    me.ins().i32_load(MemArg {
                        memory_index: 0,
                        offset: 0,
                        align: 2,
                    });
                });
                let (dst_ptr, dst_len) = dest.split_ptr_len();
                self.store_scalar_from_top_of_stack(&dst_ptr, ValType::I32, 2);
                self.store_scalar_from_top_of_stack(&dst_len, ValType::I32, 2);
            }
            Type::Id(id) => self.lower_id(id, dest),
        }
    }

    fn lower_id(&mut self, id: TypeId, dest: &AbiLoc<'_>) {
        let ty = self.adapter.type_map[&id];
        let i = self.adapter.intrinsics();
        match &self.resolve.types[id].kind {
            // Simple delegation to whatever this type points to
            TypeDefKind::Type(t) => self.lower(*t, dest),

            // Lowering values which are represented by a single raw wasm
            // value. Lowering requires contextual information though which is
            // the type of the value being lowered.
            TypeDefKind::Handle(Handle::Borrow(_)) => {
                let metadata::Type::Borrow(resource_index) = ty else {
                    unreachable!()
                };
                self.pop_typed_scalar(dest, resource_index, i.pop_borrow, 2);
            }
            TypeDefKind::Handle(Handle::Own(_)) => {
                let metadata::Type::Own(resource_index) = ty else {
                    unreachable!()
                };
                self.pop_typed_scalar(dest, resource_index, i.pop_own, 2);
            }
            TypeDefKind::Future(_) => {
                let metadata::Type::Future(future_index) = ty else {
                    unreachable!()
                };
                self.pop_typed_scalar(dest, future_index, i.pop_future, 2);
            }
            TypeDefKind::Stream(_) => {
                let metadata::Type::Stream(stream_index) = ty else {
                    unreachable!()
                };
                self.pop_typed_scalar(dest, stream_index, i.pop_stream, 2);
            }
            TypeDefKind::Flags(f) => {
                let metadata::Type::Flags(flags_index) = ty else {
                    unreachable!()
                };
                let size_log2 = match f.repr() {
                    FlagsRepr::U8 => 0,
                    FlagsRepr::U16 => 1,
                    FlagsRepr::U32(1) => 2,
                    FlagsRepr::U32(_) => unimplemented!(),
                };
                self.pop_typed_scalar(dest, flags_index, i.pop_flags, size_log2)
            }
            TypeDefKind::Enum(f) => {
                let metadata::Type::Enum(enum_index) = ty else {
                    unreachable!()
                };
                let size_log2 = match f.tag() {
                    Int::U8 => 0,
                    Int::U16 => 1,
                    Int::U32 => 2,
                    Int::U64 => 3,
                };
                self.pop_typed_scalar(dest, enum_index, i.pop_enum, size_log2)
            }

            // Lowering record-like thing, or consecutive sequences of values.
            TypeDefKind::Record(r) => {
                let metadata::Type::Record(record_index) = ty else {
                    unreachable!()
                };
                self.pop_record(
                    dest,
                    record_index,
                    i.pop_record,
                    r.fields.iter().map(|f| &f.ty),
                );
            }
            TypeDefKind::Tuple(t) => {
                let metadata::Type::Tuple(tuple_index) = ty else {
                    unreachable!()
                };
                self.pop_record(dest, tuple_index, i.pop_tuple, t.types.iter());
            }

            // Lowering a list has a fair bit of custom logic. The general idea
            // is that we'll query the engine if it has an already lowered
            // representation to which it can return NULL. If it's non-NULL then
            // that's returned directly (e.g. `list<u8>`) but if it's NULL
            // then the list is allocated manually with `cabi_realloc` and then
            // lowered element-by-element.
            TypeDefKind::List(t) => {
                let metadata::Type::List(list_index) = ty else {
                    unreachable!()
                };
                let pop_list = self.adapter.intrinsics().pop_list;
                let pop_iter_next = self.adapter.intrinsics().pop_iter_next;
                let pop_iter = self.adapter.intrinsics().pop_iter;
                let cabi_realloc = self.adapter.intrinsics().cabi_realloc;
                let size = self.adapter.sizes.size(t).size_wasm32();
                let align = self.adapter.sizes.align(t).align_wasm32();
                let (dst_ptr, dst_len) = dest.split_ptr_len();

                // Load the list ptr/len into locals
                self.with_temp_stack(4, |me, addr| {
                    me.local_get_ctx();
                    me.ins().i32_const(list_index.try_into().unwrap());
                    me.ins().local_get(addr.idx);
                    me.ins().call(pop_list);
                    me.ins().local_get(addr.idx);
                    me.ins().i32_load(MemArg {
                        memory_index: 0,
                        offset: 0,
                        align: 2,
                    });
                });
                let l_ptr = self.local_set_new_tmp(ValType::I32);
                let l_len = self.local_set_new_tmp(ValType::I32);

                // If the pointer is null then the interpreter doesn't support
                // the same canonical ABI view of this list so a loop is
                // required to lower element-by-element.
                self.ins().local_get(l_ptr.idx);
                self.ins().i32_eqz();
                self.ins().if_(BlockType::Empty);
                {
                    self.ins().i32_const(0);
                    let l_index = self.local_set_new_tmp(ValType::I32);

                    self.ins().i32_const(0);
                    self.ins().i32_const(0);
                    self.ins().i32_const(align.try_into().unwrap());
                    self.ins().local_get(l_len.idx);
                    self.ins().i32_const(size.try_into().unwrap());
                    self.ins().i32_mul();
                    let l_byte_size = self.local_tee_new_tmp(ValType::I32);
                    self.ins().call(cabi_realloc);
                    let l_elem_addr = self.local_tee_new_tmp(ValType::I32);
                    self.ins().local_set(l_ptr.idx);

                    // Loop over each element of the list.
                    self.ins().loop_(BlockType::Empty);
                    {
                        // Entry loop condition, `l_index != l_len`
                        self.ins().local_get(l_index.idx);
                        self.ins().local_get(l_len.idx);
                        self.ins().i32_ne();
                        self.ins().if_(BlockType::Empty);
                        {
                            // Get the `l_index`th element from `l_list`
                            self.local_get_ctx();
                            self.ins().i32_const(list_index.try_into().unwrap());
                            self.ins().call(pop_iter_next);

                            // Lower the list element
                            self.lower(
                                *t,
                                &AbiLoc::Memory(Memory {
                                    addr: TempLocal::new(l_elem_addr.idx, ValType::I32),
                                    offset: 0,
                                }),
                            );

                            // Increment the `l_index` counter
                            self.ins().local_get(l_index.idx);
                            self.ins().i32_const(1);
                            self.ins().i32_add();
                            self.ins().local_set(l_index.idx);

                            // Increment the `l_elem_addr` counter
                            self.ins().local_get(l_elem_addr.idx);
                            self.ins().i32_const(size.try_into().unwrap());
                            self.ins().i32_add();
                            self.ins().local_set(l_elem_addr.idx);

                            // Continue the loop.
                            self.ins().br(1);
                        }
                        self.ins().end();
                    }
                    self.ins().end();

                    self.defer_deallocate_lowered_list(&l_ptr, &l_byte_size, align);

                    self.local_get_ctx();
                    self.ins().i32_const(list_index.try_into().unwrap());
                    self.ins().call(pop_iter);

                    self.free_temp_local(l_index);
                    self.free_temp_local(l_elem_addr);
                    self.free_temp_local(l_byte_size);
                }
                self.ins().end();

                self.ins().local_get(l_ptr.idx);
                self.store_scalar_from_top_of_stack(&dst_ptr, ValType::I32, 2);
                self.ins().local_get(l_len.idx);
                self.store_scalar_from_top_of_stack(&dst_len, ValType::I32, 2);

                self.free_temp_local(l_len);
                self.free_temp_local(l_ptr);
            }

            // Variant-like items all go through the `lower_variant` helper.
            TypeDefKind::Variant(t) => {
                let metadata::Type::Variant(variant_index) = ty else {
                    unreachable!()
                };
                self.pop_variant(
                    dest,
                    variant_index,
                    i.pop_variant,
                    t.tag(),
                    t.cases.iter().map(|c| c.ty.as_ref()),
                );
            }
            TypeDefKind::Option(t) => {
                let metadata::Type::Option(option_index) = ty else {
                    unreachable!()
                };
                self.pop_variant(dest, option_index, i.pop_option, Int::U8, [None, Some(t)]);
            }
            TypeDefKind::Result(t) => {
                let metadata::Type::Result(result_index) = ty else {
                    unreachable!()
                };
                self.pop_variant(
                    dest,
                    result_index,
                    i.pop_result,
                    Int::U8,
                    [t.ok.as_ref(), t.err.as_ref()],
                );
            }

            // TODO: expand inline? Use a loop if `len` is too big? How much to
            // share with lowering a list above? Deal with it later.
            TypeDefKind::FixedSizeList(t, len) => {
                let _ = (t, len);
                todo!("fixed-size-list")
            }

            // Should not be possible to hit during lowering.
            TypeDefKind::Resource => unreachable!(),
            TypeDefKind::Unknown => unreachable!(),
        }
    }

    fn pop_record<'b>(
        &mut self,
        dest: &AbiLoc<'_>,
        type_index: usize,
        pop_intrinsic: u32,
        types: impl IntoIterator<Item = &'b Type> + Clone,
    ) {
        self.local_get_ctx();
        self.ins().i32_const(type_index.try_into().unwrap());
        self.ins().call(pop_intrinsic);

        let dsts = self.record_field_locs(dest, types.clone());
        for (ty, destination) in types.into_iter().zip(dsts) {
            self.lower(*ty, &destination);
        }
    }

    fn pop_variant<'b>(
        &mut self,
        dest: &AbiLoc<'_>,
        type_index: usize,
        pop_intrinsic: u32,
        tag: Int,
        payloads: impl IntoIterator<Item = Option<&'b Type>> + Clone,
    ) {
        let (discr_dst, payload_dst) =
            dest.split_discr_payload(self.adapter, tag, payloads.clone());
        let tag_size_log2 = match tag {
            Int::U8 => 0,
            Int::U16 => 1,
            Int::U32 => 2,
            Int::U64 => 3,
        };

        let push_zeros = |me: &mut Self, locals: &[TempLocal]| {
            for local in locals {
                match local.ty {
                    ValType::I32 => me.ins().i32_const(0),
                    ValType::I64 => me.ins().i64_const(0),
                    ValType::F32 => me.ins().f32_const(0.0.into()),
                    ValType::F64 => me.ins().f64_const(0.0.into()),
                    _ => unreachable!(),
                };
                me.ins().local_set(local.idx);
            }
        };

        let lower_payload = |me: &mut Self, ty: Option<&Type>| match ty {
            // With a payload the interpreter tells us what the payload value
            // is and then that's appropriately lowered depending on
            // stack/memory destination configuration. Note that for the stack
            // destination the lower operation stores in a prefix of the values
            // requested and the others are manually zero'd.
            Some(ty) => {
                let (dst, zero_locals) = payload_dst.narrow_payload(me.resolve, ty);
                me.lower(*ty, &dst);
                if let Some(zero_locals) = zero_locals {
                    push_zeros(me, zero_locals);
                }
            }

            // If there's no payload, then the only necessary thing is to push
            // zeros for a stack destination.
            None => match &payload_dst {
                AbiLoc::Stack(types) => push_zeros(me, types),
                AbiLoc::Memory(_) => {}
            },
        };
        match payloads.clone().into_iter().count() {
            // With only 2 cases (e.g. option and result) this can be a simple
            // if/else.
            2 => {
                let mut iter = payloads.clone().into_iter();
                let payload0 = iter.next().unwrap();
                let payload1 = iter.next().unwrap();
                self.local_get_ctx();
                self.ins().i32_const(type_index.try_into().unwrap());
                self.ins().call(pop_intrinsic);

                self.ins().if_(BlockType::Empty);
                self.ins().i32_const(1);
                self.store_scalar_from_top_of_stack(&discr_dst, ValType::I32, tag_size_log2);
                lower_payload(self, payload1);
                self.ins().else_();
                self.ins().i32_const(0);
                self.store_scalar_from_top_of_stack(&discr_dst, ValType::I32, tag_size_log2);
                lower_payload(self, payload0);
                self.ins().end();
            }
            n => {
                // AbiLoc block all cases will jump to
                self.ins().block(BlockType::Empty);

                // Block-per-case
                for _ in 0..n {
                    self.ins().block(BlockType::Empty);
                }

                // Block for an invalid discriminant
                self.ins().block(BlockType::Empty);

                // Block to jump out of
                self.ins().block(BlockType::Empty);
                self.local_get_ctx();
                self.ins().i32_const(type_index.try_into().unwrap());
                self.ins().call(pop_intrinsic);
                self.ins().br_table(1..(n + 1) as u32, 0);
                self.ins().end();

                // close the invalid discriminant block
                self.ins().unreachable();
                self.ins().end();

                for (i, ty) in payloads.clone().into_iter().enumerate() {
                    self.ins().i32_const(i.try_into().unwrap());
                    self.store_scalar_from_top_of_stack(&discr_dst, ValType::I32, tag_size_log2);
                    lower_payload(self, ty);
                    // jump to the outermost destination block with our results.
                    self.ins().br((n - i) as u32);
                    self.ins().end();
                }

                self.ins().end();
            }
        }
    }

    /// Lowers `src` as a "typed scalar" meaning that the `lower_intrinsic`
    /// specified takes the `type_index` argument first, then `src`, and returns
    /// a single wasm value of type `I32` which is then stored into `dest`.
    fn pop_typed_scalar(
        &mut self,
        dest: &AbiLoc<'_>,
        type_index: usize,
        push_intrinsic: u32,
        size_log2: u32,
    ) {
        self.local_get_ctx();
        self.ins().i32_const(type_index.try_into().unwrap());
        self.ins().call(push_intrinsic);
        self.store_scalar_from_top_of_stack(dest, ValType::I32, size_log2);
    }

    /// Lowers `src` as a scalar meaning that the `lower_intrinsic` specified
    /// takes `src` and returns a single wasm value of type `ty` which is then
    /// stored into `dest`.
    fn pop_scalar(&mut self, dest: &AbiLoc<'_>, ty: ValType, pop_intrinsic: u32, size_log2: u32) {
        self.local_get_ctx();
        self.ins().call(pop_intrinsic);
        self.store_scalar_from_top_of_stack(dest, ty, size_log2);
    }

    /// Stores a single wasm value of type `ty` on the wasm stack at `dest`
    fn store_scalar_from_top_of_stack(&mut self, dest: &AbiLoc<'_>, ty: ValType, size_log2: u32) {
        match dest {
            AbiLoc::Stack(locals) => {
                assert_eq!(locals.len(), 1);
                let local = &locals[0];
                let dst_ty = local.ty;
                match (ty, dst_ty) {
                    (ValType::I32, ValType::I32)
                    | (ValType::I64, ValType::I64)
                    | (ValType::F32, ValType::F32)
                    | (ValType::F64, ValType::F64) => {}

                    (ValType::F32, ValType::I32) => {
                        self.ins().i32_reinterpret_f32();
                    }
                    (ValType::I32, ValType::I64) => {
                        self.ins().i64_extend_i32_u();
                    }
                    (ValType::F64, ValType::I64) => {
                        self.ins().i64_reinterpret_f64();
                    }
                    (ValType::F32, ValType::I64) => {
                        self.ins().i32_reinterpret_f32();
                        self.ins().i64_extend_i32_u();
                    }

                    // should not be possible given the `join` function for
                    // variants
                    (ValType::I64, ValType::I32)
                    | (ValType::F64, ValType::I32)
                    | (ValType::I32, ValType::F32)
                    | (ValType::I64, ValType::F32)
                    | (ValType::F64, ValType::F32)
                    | (ValType::I32, ValType::F64)
                    | (ValType::I64, ValType::F64)
                    | (ValType::F32, ValType::F64)

                    // not used in the component model
                    | (ValType::Ref(_), _)
                    | (_, ValType::Ref(_))
                    | (ValType::V128, _)
                    | (_, ValType::V128) => {
                        panic!("cannot get {dst_ty:?} from {ty:?} local");
                    }
                }
                self.ins().local_set(local.idx);
            }
            AbiLoc::Memory(mem) => {
                let tmp = self.local_set_new_tmp(ty);
                let memarg = mem.memarg(size_log2);
                self.ins().local_get(mem.addr.idx);
                self.ins().local_get(tmp.idx);
                match (ty, size_log2) {
                    (ValType::I32, 0) => self.ins().i32_store8(memarg),
                    (ValType::I32, 1) => self.ins().i32_store16(memarg),
                    (ValType::I32, 2) => self.ins().i32_store(memarg),
                    (ValType::I64, 3) => self.ins().i64_store(memarg),
                    (ValType::F32, 2) => self.ins().f32_store(memarg),
                    (ValType::F64, 3) => self.ins().f64_store(memarg),
                    other => unimplemented!("{other:?}"),
                };
                self.free_temp_local(tmp);
            }
        }
    }

    fn lift(&mut self, src: &AbiLoc<'_>, ty: Type) {
        let i = self.adapter.intrinsics();
        match ty {
            Type::Bool => self.push_scalar(src, ValType::I32, i.push_bool, 0),
            Type::Char => self.push_scalar(src, ValType::I32, i.push_char, 2),
            Type::U8 => self.push_scalar(src, ValType::I32, i.push_u8, 0),
            Type::S8 => self.push_scalar(src, ValType::I32, i.push_s8, 0),
            Type::U16 => self.push_scalar(src, ValType::I32, i.push_u16, 1),
            Type::S16 => self.push_scalar(src, ValType::I32, i.push_s16, 1),
            Type::U32 => self.push_scalar(src, ValType::I32, i.push_u32, 2),
            Type::S32 => self.push_scalar(src, ValType::I32, i.push_s32, 2),
            Type::U64 => self.push_scalar(src, ValType::I64, i.push_u64, 3),
            Type::S64 => self.push_scalar(src, ValType::I64, i.push_s64, 3),
            Type::F32 => self.push_scalar(src, ValType::F32, i.push_f32, 2),
            Type::F64 => self.push_scalar(src, ValType::F64, i.push_f64, 3),
            Type::String => {
                let push_string = i.push_string;
                let (src_ptr, src_len) = src.split_ptr_len();
                self.local_get_ctx();
                self.load_abi_loc(&src_ptr, ValType::I32, 2);
                self.load_abi_loc(&src_len, ValType::I32, 2);
                self.ins().call(push_string);
            }
            Type::ErrorContext => todo!("error-context"),
            Type::Id(id) => self.lift_id(src, id),
        }
    }

    fn lift_id(&mut self, src: &AbiLoc<'_>, id: TypeId) {
        let ty = self.adapter.type_map[&id];
        let i = self.adapter.intrinsics();
        match &self.resolve.types[id].kind {
            // Simple delegation to whatever this type points to
            TypeDefKind::Type(t) => self.lift(src, *t),

            // Lowering record-like thing, or consecutive sequences of values.
            TypeDefKind::Record(r) => {
                let metadata::Type::Record(record_index) = ty else {
                    unreachable!()
                };
                self.push_record(
                    src,
                    record_index,
                    i.push_record,
                    r.fields.iter().map(|f| &f.ty),
                );
            }
            TypeDefKind::Tuple(t) => {
                let metadata::Type::Tuple(tuple_index) = ty else {
                    unreachable!()
                };
                self.push_record(src, tuple_index, i.push_tuple, t.types.iter());
            }

            // Lifting values which are represented by a single raw wasm
            // value. Lifting requires contextual information though which is
            // the type of the value being lowered.
            TypeDefKind::Handle(Handle::Borrow(_)) => {
                let metadata::Type::Borrow(resource_index) = ty else {
                    unreachable!()
                };
                self.push_typed_scalar(src, resource_index, i.push_borrow, 2);
            }
            TypeDefKind::Handle(Handle::Own(_)) => {
                let metadata::Type::Own(resource_index) = ty else {
                    unreachable!()
                };
                self.push_typed_scalar(src, resource_index, i.push_own, 2);
            }
            TypeDefKind::Future(_) => {
                let metadata::Type::Future(future_index) = ty else {
                    unreachable!()
                };
                self.push_typed_scalar(src, future_index, i.push_future, 2);
            }
            TypeDefKind::Stream(_) => {
                let metadata::Type::Stream(stream_index) = ty else {
                    unreachable!()
                };
                self.push_typed_scalar(src, stream_index, i.push_stream, 2);
            }
            TypeDefKind::Flags(f) => {
                let metadata::Type::Flags(flags_index) = ty else {
                    unreachable!()
                };
                let size_log2 = match f.repr() {
                    FlagsRepr::U8 => 0,
                    FlagsRepr::U16 => 1,
                    FlagsRepr::U32(1) => 2,
                    FlagsRepr::U32(_) => unimplemented!(),
                };
                self.push_typed_scalar(src, flags_index, i.push_flags, size_log2)
            }
            TypeDefKind::Enum(f) => {
                let metadata::Type::Enum(enum_index) = ty else {
                    unreachable!()
                };
                let size_log2 = match f.tag() {
                    Int::U8 => 0,
                    Int::U16 => 1,
                    Int::U32 => 2,
                    Int::U64 => 3,
                };
                self.push_typed_scalar(src, enum_index, i.push_enum, size_log2)
            }

            // TODO: expand inline? Use a loop if `len` is too big? How much to
            // share with lowering a list above? Deal with it later.
            TypeDefKind::FixedSizeList(t, len) => {
                let _ = (t, len);
                todo!("fixed-size-list")
            }

            // Variant-like items all go through the `lift_variant` helper.
            TypeDefKind::Variant(t) => {
                let metadata::Type::Variant(variant_index) = ty else {
                    unreachable!()
                };
                self.push_variant(
                    src,
                    variant_index,
                    i.push_variant,
                    t.tag(),
                    t.cases.iter().map(|c| c.ty.as_ref()),
                );
            }
            TypeDefKind::Option(t) => {
                let metadata::Type::Option(option_index) = ty else {
                    unreachable!()
                };
                self.push_variant(src, option_index, i.push_option, Int::U8, [None, Some(t)]);
            }
            TypeDefKind::Result(t) => {
                let metadata::Type::Result(result_index) = ty else {
                    unreachable!()
                };
                self.push_variant(
                    src,
                    result_index,
                    i.push_result,
                    Int::U8,
                    [t.ok.as_ref(), t.err.as_ref()],
                );
            }

            TypeDefKind::List(t) => {
                let metadata::Type::List(list_index) = ty else {
                    unreachable!()
                };
                let elem_size = self.adapter.sizes.size(t).size_wasm32();
                let elem_align = self.adapter.sizes.align(t).align_wasm32();
                let list_index = i32::try_from(list_index).unwrap();
                let push_list = i.push_list;
                let list_append = i.list_append;
                let dealloc_bytes = i.dealloc_bytes;
                let (src_ptr, src_len) = src.split_ptr_len();

                // Give the interpreter to lift the list exactly as-is and take
                // ownership of the allocation.
                self.local_get_ctx();
                self.ins().i32_const(list_index);
                self.load_abi_loc(&src_ptr, ValType::I32, 2);
                let l_ptr = self.local_tee_new_tmp(ValType::I32);
                self.load_abi_loc(&src_len, ValType::I32, 2);
                let l_len = self.local_tee_new_tmp(ValType::I32);
                self.ins().call(push_list);

                // If the interpreter returned 0 then the list needs to be
                // lifted manually element-by-element.
                self.ins().i32_eqz();
                self.ins().if_(BlockType::Empty);
                {
                    // Prep deallocation of the list after the loop by saving
                    // off the pointer/byte size.
                    self.ins().local_get(l_len.idx);
                    self.ins().i32_const(elem_size.try_into().unwrap());
                    self.ins().i32_mul();
                    let l_byte_size = self.local_set_new_tmp(ValType::I32);
                    self.ins().local_get(l_ptr.idx);
                    let l_ptr_to_free = self.local_set_new_tmp(ValType::I32);

                    // Using `l_len` as the loop counter, element-by-element
                    // lift from the list and push into the list.
                    self.ins().loop_(BlockType::Empty);
                    {
                        self.ins().local_get(l_len.idx);
                        self.ins().if_(BlockType::Empty);
                        {
                            // Lift this list element from memory into a local
                            let src = AbiLoc::Memory(Memory {
                                addr: TempLocal::new(l_ptr.idx, l_ptr.ty),
                                offset: 0,
                            });
                            self.lift(&src, *t);

                            // Push the lifted element onto the list. Note that
                            // this takes and returns the list.
                            self.local_get_ctx();
                            self.ins().i32_const(list_index);
                            self.ins().call(list_append);

                            // decrement the length counter
                            self.ins().local_get(l_len.idx);
                            self.ins().i32_const(1);
                            self.ins().i32_sub();
                            self.ins().local_set(l_len.idx);

                            // increment the pointer
                            self.ins().local_get(l_ptr.idx);
                            self.ins().i32_const(elem_size.try_into().unwrap());
                            self.ins().i32_add();
                            self.ins().local_set(l_ptr.idx);

                            self.ins().br(1);
                        }
                        self.ins().end();
                    }
                    self.ins().end();

                    // The canonical ABI representation of this list is no
                    // longer needed, so discard it.
                    self.ins().local_get(l_byte_size.idx);
                    self.ins().if_(BlockType::Empty);
                    {
                        self.ins().local_get(l_ptr_to_free.idx);
                        self.ins().local_get(l_byte_size.idx);
                        self.ins().i32_const(elem_align.try_into().unwrap());
                        self.ins().call(dealloc_bytes);
                    }
                    self.ins().end();

                    self.free_temp_local(l_byte_size);
                    self.free_temp_local(l_ptr_to_free);
                }
                self.ins().end();

                self.free_temp_local(l_ptr);
                self.free_temp_local(l_len);
            }

            // Should not be possible to hit during lifting.
            TypeDefKind::Resource => unreachable!(),
            TypeDefKind::Unknown => unreachable!(),
        }
    }

    fn push_record<'b>(
        &mut self,
        src: &AbiLoc<'_>,
        type_index: usize,
        push_intrinsic: u32,
        types: impl IntoIterator<Item = &'b Type> + Clone,
    ) {
        let srcs = self.record_field_locs(src, types.clone());
        for (ty, src) in types.into_iter().zip(srcs) {
            self.lift(&src, *ty);
        }

        self.local_get_ctx();
        self.ins().i32_const(type_index.try_into().unwrap());
        self.ins().call(push_intrinsic);
    }

    fn push_variant<'b>(
        &mut self,
        src: &AbiLoc<'_>,
        type_index: usize,
        push_intrinsic: u32,
        tag: Int,
        payloads: impl IntoIterator<Item = Option<&'b Type>> + Clone,
    ) {
        let (discr_src, payload_src) = src.split_discr_payload(self.adapter, tag, payloads.clone());
        let tag_size_log2 = match tag {
            Int::U8 => 0,
            Int::U16 => 1,
            Int::U32 => 2,
            Int::U64 => 3,
        };

        let lift_payload = |me: &mut Self, ty: Option<&Type>| match ty {
            Some(ty) => {
                let (src, _) = payload_src.narrow_payload(me.resolve, ty);
                me.lift(&src, *ty);
            }
            None => {}
        };

        match payloads.clone().into_iter().count() {
            // With only 2 cases (e.g. option and result) this can be a simple
            // if/else.
            2 => {
                let mut iter = payloads.clone().into_iter();
                let payload0 = iter.next().unwrap();
                let payload1 = iter.next().unwrap();

                self.load_abi_loc(&discr_src, ValType::I32, tag_size_log2);
                self.ins().if_(BlockType::Empty);
                {
                    lift_payload(self, payload1);
                    self.local_get_ctx();
                    self.ins().i32_const(type_index.try_into().unwrap());
                    self.ins().i32_const(1);
                    self.ins().call(push_intrinsic);
                }
                self.ins().else_();
                {
                    lift_payload(self, payload0);
                    self.local_get_ctx();
                    self.ins().i32_const(type_index.try_into().unwrap());
                    self.ins().i32_const(0);
                    self.ins().call(push_intrinsic);
                }
                self.ins().end();
            }
            n => {
                // Continue-onwards block everything jumps to
                self.ins().block(BlockType::Empty);

                // Block-per-case
                for _ in 0..n {
                    self.ins().block(BlockType::Empty);
                }

                // Block for an invalid discriminant
                self.ins().block(BlockType::Empty);

                // Block to jump out of
                self.ins().block(BlockType::Empty);
                self.load_abi_loc(&discr_src, ValType::I32, tag_size_log2);
                self.ins().br_table(1..(n + 1) as u32, 0);
                self.ins().end();

                // close the invalid discriminant block
                self.ins().unreachable();
                self.ins().end();

                for (i, ty) in payloads.clone().into_iter().enumerate() {
                    lift_payload(self, ty);
                    self.local_get_ctx();
                    self.ins().i32_const(type_index.try_into().unwrap());
                    self.ins().i32_const(i.try_into().unwrap());
                    self.ins().call(push_intrinsic);
                    // jump to the outermost destination block with our results.
                    self.ins().br((n - i) as u32);
                    self.ins().end();
                }

                self.ins().end();
            }
        }
    }

    /// Loads `src` and converts it to this language's representation with
    /// `push_intrinsic`.
    fn push_typed_scalar(
        &mut self,
        src: &AbiLoc<'_>,
        type_index: usize,
        push_intrinsic: u32,
        size_log2: u32,
    ) {
        self.local_get_ctx();
        self.ins().i32_const(type_index.try_into().unwrap());
        self.load_abi_loc(src, ValType::I32, size_log2);
        self.ins().call(push_intrinsic);
    }

    fn push_scalar(&mut self, src: &AbiLoc<'_>, ty: ValType, push_intrinsic: u32, size_log2: u32) {
        self.local_get_ctx();
        self.load_abi_loc(src, ty, size_log2);
        self.ins().call(push_intrinsic);
    }

    fn load_abi_loc(&mut self, src: &AbiLoc<'_>, ty: ValType, size_log2: u32) {
        match src {
            AbiLoc::Stack([local]) => {
                self.ins().local_get(local.idx);
                match (local.ty, ty) {
                    (ValType::I32, ValType::I32)
                    | (ValType::I64, ValType::I64)
                    | (ValType::F32, ValType::F32)
                    | (ValType::F64, ValType::F64) => {}

                    (ValType::I32, ValType::F32) => {
                        self.ins().f32_reinterpret_i32();
                    }
                    (ValType::I64, ValType::I32) => {
                        self.ins().i32_wrap_i64();
                    }
                    (ValType::I64, ValType::F64) => {
                        self.ins().f64_reinterpret_i64();
                    }
                    (ValType::I64, ValType::F32) => {
                        self.ins().i32_wrap_i64();
                        self.ins().f32_reinterpret_i32();
                    }

                    // should not be possible given the `join` function for
                    // variants
                    (ValType::I32, ValType::I64)
                    | (ValType::F64, ValType::I32)
                    | (ValType::F32, ValType::I32)
                    | (ValType::F32, ValType::I64)
                    | (ValType::F64, ValType::F32)
                    | (ValType::I32, ValType::F64)
                    | (ValType::F64, ValType::I64)
                    | (ValType::F32, ValType::F64)

                    // not used in the component model
                    | (ValType::Ref(_), _)
                    | (_, ValType::Ref(_))
                    | (ValType::V128, _)
                    | (_, ValType::V128) => {
                        panic!("cannot convert {:?} to {ty:?}", local.ty)
                    }
                }
            }
            AbiLoc::Stack(_) => unreachable!(),
            AbiLoc::Memory(mem) => {
                self.ins().local_get(mem.addr.idx);
                let memarg = mem.memarg(size_log2);
                match (ty, size_log2) {
                    (ValType::I32, 0) => self.ins().i32_load8_u(memarg),
                    (ValType::I32, 1) => self.ins().i32_load16_u(memarg),
                    (ValType::I32, 2) => self.ins().i32_load(memarg),
                    (ValType::I64, 3) => self.ins().i64_load(memarg),
                    (ValType::F32, 2) => self.ins().f32_load(memarg),
                    (ValType::F64, 3) => self.ins().f64_load(memarg),
                    other => unimplemented!("{other:?}"),
                };
            }
        }
    }

    /// Returns the destinations used by the `fields` for a record when storing
    /// the record at `dst`
    fn record_field_locs<'b, 'c>(
        &self,
        dst: &AbiLoc<'b>,
        fields: impl IntoIterator<Item = &'c Type>,
    ) -> Vec<AbiLoc<'b>> {
        match dst {
            AbiLoc::Stack(types) => {
                let mut types = &types[..];
                fields
                    .into_iter()
                    .map(|ty| {
                        let n = flat_types(self.resolve, ty).unwrap().len();
                        let (a, b) = types.split_at(n);
                        types = b;
                        AbiLoc::Stack(a)
                    })
                    .collect()
            }
            AbiLoc::Memory(mem) => self
                .adapter
                .sizes
                .field_offsets(fields)
                .iter()
                .map(|(size, _ty)| mem.bump(size.size_wasm32()))
                .map(AbiLoc::Memory)
                .collect(),
        }
    }

    fn defer_deallocate_lowered_list(
        &mut self,
        l_ptr: &TempLocal,
        l_byte_size: &TempLocal,
        align: usize,
    ) {
        let sp = self.adapter.stack_pointer();
        assert!(self.using_temp_stack == 0);

        let num_lists = if self.num_dynamic_lists_to_free.is_some() {
            self.num_dynamic_lists_to_free.as_ref().unwrap().idx
        } else {
            self.ins().i32_const(0);
            let tmp = self.local_set_new_tmp(ValType::I32);
            let ret = tmp.idx;
            self.num_dynamic_lists_to_free = Some(tmp);
            ret
        };

        // Only defer a cleanup if this list is of nonzero size.
        self.ins().local_get(l_byte_size.idx);
        self.ins().if_(BlockType::Empty);
        {
            // Increase the number of lists to deallocate
            self.ins().local_get(num_lists);
            self.ins().i32_const(1);
            self.ins().i32_add();
            self.ins().local_set(num_lists);

            // Store the ptr/size/align onto the stack after allocating 16 more
            // bytes of stack space. (16 instead of 12 to keep the stack
            // aligned)
            self.ins().global_get(sp);
            self.ins().i32_const(16);
            self.ins().i32_sub();
            let addr = self.local_tee_new_tmp(ValType::I32);
            self.ins().global_set(sp);

            let memarg = |offset| MemArg {
                memory_index: 0,
                offset,
                align: 2,
            };
            self.ins().local_get(addr.idx);
            self.ins().local_get(l_ptr.idx);
            self.ins().i32_store(memarg(0));
            self.ins().local_get(addr.idx);
            self.ins().local_get(l_byte_size.idx);
            self.ins().i32_store(memarg(4));
            self.ins().local_get(addr.idx);
            self.ins().i32_const(align.try_into().unwrap());
            self.ins().i32_store(memarg(8));

            self.free_temp_local(addr);
        }
        self.ins().end();
    }

    fn with_temp_stack(&mut self, size: usize, f: impl FnOnce(&mut Self, &TempLocal)) {
        self.using_temp_stack += 1;
        let sp = self.adapter.stack_pointer();

        self.ins().global_get(sp);
        self.ins().i32_const(size.try_into().unwrap());
        self.ins().i32_sub();
        let tmp = self.local_tee_new_tmp(ValType::I32);
        self.ins().global_set(sp);

        f(self, &tmp);

        self.ins().local_get(tmp.idx);
        self.ins().i32_const(size.try_into().unwrap());
        self.ins().i32_add();
        self.ins().global_set(sp);

        self.using_temp_stack -= 1;
        self.free_temp_local(tmp);
    }
}

struct StackFrame {
    size: u32,
    abi_param_offset: Option<u32>,
    retptr_offset: Option<u32>,
}

impl StackFrame {
    fn abi_loc(&self, base: &TempLocal) -> Option<AbiLoc<'_>> {
        let offset = self.retptr_offset?;
        Some(AbiLoc::Memory(Memory {
            addr: TempLocal::new(base.idx, base.ty),
            offset,
        }))
    }
}

fn flat_types(resolve: &Resolve, ty: &Type) -> Option<Vec<WasmType>> {
    let mut storage = [WasmType::I32; Resolve::MAX_FLAT_PARAMS];
    let mut flat = FlatTypes::new(&mut storage);
    if resolve.push_flat(ty, &mut flat) {
        Some(flat.to_vec())
    } else {
        None
    }
}

/// Same as `Source` but for where values are translated into.
enum AbiLoc<'a> {
    /// This value is destined for the WebAssembly stack which means that
    /// results are simply pushed as we go along.
    ///
    /// The types listed are the types that are expected to be on the stack at
    /// the end of translation.
    Stack(&'a [TempLocal]),

    /// This value is to be placed in linear memory described by `Memory`.
    Memory(Memory),
}

impl<'a> AbiLoc<'a> {
    fn split_ptr_len(&self) -> (Self, Self) {
        match self {
            AbiLoc::Stack([dst_ptr, dst_len]) => (
                AbiLoc::Stack(slice::from_ref(dst_ptr)),
                AbiLoc::Stack(slice::from_ref(dst_len)),
            ),
            AbiLoc::Stack(_) => unreachable!(),
            AbiLoc::Memory(mem) => (AbiLoc::Memory(mem.bump(0)), AbiLoc::Memory(mem.bump(4))),
        }
    }

    fn split_discr_payload<'b>(
        &self,
        adapter: &Adapter,
        tag: Int,
        payloads: impl IntoIterator<Item = Option<&'b Type>>,
    ) -> (Self, Self) {
        match self {
            AbiLoc::Stack([discr, rest @ ..]) => {
                (AbiLoc::Stack(slice::from_ref(discr)), AbiLoc::Stack(rest))
            }
            AbiLoc::Stack([]) => unreachable!(),
            AbiLoc::Memory(mem) => {
                let offset = adapter.sizes.payload_offset(tag, payloads);
                (
                    AbiLoc::Memory(mem.bump(0)),
                    AbiLoc::Memory(mem.bump(offset.size_wasm32())),
                )
            }
        }
    }

    fn narrow_payload(&self, resolve: &Resolve, ty: &Type) -> (Self, Option<&'a [TempLocal]>) {
        match self {
            AbiLoc::Stack(locals) => {
                let flat_count = flat_types(resolve, ty).unwrap().len();
                let (lower_locals, rest) = locals.split_at(flat_count);
                (AbiLoc::Stack(lower_locals), Some(rest))
            }
            AbiLoc::Memory(m) => (AbiLoc::Memory(m.bump(0)), None),
        }
    }
}

/// Representation of where a value is going to be stored in linear memory.
struct Memory {
    /// The index of the local that contains the base address of where the
    /// storage is happening.
    addr: TempLocal,
    /// A static offset that will be baked into wasm instructions for where
    /// memory loads/stores happen.
    offset: u32,
}

impl Memory {
    fn memarg(&self, align: u32) -> MemArg {
        MemArg {
            offset: u64::from(self.offset),
            align,
            memory_index: 0,
        }
    }

    fn bump(&self, offset: usize) -> Memory {
        Memory {
            addr: TempLocal::new(self.addr.idx, self.addr.ty),
            offset: self.offset + u32::try_from(offset).unwrap(),
        }
    }
}

struct TempLocal {
    idx: u32,
    ty: ValType,
    needs_free: bool,
}

impl TempLocal {
    fn new(idx: u32, ty: ValType) -> TempLocal {
        TempLocal {
            idx,
            ty,
            needs_free: false,
        }
    }
}

impl Drop for TempLocal {
    fn drop(&mut self) {
        if self.needs_free {
            if std::thread::panicking() {
                eprintln!("temporary local not free'd");
            } else {
                panic!("temporary local not free'd");
            }
        }
    }
}

fn align_up(a: usize, align: usize) -> usize {
    assert!(align.is_power_of_two());
    (a + (align - 1)) & !(align - 1)
}
