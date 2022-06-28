#![allow(unused_variables)] // TODO FITZGEN

use crate::core::*;
use wasm_encoder::{Instruction, ValType};

const WASM_PAGE_SIZE: u64 = 65_536;

impl Module {
    /// Ensure that this generated module will never trap.
    ///
    /// This will take a number of approaches to avoid traps, such as
    ///
    /// * mask loads' and stores' addresses to the associated memory's size
    ///
    /// * mask `table.get`s' and `table.set`'s index to the associated table's size
    ///
    /// * ensure that a divisor is never zero
    ///
    /// * replace `unreachable`s with dummy-value `return`s
    ///
    /// * Masking data and element segments' offsets to be in bounds of their
    /// associated tables or memories
    ///
    /// TODO: floating point conversion instructions that trap
    pub fn no_traps(&mut self) {
        self.no_trapping_segments();

        for (i, code) in self.code.iter_mut().enumerate() {
            let mut new_insts = vec![];

            let insts = match &mut code.instructions {
                Instructions::Generated(is) => std::mem::replace(is, vec![]),
                Instructions::Arbitrary(_) => unreachable!(),
            };

            for inst in insts {
                match inst {
                    // Replace `unreachable` with an early return of dummy values.
                    //
                    // We *could* instead abstractly interpret all these
                    // instructions and maintain a stack of types (the way the
                    // validation algorithm does) and insert dummy values
                    // whenever an instruction expects a value of type `T` but
                    // there is an `unreachable` on the stack. This would allow
                    // us to keep executing the rest of the code following the
                    // `unreachable`, but also is a ton more work, and it isn't
                    // clear that it would pay for itself.
                    Instruction::Unreachable => {
                        let func_ty = &self.funcs[i].1;
                        for ty in &func_ty.results {
                            new_insts.push(dummy_value_inst(*ty));
                        }
                        new_insts.push(Instruction::Return);
                    }

                    // We have no way to reflect, at run time, on a `funcref` in
                    // the `i`th slot in a table and dynamically avoid trapping
                    // `call_indirect`s. Therefore, we can't emit *any*
                    // `call_indirect` instructions. Instead, we consume the
                    // arguments and generate dummy results.
                    Instruction::CallIndirect { ty, table } => {
                        let func_ty = &self.funcs[i].1;

                        // When we can, avoid emitting `drop`s to consume the
                        // arguments when possible, since dead code isn't
                        // usually an interesting thing to give to a Wasm
                        // compiler. Instead, prefer writing them to the first
                        // page of the first memory if possible.
                        let can_store_args_to_memory = func_ty.params.len()
                            < usize::try_from(WASM_PAGE_SIZE).unwrap()
                            && self.memories.get(0).map_or(false, |m| m.minimum > 0);
                        let memory_64 = self.memories.get(0).map_or(false, |m| m.memory64);
                        let address = if memory_64 {
                            Instruction::I64Const(0)
                        } else {
                            Instruction::I32Const(0)
                        };
                        let memarg = wasm_encoder::MemArg {
                            offset: 0,
                            align: 0,
                            memory_index: 0,
                        };

                        for ty in func_ty.params.iter().rev() {
                            match ty {
                                ValType::I32 if can_store_args_to_memory => {
                                    new_insts.push(address.clone());
                                    new_insts.push(Instruction::I32Store(memarg));
                                }
                                ValType::I64 if can_store_args_to_memory => {
                                    new_insts.push(address.clone());
                                    new_insts.push(Instruction::I64Store(memarg));
                                }
                                ValType::F32 if can_store_args_to_memory => {
                                    new_insts.push(address.clone());
                                    new_insts.push(Instruction::F32Store(memarg));
                                }
                                ValType::F64 if can_store_args_to_memory => {
                                    new_insts.push(address.clone());
                                    new_insts.push(Instruction::F64Store(memarg));
                                }
                                ValType::V128 if can_store_args_to_memory => {
                                    new_insts.push(address.clone());
                                    new_insts.push(Instruction::V128Store { memarg });
                                }
                                _ => {
                                    new_insts.push(Instruction::Drop);
                                }
                            }
                        }

                        for ty in &func_ty.results {
                            new_insts.push(dummy_value_inst(*ty));
                        }
                    }

                    // For loads, we dynamically check whether the load will
                    // trap, and if it will then we generate a dummy value to
                    // use instead.
                    Instruction::I32Load(memarg)
                    | Instruction::I64Load(memarg)
                    | Instruction::F32Load(memarg)
                    | Instruction::F64Load(memarg)
                    | Instruction::I32Load8_S(memarg)
                    | Instruction::I32Load8_U(memarg)
                    | Instruction::I32Load16_S(memarg)
                    | Instruction::I32Load16_U(memarg)
                    | Instruction::I64Load8_S(memarg)
                    | Instruction::I64Load8_U(memarg)
                    | Instruction::I64Load16_S(memarg)
                    | Instruction::I64Load16_U(memarg)
                    | Instruction::I64Load32_S(memarg)
                    | Instruction::I64Load32_U(memarg)
                    | Instruction::V128Load { memarg }
                    | Instruction::V128Load8x8S { memarg }
                    | Instruction::V128Load8x8U { memarg }
                    | Instruction::V128Load16x4S { memarg }
                    | Instruction::V128Load16x4U { memarg }
                    | Instruction::V128Load32x2S { memarg }
                    | Instruction::V128Load32x2U { memarg }
                    | Instruction::V128Load8Splat { memarg }
                    | Instruction::V128Load16Splat { memarg }
                    | Instruction::V128Load32Splat { memarg }
                    | Instruction::V128Load64Splat { memarg }
                    | Instruction::V128Load32Zero { memarg }
                    | Instruction::V128Load64Zero { memarg } => {
                        let memory = &self.memories[memarg.memory_index as usize];
                        let address_type = if memory.memory64 {
                            ValType::I64
                        } else {
                            ValType::I32
                        };

                        // Add a temporary local to hold this load's address.
                        let address_local = u32::try_from(code.locals.len()).unwrap();
                        code.locals.push(address_type);

                        // Add a temporary local to hold the result of this load.
                        let load_type = type_of_memory_access(&inst);
                        let result_local = u32::try_from(code.locals.len()).unwrap();
                        code.locals.push(load_type);

                        // [address:address_type]
                        new_insts.push(Instruction::LocalSet(address_local));
                        // []
                        new_insts.push(Instruction::Block(wasm_encoder::BlockType::Empty));
                        {
                            // []
                            new_insts.push(Instruction::Block(wasm_encoder::BlockType::Empty));
                            {
                                // []
                                new_insts.push(Instruction::MemorySize(memarg.memory_index));
                                // [mem_size_in_pages:address_type]
                                new_insts.push(int_const_inst(address_type, 65_536));
                                // [mem_size_in_pages:address_type wasm_page_size:address_type]
                                new_insts.push(int_mul_inst(address_type));
                                // [mem_size_in_bytes:address_type]
                                new_insts.push(int_const_inst(
                                    address_type,
                                    (memarg.offset + size_of_type_in_memory(load_type)) as i64,
                                ));
                                // [mem_size_in_bytes:address_type offset_and_size:address_type]
                                new_insts.push(Instruction::LocalGet(address_local));
                                // [mem_size_in_bytes:address_type offset_and_size:address_type address:address_type]
                                new_insts.push(int_add_inst(address_type));
                                // [mem_size_in_bytes:address_type highest_byte_accessed:address_type]
                                new_insts.push(int_le_u_inst(address_type));
                                // [load_will_trap:i32]
                                new_insts.push(Instruction::BrIf(0));
                                // []
                                new_insts.push(Instruction::LocalGet(address_local));
                                // [address:address_type]
                                new_insts.push(inst);
                                // [result:load_type]
                                new_insts.push(Instruction::LocalSet(result_local));
                                // []
                                new_insts.push(Instruction::Br(1));
                                // <unreachable>
                            }
                            // []
                            new_insts.push(Instruction::End);
                            // []
                            new_insts.push(dummy_value_inst(load_type));
                            // [dummy_value:load_type]
                            new_insts.push(Instruction::LocalSet(result_local));
                            // []
                        }
                        // []
                        new_insts.push(Instruction::End);
                        // []
                        new_insts.push(Instruction::LocalGet(result_local));
                        // [result:load_type]
                    }

                    // Stores are similar to loads: we check whether the store
                    // will trap, and if it will then we just drop the value.
                    Instruction::I32Store(_) => todo!(),
                    Instruction::I64Store(_) => todo!(),
                    Instruction::F32Store(_) => todo!(),
                    Instruction::F64Store(_) => todo!(),
                    Instruction::I32Store8(_) => todo!(),
                    Instruction::I32Store16(_) => todo!(),
                    Instruction::I64Store8(_) => todo!(),
                    Instruction::I64Store16(_) => todo!(),
                    Instruction::I64Store32(_) => todo!(),
                    Instruction::V128Store { memarg } => todo!(),
                    Instruction::V128Load8Lane { memarg, lane: _ } => todo!(),
                    Instruction::V128Load16Lane { memarg, lane: _ } => todo!(),
                    Instruction::V128Load32Lane { memarg, lane: _ } => todo!(),
                    Instruction::V128Load64Lane { memarg, lane: _ } => todo!(),
                    Instruction::V128Store8Lane { memarg, lane: _ } => todo!(),
                    Instruction::V128Store16Lane { memarg, lane: _ } => todo!(),
                    Instruction::V128Store32Lane { memarg, lane: _ } => todo!(),
                    Instruction::V128Store64Lane { memarg, lane: _ } => todo!(),

                    Instruction::MemoryCopy { src, dst } => todo!(),
                    Instruction::MemoryFill(_) => todo!(),
                    Instruction::MemoryInit { mem, data } => todo!(),

                    /*
                    Unsigned integer division and remainder will trap when
                    the divisor is 0. To avoid the trap, we will set any 0
                    divisors to 1 prior to the operation
                    The code below is equivalent to this expression:

                        local.set $temp_divisor
                        (select (i32.eqz (local.get $temp_divisor) (i32.const 1) (local.get $temp_divisor))
                    */
                    Instruction::I32RemU
                    | Instruction::I64RemU
                    | Instruction::I64DivU
                    | Instruction::I32DivU => {
                        let op_type = type_of_integer_operation(&inst);
                        let temp_divisor = u32::try_from(code.locals.len()).unwrap();
                        code.locals.push(op_type);
                        let temp_divisor = u32::try_from(code.locals.len()).unwrap();
                        code.locals.push(op_type);

                        // [dividend:op_type divisor:op_type]
                        new_insts.push(Instruction::LocalSet(temp_divisor));
                        // [dividend:op_type]
                        new_insts.push(int_const_inst(op_type, 1));
                        // [dividend:op_type 1:op_type]
                        new_insts.push(Instruction::LocalGet(temp_divisor));
                        // [dividend:op_type 1:op_type divisor:op_type]
                        new_insts.push(Instruction::LocalGet(temp_divisor));
                        // [dividend:op_type 1:op_type divisor:op_type divisor:op_type]
                        new_insts.push(eqz_inst(op_type));
                        // [dividend:op_type 1:op_type divisor:op_type is_zero:i32]
                        new_insts.push(Instruction::Select);
                        // [dividend:op_type divisor:op_type]
                        new_insts.push(inst);
                        // [result:op_type]
                    }

                    /*
                    Signed division and remainder will trap in the following instances:
                        - The divisor is 0
                        - The result of the division is 2^(n-1)
                    */
                    Instruction::I32DivS
                    | Instruction::I32RemS
                    | Instruction::I64DivS
                    | Instruction::I64RemS => {
                        // If divisor is 0, replace with 1
                        let op_type = type_of_integer_operation(&inst);
                        let temp_divisor = u32::try_from(code.locals.len()).unwrap();
                        code.locals.push(op_type);

                        // [dividend:op_type divisor:op_type]
                        new_insts.push(Instruction::LocalSet(temp_divisor));
                        // [dividend:op_type]
                        new_insts.push(int_const_inst(op_type, 1));
                        // [dividend:op_type 1:op_type]
                        new_insts.push(Instruction::LocalGet(temp_divisor));
                        // [dividend:op_type 1:op_type divisor:op_type]
                        new_insts.push(Instruction::LocalGet(temp_divisor));
                        // [dividend:op_type 1:op_type divisor:op_type divisor:op_type]
                        new_insts.push(eqz_inst(op_type));
                        // [dividend:op_type 1:op_type divisor:op_type is_zero:i32]
                        new_insts.push(Instruction::Select);
                        // [dividend:op_type divisor:op_type]

                        // If dividend and divisor are -int.max and -1, replace
                        // divisor with 1?
                        let temp_dividend = u32::try_from(code.locals.len()).unwrap();
                        code.locals.push(op_type);
                        new_insts.push(Instruction::Block(wasm_encoder::BlockType::Empty));
                        {
                            new_insts.push(Instruction::Block(wasm_encoder::BlockType::Empty));
                            {
                                // [dividend:op_type divisor:op_type]
                                new_insts.push(Instruction::LocalSet(temp_divisor));
                                // [dividend:op_type]
                                new_insts.push(Instruction::LocalTee(temp_dividend));
                                // [dividend:op_type]
                                new_insts.push(int_min_const_inst(op_type));
                                // [dividend:op_type int_min:op_type]
                                new_insts.push(int_ne_inst(op_type));
                                // [not_int_min:i32]
                                new_insts.push(Instruction::BrIf(0));
                                // []
                                new_insts.push(Instruction::LocalGet(temp_divisor));
                                // [divisor:op_type]
                                new_insts.push(int_const_inst(op_type, -1));
                                // [divisor:op_type -1:op_type]
                                new_insts.push(int_ne_inst(op_type));
                                // [not_neg_one:i32]
                                new_insts.push(Instruction::BrIf(0));
                                // []
                                new_insts.push(Instruction::LocalGet(temp_dividend));
                                // [dividend:op_type]
                                new_insts.push(int_const_inst(op_type, 1));
                                // [dividend:op_type divisor:op_type]
                                new_insts.push(Instruction::Br(1));
                            }
                            // []
                            new_insts.push(Instruction::LocalGet(temp_dividend));
                            // [dividend:op_type]
                            new_insts.push(Instruction::LocalGet(temp_divisor));
                            // [dividend:op_type divisor:op_type]
                        }
                        // [dividend:op_type divisor:op_type]
                        new_insts.push(inst);
                    }

                    Instruction::I32TruncF32S => todo!(),
                    Instruction::I32TruncF32U => todo!(),
                    Instruction::I32TruncF64S => todo!(),
                    Instruction::I32TruncF64U => todo!(),
                    Instruction::I64TruncF32S => todo!(),
                    Instruction::I64TruncF32U => todo!(),
                    Instruction::I64TruncF64S => todo!(),
                    Instruction::I64TruncF64U => todo!(),

                    Instruction::TableFill { table } => todo!(),
                    Instruction::TableSet { table } => todo!(),
                    Instruction::TableGet { table } => todo!(),
                    Instruction::TableInit { segment, table } => todo!(),
                    Instruction::TableCopy { src, dst } => todo!(),

                    // None of the other instructions can trap, so just copy them over.
                    inst => new_insts.push(inst),
                }
            }

            code.instructions = Instructions::Generated(new_insts);
        }
    }

    /// Mask data and element segments' offsets to be in bounds of their
    /// associated tables and memories.
    fn no_trapping_segments(&mut self) {
        for data in &mut self.data {
            match &mut data.kind {
                DataSegmentKind::Passive => continue,
                DataSegmentKind::Active {
                    memory_index,
                    offset,
                } => {
                    let mem = &mut self.memories[usize::try_from(*memory_index).unwrap()];

                    // Ensure that all memories have at least one
                    // page. Otherwise, if we had a zero-minimum memory, then we
                    // wouldn't be able to mask the initializers to be
                    // definitely in-bounds.
                    mem.minimum = std::cmp::max(1, mem.minimum);
                    mem.maximum = mem.maximum.map(|n| std::cmp::max(mem.minimum, n));

                    // Make sure that the data segment can fit into the memory.
                    data.init
                        .truncate(usize::try_from(mem.minimum * WASM_PAGE_SIZE).unwrap());
                    let data_len = data.init.len() as u64;

                    match offset {
                        Instruction::I32Const(n) => {
                            let n = *n as u64;
                            let n = n
                                .checked_rem(mem.minimum * WASM_PAGE_SIZE - data_len)
                                .unwrap_or(0);
                            let n = u32::try_from(n).unwrap();
                            *offset = Instruction::I32Const(n as i32);
                        }
                        Instruction::I64Const(n) => {
                            let n = *n as u64;
                            let n = n
                                .checked_rem(mem.minimum * WASM_PAGE_SIZE - data_len)
                                .unwrap_or(0);
                            *offset = Instruction::I64Const(n as i64);
                        }
                        _ => unreachable!(),
                    }
                }
            }
        }

        for elem in &mut self.elems {
            match &mut elem.kind {
                ElementKind::Passive | ElementKind::Declared => continue,
                ElementKind::Active { table, offset } => {
                    let table = table.unwrap_or(0);
                    let table = usize::try_from(table).unwrap();
                    let table = &mut self.tables[table];

                    // Ensure that we have room for at least one element. See
                    // comment above.
                    table.minimum = std::cmp::max(1, table.minimum);
                    table.maximum = table.maximum.map(|n| std::cmp::max(table.minimum, n));

                    // Make sure that the element segment can fit into the
                    // table.
                    let elem_len = match &mut elem.items {
                        Elements::Functions(fs) => {
                            fs.truncate(usize::try_from(table.minimum).unwrap());
                            u32::try_from(fs.len()).unwrap()
                        }
                        Elements::Expressions(es) => {
                            es.truncate(usize::try_from(table.minimum).unwrap());
                            u32::try_from(es.len()).unwrap()
                        }
                    };

                    match offset {
                        Instruction::I32Const(n) => {
                            let n = *n as u32;
                            let n = n.checked_rem(table.minimum - elem_len).unwrap_or(0);
                            *offset = Instruction::I32Const(n as i32);
                        }
                        _ => unreachable!(),
                    }
                }
            }
        }
    }
}

fn dummy_value_inst<'a>(ty: ValType) -> Instruction<'a> {
    match ty {
        ValType::I32 => Instruction::I32Const(0),
        ValType::I64 => Instruction::I64Const(0),
        ValType::F32 => Instruction::F32Const(0.0),
        ValType::F64 => Instruction::F64Const(0.0),
        ValType::V128 => Instruction::V128Const(0),
        ValType::FuncRef | ValType::ExternRef => Instruction::RefNull(ty),
    }
}

fn eqz_inst<'a>(ty: ValType) -> Instruction<'a> {
    match ty {
        ValType::I32 => Instruction::I32Eqz,
        ValType::I64 => Instruction::I64Eqz,
        _ => panic!("not an integer type"),
    }
}

fn type_of_integer_operation(inst: &Instruction) -> ValType {
    match inst {
        Instruction::I32DivU
        | Instruction::I32DivS
        | Instruction::I32RemU
        | Instruction::I32RemS => ValType::I32,
        Instruction::I64RemU
        | Instruction::I64DivU
        | Instruction::I64DivS
        | Instruction::I64RemS => ValType::I64,
        _ => panic!("not integer division or remainder"),
    }
}

fn type_of_memory_access(inst: &Instruction) -> ValType {
    match inst {
        Instruction::I32Load(_)
        | Instruction::I32Load8_S(_)
        | Instruction::I32Load8_U(_)
        | Instruction::I32Load16_S(_)
        | Instruction::I32Load16_U(_) => ValType::I32,

        Instruction::I64Load(_)
        | Instruction::I64Load8_S(_)
        | Instruction::I64Load8_U(_)
        | Instruction::I64Load16_S(_)
        | Instruction::I64Load16_U(_)
        | Instruction::I64Load32_S(_)
        | Instruction::I64Load32_U(_) => ValType::I64,

        Instruction::F32Load(_) => ValType::F32,

        Instruction::F64Load(_) => ValType::F64,

        Instruction::V128Load { .. }
        | Instruction::V128Load8x8S { .. }
        | Instruction::V128Load8x8U { .. }
        | Instruction::V128Load16x4S { .. }
        | Instruction::V128Load16x4U { .. }
        | Instruction::V128Load32x2S { .. }
        | Instruction::V128Load32x2U { .. }
        | Instruction::V128Load8Splat { .. }
        | Instruction::V128Load16Splat { .. }
        | Instruction::V128Load32Splat { .. }
        | Instruction::V128Load64Splat { .. }
        | Instruction::V128Load32Zero { .. }
        | Instruction::V128Load64Zero { .. } => ValType::V128,

        _ => panic!("not a memory access instruction"),
    }
}

fn int_min_const_inst<'a>(ty: ValType) -> Instruction<'a> {
    match ty {
        ValType::I32 => Instruction::I32Const(i32::MIN),
        ValType::I64 => Instruction::I64Const(i64::MIN),
        _ => panic!("not an int type"),
    }
}

fn int_const_inst<'a>(ty: ValType, x: i64) -> Instruction<'a> {
    match ty {
        ValType::I32 => Instruction::I32Const(x as i32),
        ValType::I64 => Instruction::I64Const(x),
        _ => panic!("not an int type"),
    }
}

fn int_mul_inst<'a>(ty: ValType) -> Instruction<'a> {
    match ty {
        ValType::I32 => Instruction::I32Mul,
        ValType::I64 => Instruction::I64Mul,
        _ => panic!("not an int type"),
    }
}

fn int_add_inst<'a>(ty: ValType) -> Instruction<'a> {
    match ty {
        ValType::I32 => Instruction::I32Add,
        ValType::I64 => Instruction::I64Add,
        _ => panic!("not an int type"),
    }
}

fn int_le_u_inst<'a>(ty: ValType) -> Instruction<'a> {
    match ty {
        ValType::I32 => Instruction::I32LeU,
        ValType::I64 => Instruction::I64LeU,
        _ => panic!("not an int type"),
    }
}

fn int_ne_inst<'a>(ty: ValType) -> Instruction<'a> {
    match ty {
        ValType::I32 => Instruction::I32Ne,
        ValType::I64 => Instruction::I64Ne,
        _ => panic!("not an int type"),
    }
}

fn size_of_type_in_memory(ty: ValType) -> u64 {
    match ty {
        ValType::I32 => 4,
        ValType::I64 => 8,
        ValType::F32 => 4,
        ValType::F64 => 8,
        ValType::V128 => 16,
        ValType::FuncRef | ValType::ExternRef => panic!("not a memory type"),
    }
}
