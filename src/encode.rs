use super::*;

impl Module {
    /// Encode this Wasm module into bytes.
    pub fn to_bytes(&self) -> Vec<u8> {
        #[rustfmt::skip]
        let mut bytes = vec![
            // Magic
            0x00, 0x61, 0x73, 0x6D,
            // Version
            0x01, 0x00, 0x00, 0x00,
        ];

        self.encode_types(&mut bytes);
        self.encode_imports(&mut bytes);
        self.encode_funcs(&mut bytes);
        self.encode_tables(&mut bytes);
        self.encode_memories(&mut bytes);
        self.encode_globals(&mut bytes);
        self.encode_exports(&mut bytes);
        self.encode_start(&mut bytes);
        self.encode_elems(&mut bytes);
        self.encode_code(&mut bytes);
        self.encode_data(&mut bytes);

        bytes
    }

    fn length_placeholder(&self, bytes: &mut Vec<u8>) -> usize {
        let idx = bytes.len();
        // The longest a `u32` encoded as a LEB128 can be is 5 bytes.
        bytes.extend(std::iter::repeat(0x80_u8).take(4).chain(Some(0x00)));
        idx
    }

    fn encode_length(&self, bytes: &mut Vec<u8>, at: usize, length: u32) {
        debug_assert!(bytes.len() >= at + 5);
        let mut buf = [0; 5];
        let n = leb128::write::unsigned(&mut &mut buf[..], length as u64).unwrap();
        let start = at;
        let end = start + n;
        bytes[start..end].copy_from_slice(&buf[0..n]);
        for i in 0..4 {
            bytes[start + i] |= 1 << 7;
        }
    }

    fn with_byte_length<T>(&self, bytes: &mut Vec<u8>, f: impl FnOnce(&mut Vec<u8>) -> T) -> T {
        let placeholder = self.length_placeholder(bytes);
        let start = bytes.len();
        let result = f(bytes);
        let end = bytes.len();
        self.encode_length(bytes, placeholder, (end - start) as u32);
        result
    }

    fn section(&self, bytes: &mut Vec<u8>, id: u8, f: impl FnOnce(&mut Vec<u8>)) {
        bytes.push(id);
        self.with_byte_length(bytes, f)
    }

    fn encode_vec<'a, T: 'a>(
        &self,
        bytes: &mut Vec<u8>,
        items: impl IntoIterator<Item = &'a T> + 'a,
        encode: impl Fn(&mut Vec<u8>, &'a T),
    ) {
        let placeholder = self.length_placeholder(bytes);
        let mut len = 0;
        for x in items {
            encode(bytes, x);
            len += 1;
        }
        self.encode_length(bytes, placeholder, len)
    }

    fn encode_result_type(&self, bytes: &mut Vec<u8>, tys: &[ValType]) {
        self.encode_vec(bytes, tys, |bytes, ty| {
            self.encode_val_type(bytes, *ty);
        })
    }

    fn encode_name(&self, bytes: &mut Vec<u8>, name: &str) {
        let placeholder = self.length_placeholder(bytes);
        self.encode_length(bytes, placeholder, name.len() as u32);
        bytes.extend(name.as_bytes());
    }

    fn encode_limits(&self, bytes: &mut Vec<u8>, limits: &Limits) {
        if let Some(max) = limits.max {
            bytes.push(0x01);
            self.encode_u32(bytes, limits.min);
            self.encode_u32(bytes, max);
        } else {
            bytes.push(0x00);
            self.encode_u32(bytes, limits.min);
        }
    }

    fn encode_val_type(&self, bytes: &mut Vec<u8>, ty: ValType) {
        bytes.push(match ty {
            ValType::I32 => 0x7F,
            ValType::I64 => 0x7E,
            ValType::F32 => 0x7D,
            ValType::F64 => 0x7C,
        });
    }

    fn encode_u32(&self, bytes: &mut Vec<u8>, x: u32) {
        let mut buf = [0x00; 5];
        let n = leb128::write::unsigned(&mut &mut buf[..], x as u64).unwrap();
        bytes.extend(buf.iter().take(n).copied());
    }

    fn encode_s32(&self, bytes: &mut Vec<u8>, x: i32) {
        let mut buf = [0x00; 5];
        let n = leb128::write::signed(&mut &mut buf[..], x as i64).unwrap();
        bytes.extend(buf.iter().take(n).copied());
    }

    fn encode_s33(&self, bytes: &mut Vec<u8>, x: i64) {
        let mut buf = [0x00; 5];
        let n = leb128::write::signed(&mut &mut buf[..], x).unwrap();
        bytes.extend(buf.iter().take(n).copied());
    }

    fn encode_s64(&self, bytes: &mut Vec<u8>, x: i64) {
        let mut buf = [0x00; 10];
        let n = leb128::write::signed(&mut &mut buf[..], x).unwrap();
        bytes.extend(buf.iter().take(n).copied());
    }

    fn encode_table_type(&self, bytes: &mut Vec<u8>, ty: &TableType) {
        bytes.push(0x70); // `funcref`
        self.encode_limits(bytes, &ty.limits);
    }

    fn encode_global_type(&self, bytes: &mut Vec<u8>, g: &GlobalType) {
        self.encode_val_type(bytes, g.val_type);
        bytes.push(g.mutable as u8);
    }

    fn encode_block_type(&self, bytes: &mut Vec<u8>, bt: BlockType) {
        match bt {
            BlockType::Empty => bytes.push(0x40),
            BlockType::Result(ty) => self.encode_val_type(bytes, ty),
            BlockType::FuncType(f) => self.encode_s33(bytes, f as i64),
        }
    }

    fn encode_mem_arg(&self, bytes: &mut Vec<u8>, m: MemArg) {
        self.encode_u32(bytes, m.align);
        self.encode_u32(bytes, m.offset);
    }

    fn encode_instruction(&self, bytes: &mut Vec<u8>, inst: &Instruction) {
        match inst {
            // Control instructions.
            Instruction::Unreachable => bytes.push(0x00),
            Instruction::Nop => bytes.push(0x01),
            Instruction::Block(bt) => {
                bytes.push(0x02);
                self.encode_block_type(bytes, *bt);
            }
            Instruction::Loop(bt) => {
                bytes.push(0x03);
                self.encode_block_type(bytes, *bt);
            }
            Instruction::If(bt) => {
                bytes.push(0x04);
                self.encode_block_type(bytes, *bt);
            }
            Instruction::Else => bytes.push(0x05),
            Instruction::End => bytes.push(0x0B),
            Instruction::Br(l) => {
                bytes.push(0x0C);
                self.encode_u32(bytes, *l);
            }
            Instruction::BrIf(l) => {
                bytes.push(0x0D);
                self.encode_u32(bytes, *l);
            }
            Instruction::BrTable(ls, l) => {
                bytes.push(0x0E);
                self.encode_vec(bytes, ls, |bytes, l| {
                    self.encode_u32(bytes, *l);
                });
                self.encode_u32(bytes, *l);
            }
            Instruction::Return => bytes.push(0x0F),
            Instruction::Call(f) => {
                bytes.push(0x10);
                self.encode_u32(bytes, *f);
            }
            Instruction::CallIndirect(ty) => {
                bytes.push(0x11);
                self.encode_u32(bytes, *ty);
                bytes.push(0x00);
            }

            // Parametric instructions.
            Instruction::Drop => bytes.push(0x1A),
            Instruction::Select => bytes.push(0x1B),

            // Variable instructions.
            Instruction::LocalGet(l) => {
                bytes.push(0x20);
                self.encode_u32(bytes, *l);
            }
            Instruction::LocalSet(l) => {
                bytes.push(0x21);
                self.encode_u32(bytes, *l);
            }
            Instruction::LocalTee(l) => {
                bytes.push(0x22);
                self.encode_u32(bytes, *l);
            }
            Instruction::GlobalGet(g) => {
                bytes.push(0x23);
                self.encode_u32(bytes, *g);
            }
            Instruction::GlobalSet(g) => {
                bytes.push(0x24);
                self.encode_u32(bytes, *g);
            }

            // Memory instructions.
            Instruction::I32Load(m) => {
                bytes.push(0x28);
                self.encode_mem_arg(bytes, *m);
            }
            Instruction::I64Load(m) => {
                bytes.push(0x29);
                self.encode_mem_arg(bytes, *m);
            }
            Instruction::F32Load(m) => {
                bytes.push(0x2A);
                self.encode_mem_arg(bytes, *m);
            }
            Instruction::F64Load(m) => {
                bytes.push(0x2B);
                self.encode_mem_arg(bytes, *m);
            }
            Instruction::I32Load8_S(m) => {
                bytes.push(0x2C);
                self.encode_mem_arg(bytes, *m);
            }
            Instruction::I32Load8_U(m) => {
                bytes.push(0x2D);
                self.encode_mem_arg(bytes, *m);
            }
            Instruction::I32Load16_S(m) => {
                bytes.push(0x2E);
                self.encode_mem_arg(bytes, *m);
            }
            Instruction::I32Load16_U(m) => {
                bytes.push(0x2F);
                self.encode_mem_arg(bytes, *m);
            }
            Instruction::I64Load8_S(m) => {
                bytes.push(0x30);
                self.encode_mem_arg(bytes, *m);
            }
            Instruction::I64Load8_U(m) => {
                bytes.push(0x31);
                self.encode_mem_arg(bytes, *m);
            }
            Instruction::I64Load16_S(m) => {
                bytes.push(0x32);
                self.encode_mem_arg(bytes, *m);
            }
            Instruction::I64Load16_U(m) => {
                bytes.push(0x33);
                self.encode_mem_arg(bytes, *m);
            }
            Instruction::I64Load32_S(m) => {
                bytes.push(0x34);
                self.encode_mem_arg(bytes, *m);
            }
            Instruction::I64Load32_U(m) => {
                bytes.push(0x35);
                self.encode_mem_arg(bytes, *m);
            }
            Instruction::I32Store(m) => {
                bytes.push(0x36);
                self.encode_mem_arg(bytes, *m);
            }
            Instruction::I64Store(m) => {
                bytes.push(0x37);
                self.encode_mem_arg(bytes, *m);
            }
            Instruction::F32Store(m) => {
                bytes.push(0x38);
                self.encode_mem_arg(bytes, *m);
            }
            Instruction::F64Store(m) => {
                bytes.push(0x39);
                self.encode_mem_arg(bytes, *m);
            }
            Instruction::I32Store8(m) => {
                bytes.push(0x3A);
                self.encode_mem_arg(bytes, *m);
            }
            Instruction::I32Store16(m) => {
                bytes.push(0x3B);
                self.encode_mem_arg(bytes, *m);
            }
            Instruction::I64Store8(m) => {
                bytes.push(0x3C);
                self.encode_mem_arg(bytes, *m);
            }
            Instruction::I64Store16(m) => {
                bytes.push(0x3D);
                self.encode_mem_arg(bytes, *m);
            }
            Instruction::I64Store32(m) => {
                bytes.push(0x3E);
                self.encode_mem_arg(bytes, *m);
            }
            Instruction::MemorySize => {
                bytes.push(0x3F);
                bytes.push(0x00); // Reserved for memory index.
            }
            Instruction::MemoryGrow => {
                bytes.push(0x40);
                bytes.push(0x00); // Reserved for memory index.
            }

            // Numeric instructions.
            Instruction::I32Const(x) => {
                bytes.push(0x41);
                self.encode_s32(bytes, *x);
            }
            Instruction::I64Const(x) => {
                bytes.push(0x42);
                self.encode_s64(bytes, *x);
            }
            Instruction::F32Const(x) => {
                bytes.push(0x43);
                let x: u32 = unsafe { std::mem::transmute(*x) };
                bytes.extend(x.to_le_bytes().iter().copied());
            }
            Instruction::F64Const(x) => {
                bytes.push(0x44);
                let x: u64 = unsafe { std::mem::transmute(*x) };
                bytes.extend(x.to_le_bytes().iter().copied());
            }
            Instruction::I32Eqz => bytes.push(0x45),
            Instruction::I32Eq => bytes.push(0x46),
            Instruction::I32Neq => bytes.push(0x47),
            Instruction::I32LtS => bytes.push(0x48),
            Instruction::I32LtU => bytes.push(0x49),
            Instruction::I32GtS => bytes.push(0x4A),
            Instruction::I32GtU => bytes.push(0x4B),
            Instruction::I32LeS => bytes.push(0x4C),
            Instruction::I32LeU => bytes.push(0x4D),
            Instruction::I32GeS => bytes.push(0x4E),
            Instruction::I32GeU => bytes.push(0x4F),
            Instruction::I64Eqz => bytes.push(0x50),
            Instruction::I64Eq => bytes.push(0x51),
            Instruction::I64Neq => bytes.push(0x52),
            Instruction::I64LtS => bytes.push(0x53),
            Instruction::I64LtU => bytes.push(0x54),
            Instruction::I64GtS => bytes.push(0x55),
            Instruction::I64GtU => bytes.push(0x56),
            Instruction::I64LeS => bytes.push(0x57),
            Instruction::I64LeU => bytes.push(0x58),
            Instruction::I64GeS => bytes.push(0x59),
            Instruction::I64GeU => bytes.push(0x5A),
            Instruction::F32Eq => bytes.push(0x5B),
            Instruction::F32Neq => bytes.push(0x5C),
            Instruction::F32Lt => bytes.push(0x5D),
            Instruction::F32Gt => bytes.push(0x5E),
            Instruction::F32Le => bytes.push(0x5F),
            Instruction::F32Ge => bytes.push(0x60),
            Instruction::F64Eq => bytes.push(0x61),
            Instruction::F64Neq => bytes.push(0x62),
            Instruction::F64Lt => bytes.push(0x63),
            Instruction::F64Gt => bytes.push(0x64),
            Instruction::F64Le => bytes.push(0x65),
            Instruction::F64Ge => bytes.push(0x66),
            Instruction::I32Clz => bytes.push(0x67),
            Instruction::I32Ctz => bytes.push(0x68),
            Instruction::I32Popcnt => bytes.push(0x69),
            Instruction::I32Add => bytes.push(0x6A),
            Instruction::I32Sub => bytes.push(0x6B),
            Instruction::I32Mul => bytes.push(0x6C),
            Instruction::I32DivS => bytes.push(0x6D),
            Instruction::I32DivU => bytes.push(0x6E),
            Instruction::I32RemS => bytes.push(0x6F),
            Instruction::I32RemU => bytes.push(0x70),
            Instruction::I32And => bytes.push(0x71),
            Instruction::I32Or => bytes.push(0x72),
            Instruction::I32Xor => bytes.push(0x73),
            Instruction::I32Shl => bytes.push(0x74),
            Instruction::I32ShrS => bytes.push(0x75),
            Instruction::I32ShrU => bytes.push(0x76),
            Instruction::I32Rotl => bytes.push(0x77),
            Instruction::I32Rotr => bytes.push(0x78),
            Instruction::I64Clz => bytes.push(0x79),
            Instruction::I64Ctz => bytes.push(0x7A),
            Instruction::I64Popcnt => bytes.push(0x7B),
            Instruction::I64Add => bytes.push(0x7C),
            Instruction::I64Sub => bytes.push(0x7D),
            Instruction::I64Mul => bytes.push(0x7E),
            Instruction::I64DivS => bytes.push(0x7F),
            Instruction::I64DivU => bytes.push(0x80),
            Instruction::I64RemS => bytes.push(0x81),
            Instruction::I64RemU => bytes.push(0x82),
            Instruction::I64And => bytes.push(0x83),
            Instruction::I64Or => bytes.push(0x84),
            Instruction::I64Xor => bytes.push(0x85),
            Instruction::I64Shl => bytes.push(0x86),
            Instruction::I64ShrS => bytes.push(0x87),
            Instruction::I64ShrU => bytes.push(0x88),
            Instruction::I64Rotl => bytes.push(0x89),
            Instruction::I64Rotr => bytes.push(0x8A),
            Instruction::F32Abs => bytes.push(0x8B),
            Instruction::F32Neg => bytes.push(0x8C),
            Instruction::F32Ceil => bytes.push(0x8D),
            Instruction::F32Floor => bytes.push(0x8E),
            Instruction::F32Trunc => bytes.push(0x8F),
            Instruction::F32Nearest => bytes.push(0x90),
            Instruction::F32Sqrt => bytes.push(0x91),
            Instruction::F32Add => bytes.push(0x92),
            Instruction::F32Sub => bytes.push(0x93),
            Instruction::F32Mul => bytes.push(0x94),
            Instruction::F32Div => bytes.push(0x95),
            Instruction::F32Min => bytes.push(0x96),
            Instruction::F32Max => bytes.push(0x97),
            Instruction::F32Copysign => bytes.push(0x98),
            Instruction::F64Abs => bytes.push(0x99),
            Instruction::F64Neg => bytes.push(0x9A),
            Instruction::F64Ceil => bytes.push(0x9B),
            Instruction::F64Floor => bytes.push(0x9C),
            Instruction::F64Trunc => bytes.push(0x9D),
            Instruction::F64Nearest => bytes.push(0x9E),
            Instruction::F64Sqrt => bytes.push(0x9F),
            Instruction::F64Add => bytes.push(0xA0),
            Instruction::F64Sub => bytes.push(0xA1),
            Instruction::F64Mul => bytes.push(0xA2),
            Instruction::F64Div => bytes.push(0xA3),
            Instruction::F64Min => bytes.push(0xA4),
            Instruction::F64Max => bytes.push(0xA5),
            Instruction::F64Copysign => bytes.push(0xA6),
            Instruction::I32WrapI64 => bytes.push(0xA7),
            Instruction::I32TruncF32S => bytes.push(0xA8),
            Instruction::I32TruncF32U => bytes.push(0xA9),
            Instruction::I32TruncF64S => bytes.push(0xAA),
            Instruction::I32TruncF64U => bytes.push(0xAB),
            Instruction::I64ExtendI32S => bytes.push(0xAC),
            Instruction::I64ExtendI32U => bytes.push(0xAD),
            Instruction::I64TruncF32S => bytes.push(0xAE),
            Instruction::I64TruncF32U => bytes.push(0xAF),
            Instruction::I64TruncF64S => bytes.push(0xB0),
            Instruction::I64TruncF64U => bytes.push(0xB1),
            Instruction::F32ConvertI32S => bytes.push(0xB2),
            Instruction::F32ConvertI32U => bytes.push(0xB3),
            Instruction::F32ConvertI64S => bytes.push(0xB4),
            Instruction::F32ConvertI64U => bytes.push(0xB5),
            Instruction::F32DemoteF64 => bytes.push(0xB6),
            Instruction::F64ConvertI32S => bytes.push(0xB7),
            Instruction::F64ConvertI32U => bytes.push(0xB8),
            Instruction::F64ConvertI64S => bytes.push(0xB9),
            Instruction::F64ConvertI64U => bytes.push(0xBA),
            Instruction::F64PromoteF32 => bytes.push(0xBB),
            Instruction::I32ReinterpretF32 => bytes.push(0xBC),
            Instruction::I64ReinterpretF64 => bytes.push(0xBD),
            Instruction::F32ReinterpretI32 => bytes.push(0xBE),
            Instruction::F64ReinterpretI64 => bytes.push(0xBF),
            Instruction::I32Extend8S => bytes.push(0xC0),
            Instruction::I32Extend16S => bytes.push(0xC1),
            Instruction::I64Extend8S => bytes.push(0xC2),
            Instruction::I64Extend16S => bytes.push(0xC3),
            Instruction::I64Extend32S => bytes.push(0xC4),
            Instruction::I32TruncSatF32S => {
                bytes.push(0xFC);
                self.encode_u32(bytes, 0);
            }
            Instruction::I32TruncSatF32U => {
                bytes.push(0xFC);
                self.encode_u32(bytes, 1);
            }
            Instruction::I32TruncSatF64S => {
                bytes.push(0xFC);
                self.encode_u32(bytes, 2);
            }
            Instruction::I32TruncSatF64U => {
                bytes.push(0xFC);
                self.encode_u32(bytes, 3);
            }
            Instruction::I64TruncSatF32S => {
                bytes.push(0xFC);
                self.encode_u32(bytes, 4);
            }
            Instruction::I64TruncSatF32U => {
                bytes.push(0xFC);
                self.encode_u32(bytes, 5);
            }
            Instruction::I64TruncSatF64S => {
                bytes.push(0xFC);
                self.encode_u32(bytes, 6);
            }
            Instruction::I64TruncSatF64U => {
                bytes.push(0xFC);
                self.encode_u32(bytes, 7);
            }
        }
    }

    fn encode_types(&self, bytes: &mut Vec<u8>) {
        if self.types.is_empty() {
            return;
        }
        self.section(bytes, 1, |bytes| {
            self.encode_vec(bytes, &self.types, |bytes, ty| {
                bytes.push(0x60);
                self.encode_result_type(bytes, &ty.params);
                self.encode_result_type(bytes, &ty.results);
            });
        });
    }

    fn encode_imports(&self, bytes: &mut Vec<u8>) {
        if self.imports.is_empty() {
            return;
        }
        self.section(bytes, 2, |bytes| {
            self.encode_vec(bytes, &self.imports, |bytes, (module, name, imp)| {
                self.encode_name(bytes, module);
                self.encode_name(bytes, name);
                match imp {
                    Import::Func(f) => {
                        bytes.push(0x00);
                        self.encode_u32(bytes, *f);
                    }
                    Import::Table(ty) => {
                        bytes.push(0x01);
                        self.encode_table_type(bytes, ty);
                    }
                    Import::Memory(m) => {
                        bytes.push(0x02);
                        self.encode_limits(bytes, &m.limits);
                    }
                    Import::Global(g) => {
                        bytes.push(0x03);
                        self.encode_global_type(bytes, g);
                    }
                }
            });
        });
    }

    fn encode_funcs(&self, bytes: &mut Vec<u8>) {
        if self.funcs.is_empty() {
            return;
        }
        self.section(bytes, 3, |bytes| {
            self.encode_vec(bytes, &self.funcs, |bytes, ty| {
                self.encode_u32(bytes, *ty);
            });
        });
    }

    fn encode_tables(&self, bytes: &mut Vec<u8>) {
        if let Some(t) = self.table.as_ref() {
            self.section(bytes, 4, |bytes| {
                self.encode_vec(bytes, Some(t), |bytes, t| {
                    self.encode_table_type(bytes, t);
                });
            });
        }
    }

    fn encode_memories(&self, bytes: &mut Vec<u8>) {
        if let Some(m) = self.memory.as_ref() {
            self.section(bytes, 5, |bytes| {
                self.encode_vec(bytes, Some(m), |bytes, m| {
                    self.encode_limits(bytes, &m.limits);
                });
            });
        }
    }

    fn encode_globals(&self, bytes: &mut Vec<u8>) {
        if self.globals.is_empty() {
            return;
        }
        self.section(bytes, 6, |bytes| {
            self.encode_vec(bytes, &self.globals, |bytes, g| {
                self.encode_global_type(bytes, &g.ty);
                self.encode_instruction(bytes, &g.expr);
                self.encode_instruction(bytes, &Instruction::End);
            });
        });
    }

    fn encode_exports(&self, bytes: &mut Vec<u8>) {
        if self.exports.is_empty() {
            return;
        }
        self.section(bytes, 7, |bytes| {
            self.encode_vec(bytes, &self.exports, |bytes, (name, exp)| {
                self.encode_name(bytes, name);
                match exp {
                    Export::Func(f) => {
                        bytes.push(0x00);
                        self.encode_u32(bytes, *f);
                    }
                    Export::Table(t) => {
                        bytes.push(0x01);
                        self.encode_u32(bytes, *t);
                    }
                    Export::Memory(m) => {
                        bytes.push(0x02);
                        self.encode_u32(bytes, *m);
                    }
                    Export::Global(g) => {
                        bytes.push(0x03);
                        self.encode_u32(bytes, *g);
                    }
                }
            });
        });
    }

    fn encode_start(&self, bytes: &mut Vec<u8>) {
        if let Some(f) = self.start {
            self.section(bytes, 8, |bytes| {
                self.encode_u32(bytes, f);
            });
        }
    }

    fn encode_elems(&self, bytes: &mut Vec<u8>) {
        if self.elems.is_empty() {
            return;
        }
        self.section(bytes, 9, |bytes| {
            self.encode_vec(bytes, &self.elems, |bytes, el| {
                bytes.push(0x00); // Table index.
                self.encode_instruction(bytes, &el.offset);
                self.encode_instruction(bytes, &Instruction::End);
                self.encode_vec(bytes, &el.init, |bytes, f| {
                    self.encode_u32(bytes, *f);
                });
            });
        });
    }

    fn encode_code(&self, bytes: &mut Vec<u8>) {
        if self.code.is_empty() {
            return;
        }
        self.section(bytes, 10, |bytes| {
            self.encode_vec(bytes, &self.code, |bytes, code| {
                self.with_byte_length(bytes, |bytes| {
                    // Skip the run-length encoding because it is a little
                    // annoying to compute; use a length of one for every local.
                    self.encode_vec(bytes, &code.locals, |bytes, l| {
                        bytes.push(0x01);
                        self.encode_val_type(bytes, *l);
                    });

                    for inst in &code.instructions {
                        self.encode_instruction(bytes, inst);
                    }
                    self.encode_instruction(bytes, &Instruction::End);
                });
            });
        });
    }

    fn encode_data(&self, bytes: &mut Vec<u8>) {
        self.section(bytes, 11, |bytes| {
            self.encode_vec(bytes, &self.data, |bytes, data| {
                self.encode_u32(bytes, 0); // Memory index.
                self.encode_instruction(bytes, &data.offset);
                self.encode_instruction(bytes, &Instruction::End);
                self.encode_vec(bytes, &data.init, |bytes, b| {
                    bytes.push(*b);
                });
            });
        });
    }
}
