use crate::ast::{self, kw};
use crate::parser::{Parse, Parser, Result};

#[derive(Debug, PartialEq)]
pub struct Expression<'a> {
    pub instrs: Vec<Instruction<'a>>,
}

impl<'a> Parse<'a> for Expression<'a> {
    fn parse(parser: Parser<'a>) -> Result<Self> {
        let mut instrs = Vec::new();
        parse_folded_instrs(parser, &mut instrs, false)?;
        Ok(Expression { instrs })
    }
}

fn parse_folded_instrs<'a>(
    parser: Parser<'a>,
    instrs: &mut Vec<Instruction<'a>>,
    stop_on_then: bool,
) -> Result<()> {
    while !parser.is_empty() {
        if stop_on_then && parser.peek2::<kw::then>() {
            break;
        }

        if parser.peek::<ast::LParen>() {
            parser.parens(|parser| {
                match parser.parse()? {
                    i @ Instruction::Block(_) | i @ Instruction::Loop(_) => {
                        instrs.push(i);
                        parse_folded_instrs(parser, instrs, false)?;
                        instrs.push(Instruction::End(None));
                    }
                    i @ Instruction::If(_) => {
                        parse_folded_instrs(parser, instrs, true)?;
                        instrs.push(i);
                        parser.parens(|parser| {
                            parser.parse::<kw::then>()?;
                            parse_folded_instrs(parser, instrs, false)
                        })?;
                        if parser.peek2::<kw::r#else>() {
                            instrs.push(Instruction::Else(None));
                            parser.parens(|parser| {
                                parser.parse::<kw::r#else>()?;
                                parse_folded_instrs(parser, instrs, false)
                            })?;
                        }
                        instrs.push(Instruction::End(None));
                    }
                    other => {
                        parse_folded_instrs(parser, instrs, false)?;
                        instrs.push(other);
                    }
                }
                Ok(())
            })?;
            continue;
        }

        let instr = parser.parse::<Instruction>()?;
        instrs.push(instr);
    }
    Ok(())
}

// TODO: document this obscenity
macro_rules! instructions {
    (pub enum Instruction<'a> {
        $(
            $name:ident $(($($arg:tt)*))? : [$($binary:tt)*] : $instr:tt $( | $deprecated:tt )?,
        )*
    }) => (
        #[derive(Debug, PartialEq)]
        pub enum Instruction<'a> {
            $(
                $name $(( instructions!(@ty $($arg)*) ))?,
            )*

        }

        #[allow(non_snake_case)]
        impl<'a> Parse<'a> for Instruction<'a> {
            fn parse(parser: Parser<'a>) -> Result<Self> {
                $(
                    fn $name<'a>(_parser: Parser<'a>) -> Result<Instruction<'a>> {
                        Ok(Instruction::$name $((
                            instructions!(@parse _parser $($arg)*)?
                        ))?)
                    }
                )*
                let parse_remainder = parser.step(|c| {
                    let (kw, rest) = match c.keyword() {
                        Some(pair) => pair,
                        None => return Err(c.error("expected an instruction")),
                    };
                    match kw {
                        $($instr $( | $deprecated )?=> Ok(($name as fn(_) -> _, rest)),)*
                        _ => return Err(c.error("unknown instruction")),
                    }
                })?;
                parse_remainder(parser)
            }
        }

        impl crate::binary::Encode for Instruction<'_> {
            #[allow(non_snake_case)]
            fn encode(&self, v: &mut Vec<u8>) {
                match self {
                    $(
                        Instruction::$name $((instructions!(@first $($arg)*)))? => {
                            fn encode<'a>($(arg: &instructions!(@ty $($arg)*),)? v: &mut Vec<u8>) {
                                v.extend_from_slice(&[$($binary)*]);
                                $(<instructions!(@ty $($arg)*) as crate::binary::Encode>::encode(arg, v);)?
                            }
                            encode($( instructions!(@first $($arg)*), )? v)
                        }
                    )*
                }
            }
        }
    );

    (@ty MemArg<$amt:tt>) => (MemArg);
    (@ty $other:ty) => ($other);

    (@first $first:ident $($t:tt)*) => ($first);

    (@parse $parser:ident MemArg<$amt:tt>) => (MemArg::parse($parser, $amt));
    (@parse $parser:ident MemArg) => (compile_error!("must specify `MemArg` default"));
    (@parse $parser:ident $other:ty) => ($parser.parse::<$other>());
}

instructions! {
    pub enum Instruction<'a> {
        Block(BlockType<'a>) : [0x02] : "block",
        If(BlockType<'a>) : [0x04] : "if",
        Else(Option<ast::Id<'a>>) : [0x05] : "else",
        Loop(BlockType<'a>) : [0x03] : "loop",
        End(Option<ast::Id<'a>>) : [0x0b] : "end",

        Unreachable : [0x00] : "unreachable",
        Nop : [0x01] : "nop",
        Br(ast::Index<'a>) : [0x0c] : "br",
        BrIf(ast::Index<'a>) : [0x0d] : "br_if",
        BrTable(BrTableIndices<'a>) : [0x0e] : "br_table",
        Return : [0x0f] : "return",
        Call(ast::Index<'a>) : [0x10] : "call",
        CallIndirect(CallIndirect<'a>) : [0x11] : "call_indirect",
        Drop : [0x1a] : "drop",
        Select : [0x1b] : "select",
        LocalGet(ast::Index<'a>) : [0x20] : "local.get" | "get_local",
        LocalSet(ast::Index<'a>) : [0x21] : "local.set" | "set_local",
        LocalTee(ast::Index<'a>) : [0x22] : "local.tee" | "tee_local",
        GlobalGet(ast::Index<'a>) : [0x23] : "global.get" | "get_global",
        GlobalSet(ast::Index<'a>) : [0x24] : "global.set" | "set_global",

        TableGet(ast::Index<'a>) : [0x25] : "table.get",
        TableSet(ast::Index<'a>) : [0x26] : "table.set",

        I32Load(MemArg<4>) : [0x28] : "i32.load",
        I64Load(MemArg<8>) : [0x29] : "i64.load",
        F32Load(MemArg<4>) : [0x2a] : "f32.load",
        F64Load(MemArg<8>) : [0x2b] : "f64.load",
        I32Load8s(MemArg<1>) : [0x2c] : "i32.load8_s",
        I32Load8u(MemArg<1>) : [0x2d] : "i32.load8_u",
        I32Load16s(MemArg<2>) : [0x2e] : "i32.load16_s",
        I32Load16u(MemArg<2>) : [0x2f] : "i32.load16_u",
        I64Load8s(MemArg<1>) : [0x30] : "i64.load8_s",
        I64Load8u(MemArg<1>) : [0x31] : "i64.load8_u",
        I64Load16s(MemArg<2>) : [0x32] : "i64.load16_s",
        I64Load16u(MemArg<2>) : [0x33] : "i64.load16_u",
        I64Load32s(MemArg<4>) : [0x34] : "i64.load32_s",
        I64Load32u(MemArg<4>) : [0x35] : "i64.load32_u",
        I32Store(MemArg<4>) : [0x36] : "i32.store",
        I64Store(MemArg<8>) : [0x37] : "i64.store",
        F32Store(MemArg<4>) : [0x38] : "f32.store",
        F64Store(MemArg<8>) : [0x39] : "f64.store",
        I32Store8(MemArg<1>) : [0x3a] : "i32.store8",
        I32Store16(MemArg<2>) : [0x3b] : "i32.store16",
        I64Store8(MemArg<1>) : [0x3c] : "i64.store8",
        I64Store16(MemArg<2>) : [0x3d] : "i64.store16",
        I64Store32(MemArg<4>) : [0x3e] : "i64.store32",

        // Lots of bulk memory proposal here as well
        MemorySize : [0x3f, 0x00] : "memory.size" | "current_memory",
        MemoryGrow : [0x40, 0x00] : "memory.grow" | "grow_memory",
        MemoryInit(ast::Index<'a>) : [0xfc, 0x08, 0x00] : "memory.init",
        MemoryCopy : [0xfc, 0x0a, 0x00, 0x00] : "memory.copy",
        MemoryFill : [0xfc, 0x0b, 0x00] : "memory.fill",
        DataDrop(ast::Index<'a>) : [0xfc, 0x09] : "data.drop",
        ElemDrop(ast::Index<'a>) : [0xfc, 0x0d] : "elem.drop",
        TableInit(ast::Index<'a>) : [0xfc, 0x0c, 0x00] : "table.init",
        TableCopy : [0xfc, 0x0e, 0x00, 0x00] : "table.copy",
        TableFill(ast::Index<'a>) : [0xfc, 0x11] : "table.fill",
        TableSize(ast::Index<'a>) : [0xfc, 0x10] : "table.size",
        TableGrow(ast::Index<'a>) : [0xfc, 0x0f] : "table.grow",

        RefNull : [0xd0] : "ref.null",
        RefIsNull : [0xd1] : "ref.is_null",
        RefHost(u32) : [0xff] : "ref.host", // only used in test harness
        RefFunc(ast::Index<'a>) : [0xd2] : "ref.func", // only used in test harness

        I32Const(i32) : [0x41] : "i32.const",
        I64Const(i64) : [0x42] : "i64.const",
        F32Const(ast::Float32) : [0x43] : "f32.const",
        F64Const(ast::Float64) : [0x44] : "f64.const",

        I32Clz : [0x67] : "i32.clz",
        I32Ctz : [0x68] : "i32.ctz",
        I32Popcnt : [0x69] : "i32.popcnt",
        I32Add : [0x6a] : "i32.add",
        I32Sub : [0x6b] : "i32.sub",
        I32Mul : [0x6c] : "i32.mul",
        I32DivS : [0x6d] : "i32.div_s",
        I32DivU : [0x6e] : "i32.div_u",
        I32RemS : [0x6f] : "i32.rem_s",
        I32RemU : [0x70] : "i32.rem_u",
        I32And : [0x71] : "i32.and",
        I32Or : [0x72] : "i32.or",
        I32Xor : [0x73] : "i32.xor",
        I32Shl : [0x74] : "i32.shl",
        I32ShrS : [0x75] : "i32.shr_s",
        I32ShrU : [0x76] : "i32.shr_u",
        I32Rotl : [0x77] : "i32.rotl",
        I32Rotr : [0x78] : "i32.rotr",

        I64Clz : [0x79] : "i64.clz",
        I64Ctz : [0x7a] : "i64.ctz",
        I64Popcnt : [0x7b] : "i64.popcnt",
        I64Add : [0x7c] : "i64.add",
        I64Sub : [0x7d] : "i64.sub",
        I64Mul : [0x7e] : "i64.mul",
        I64DivS : [0x7f] : "i64.div_s",
        I64DivU : [0x80] : "i64.div_u",
        I64RemS : [0x81] : "i64.rem_s",
        I64RemU : [0x82] : "i64.rem_u",
        I64And : [0x83] : "i64.and",
        I64Or : [0x84] : "i64.or",
        I64Xor : [0x85] : "i64.xor",
        I64Shl : [0x86] : "i64.shl",
        I64ShrS : [0x87] : "i64.shr_s",
        I64ShrU : [0x88] : "i64.shr_u",
        I64Rotl : [0x89] : "i64.rotl",
        I64Rotr : [0x8a] : "i64.rotr",

        F32Abs : [0x8b] : "f32.abs",
        F32Neg : [0x8c] : "f32.neg",
        F32Ceil : [0x8d] : "f32.ceil",
        F32Floor : [0x8e] : "f32.floor",
        F32Trunc : [0x8f] : "f32.trunc",
        F32Nearest : [0x90] : "f32.nearest",
        F32Sqrt : [0x91] : "f32.sqrt",
        F32Add : [0x92] : "f32.add",
        F32Sub : [0x93] : "f32.sub",
        F32Mul : [0x94] : "f32.mul",
        F32Div : [0x95] : "f32.div",
        F32Min : [0x96] : "f32.min",
        F32Max : [0x97] : "f32.max",
        F32Copysign : [0x98] : "f32.copysign",

        F64Abs : [0x99] : "f64.abs",
        F64Neg : [0x9a] : "f64.neg",
        F64Ceil : [0x9b] : "f64.ceil",
        F64Floor : [0x9c] : "f64.floor",
        F64Trunc : [0x9d] : "f64.trunc",
        F64Nearest : [0x9e] : "f64.nearest",
        F64Sqrt : [0x9f] : "f64.sqrt",
        F64Add : [0xa0] : "f64.add",
        F64Sub : [0xa1] : "f64.sub",
        F64Mul : [0xa2] : "f64.mul",
        F64Div : [0xa3] : "f64.div",
        F64Min : [0xa4] : "f64.min",
        F64Max : [0xa5] : "f64.max",
        F64Copysign : [0xa6] : "f64.copysign",

        I32Eqz : [0x45] : "i32.eqz",
        I32Eq : [0x46] : "i32.eq",
        I32Ne : [0x47] : "i32.ne",
        I32LtS : [0x48] : "i32.lt_s",
        I32LtU : [0x49] : "i32.lt_u",
        I32GtS : [0x4a] : "i32.gt_s",
        I32GtU : [0x4b] : "i32.gt_u",
        I32LeS : [0x4c] : "i32.le_s",
        I32LeU : [0x4d] : "i32.le_u",
        I32GeS : [0x4e] : "i32.ge_s",
        I32GeU : [0x4f] : "i32.ge_u",

        I64Eqz : [0x50] : "i64.eqz",
        I64Eq : [0x51] : "i64.eq",
        I64Ne : [0x52] : "i64.ne",
        I64LtS : [0x53] : "i64.lt_s",
        I64LtU : [0x54] : "i64.lt_u",
        I64GtS : [0x55] : "i64.gt_s",
        I64GtU : [0x56] : "i64.gt_u",
        I64LeS : [0x57] : "i64.le_s",
        I64LeU : [0x58] : "i64.le_u",
        I64GeS : [0x59] : "i64.ge_s",
        I64GeU : [0x5a] : "i64.ge_u",

        F32Eq : [0x5b] : "f32.eq",
        F32Ne : [0x5c] : "f32.ne",
        F32Lt : [0x5d] : "f32.lt",
        F32Gt : [0x5e] : "f32.gt",
        F32Le : [0x5f] : "f32.le",
        F32Ge : [0x60] : "f32.ge",

        F64Eq : [0x61] : "f64.eq",
        F64Ne : [0x62] : "f64.ne",
        F64Lt : [0x63] : "f64.lt",
        F64Gt : [0x64] : "f64.gt",
        F64Le : [0x65] : "f64.le",
        F64Ge : [0x66] : "f64.ge",

        I32WrapI64 : [0xa7] : "i32.wrap_i64" | "i32.wrap/i64",
        I32TruncF32S : [0xa8] : "i32.trunc_f32_s" | "i32.trunc_s/f32",
        I32TruncF32U : [0xa9] : "i32.trunc_f32_u" | "i32.trunc_u/f32",
        I32TruncF64S : [0xaa] : "i32.trunc_f64_s" | "i32.trunc_s/f64",
        I32TruncF64U : [0xab] : "i32.trunc_f64_u" | "i32.trunc_u/f64",
        I64ExtendI32S : [0xac] : "i64.extend_i32_s" | "i64.extend_s/i32",
        I64ExtendI32U : [0xad] : "i64.extend_i32_u" | "i64.extend_u/i32",
        I64TruncF32S : [0xae] : "i64.trunc_f32_s" | "i64.trunc_s/f32",
        I64TruncF32U : [0xaf] : "i64.trunc_f32_u" | "i64.trunc_u/f32",
        I64TruncF64S : [0xb0] : "i64.trunc_f64_s" | "i64.trunc_s/f64",
        I64TruncF64U : [0xb1] : "i64.trunc_f64_u" | "i64.trunc_u/f64",
        F32ConvertI32S : [0xb2] : "f32.convert_i32_s" | "f32.convert_s/i32",
        F32ConvertI32U : [0xb3] : "f32.convert_i32_u" | "f32.convert_u/i32",
        F32ConvertI64S : [0xb4] : "f32.convert_i64_s" | "f32.convert_s/i64",
        F32ConvertI64U : [0xb5] : "f32.convert_i64_u" | "f32.convert_u/i64",
        F32DemoteF64 : [0xb6] : "f32.demote_f64" | "f32.demote/f64",
        F64ConvertI32S : [0xb7] : "f64.convert_i32_s" | "f64.convert_s/i32",
        F64ConvertI32U : [0xb8] : "f64.convert_i32_u" | "f64.convert_u/i32",
        F64ConvertI64S : [0xb9] : "f64.convert_i64_s" | "f64.convert_s/i64",
        F64ConvertI64U : [0xba] : "f64.convert_i64_u" | "f64.convert_u/i64",
        F64PromoteF32 : [0xbb] : "f64.promote_f32" | "f64.promote/f32",
        I32ReinterpretF32 : [0xbc] : "i32.reinterpret_f32" | "i32.reinterpret/f32",
        I64ReinterpretF64 : [0xbd] : "i64.reinterpret_f64" | "i64.reinterpret/f64",
        F32ReinterpretI32 : [0xbe] : "f32.reinterpret_i32" | "f32.reinterpret/i32",
        F64ReinterpretI64 : [0xbf] : "f64.reinterpret_i64" | "f64.reinterpret/i64",

        // non-trapping float to int
        I32TruncSatF32S : [0x00] : "i32.trunc_sat_f32_s" | "i32.trunc_s:sat/f32",
        I32TruncSatF32U : [0x00] : "i32.trunc_sat_f32_u" | "i32.trunc_u:sat/f32",
        I32TruncSatF64S : [0x00] : "i32.trunc_sat_f64_s" | "i32.trunc_s:sat/f64",
        I32TruncSatF64U : [0x00] : "i32.trunc_sat_f64_u" | "i32.trunc_u:sat/f64",
        I64TruncSatF32S : [0x00] : "i64.trunc_sat_f32_s" | "i64.trunc_s:sat/f32",
        I64TruncSatF32U : [0x00] : "i64.trunc_sat_f32_u" | "i64.trunc_u:sat/f32",
        I64TruncSatF64S : [0x00] : "i64.trunc_sat_f64_s" | "i64.trunc_s:sat/f64",
        I64TruncSatF64U : [0x00] : "i64.trunc_sat_f64_u" | "i64.trunc_u:sat/f64",

        // sign extension proposal
        I32Extend8S : [0x00] : "i32.extend8_s",
        I32Extend16S : [0x00] : "i32.extend16_s",
        I64Extend8S : [0x00] : "i64.extend8_s",
        I64Extend16S : [0x00] : "i64.extend16_s",
        I64Extend32S : [0x00] : "i64.extend32_s",

        // atomics proposal
        AtomicNotify(MemArg<1>) : [0x00] : "atomic.notify",
        I32AtomicWait(MemArg<4>) : [0x00] : "i32.atomic.wait",
        I64AtomicWait(MemArg<8>) : [0x00] : "i64.atomic.wait",
        AtomicFence : [0x00] : "atomic.fence",

        I32AtomicLoad(MemArg<4>) : [0x00] : "i32.atomic.load",
        I64AtomicLoad(MemArg<8>) : [0x00] : "i64.atomic.load",
        I32AtomicLoad8u(MemArg<1>) : [0x00] : "i32.atomic.load8_u",
        I32AtomicLoad16u(MemArg<2>) : [0x00] : "i32.atomic.load16_u",
        I64AtomicLoad8u(MemArg<1>) : [0x00] : "i64.atomic.load8_u",
        I64AtomicLoad16u(MemArg<2>) : [0x00] : "i64.atomic.load16_u",
        I64AtomicLoad32u(MemArg<4>) : [0x00] : "i64.atomic.load32_u",
        I32AtomicStore(MemArg<4>) : [0x00] : "i32.atomic.store",
        I64AtomicStore(MemArg<8>) : [0x00] : "i64.atomic.store",
        I32AtomicStore8(MemArg<1>) : [0x00] : "i32.atomic.store8",
        I32AtomicStore16(MemArg<2>) : [0x00] : "i32.atomic.store16",
        I64AtomicStore8(MemArg<1>) : [0x00] : "i64.atomic.store8",
        I64AtomicStore16(MemArg<2>) : [0x00] : "i64.atomic.store16",
        I64AtomicStore32(MemArg<4>) : [0x00] : "i64.atomic.store32",

        I32AtomicRmwAdd(MemArg<4>) : [0x00] : "i32.atomic.rmw.add",
        I64AtomicRmwAdd(MemArg<8>) : [0x00] : "i64.atomic.rmw.add",
        I32AtomicRmw8AddU(MemArg<1>) : [0x00] : "i32.atomic.rmw8.add_u",
        I32AtomicRmw16AddU(MemArg<2>) : [0x00] : "i32.atomic.rmw16.add_u",
        I64AtomicRmw8AddU(MemArg<1>) : [0x00] : "i64.atomic.rmw8.add_u",
        I64AtomicRmw16AddU(MemArg<2>) : [0x00] : "i64.atomic.rmw16.add_u",
        I64AtomicRmw32AddU(MemArg<4>) : [0x00] : "i64.atomic.rmw32.add_u",

        I32AtomicRmwSub(MemArg<4>) : [0x00] : "i32.atomic.rmw.sub",
        I64AtomicRmwSub(MemArg<8>) : [0x00] : "i64.atomic.rmw.sub",
        I32AtomicRmw8SubU(MemArg<1>) : [0x00] : "i32.atomic.rmw8.sub_u",
        I32AtomicRmw16SubU(MemArg<2>) : [0x00] : "i32.atomic.rmw16.sub_u",
        I64AtomicRmw8SubU(MemArg<1>) : [0x00] : "i64.atomic.rmw8.sub_u",
        I64AtomicRmw16SubU(MemArg<2>) : [0x00] : "i64.atomic.rmw16.sub_u",
        I64AtomicRmw32SubU(MemArg<4>) : [0x00] : "i64.atomic.rmw32.sub_u",

        I32AtomicRmwAnd(MemArg<4>) : [0x00] : "i32.atomic.rmw.and",
        I64AtomicRmwAnd(MemArg<8>) : [0x00] : "i64.atomic.rmw.and",
        I32AtomicRmw8AndU(MemArg<1>) : [0x00] : "i32.atomic.rmw8.and_u",
        I32AtomicRmw16AndU(MemArg<2>) : [0x00] : "i32.atomic.rmw16.and_u",
        I64AtomicRmw8AndU(MemArg<1>) : [0x00] : "i64.atomic.rmw8.and_u",
        I64AtomicRmw16AndU(MemArg<2>) : [0x00] : "i64.atomic.rmw16.and_u",
        I64AtomicRmw32AndU(MemArg<4>) : [0x00] : "i64.atomic.rmw32.and_u",

        I32AtomicRmwOr(MemArg<4>) : [0x00] : "i32.atomic.rmw.or",
        I64AtomicRmwOr(MemArg<8>) : [0x00] : "i64.atomic.rmw.or",
        I32AtomicRmw8OrU(MemArg<1>) : [0x00] : "i32.atomic.rmw8.or_u",
        I32AtomicRmw16OrU(MemArg<2>) : [0x00] : "i32.atomic.rmw16.or_u",
        I64AtomicRmw8OrU(MemArg<1>) : [0x00] : "i64.atomic.rmw8.or_u",
        I64AtomicRmw16OrU(MemArg<2>) : [0x00] : "i64.atomic.rmw16.or_u",
        I64AtomicRmw32OrU(MemArg<4>) : [0x00] : "i64.atomic.rmw32.or_u",

        I32AtomicRmwXor(MemArg<4>) : [0x00] : "i32.atomic.rmw.xor",
        I64AtomicRmwXor(MemArg<8>) : [0x00] : "i64.atomic.rmw.xor",
        I32AtomicRmw8XorU(MemArg<1>) : [0x00] : "i32.atomic.rmw8.xor_u",
        I32AtomicRmw16XorU(MemArg<2>) : [0x00] : "i32.atomic.rmw16.xor_u",
        I64AtomicRmw8XorU(MemArg<1>) : [0x00] : "i64.atomic.rmw8.xor_u",
        I64AtomicRmw16XorU(MemArg<2>) : [0x00] : "i64.atomic.rmw16.xor_u",
        I64AtomicRmw32XorU(MemArg<4>) : [0x00] : "i64.atomic.rmw32.xor_u",

        I32AtomicRmwXchg(MemArg<4>) : [0x00] : "i32.atomic.rmw.xchg",
        I64AtomicRmwXchg(MemArg<8>) : [0x00] : "i64.atomic.rmw.xchg",
        I32AtomicRmw8XchgU(MemArg<1>) : [0x00] : "i32.atomic.rmw8.xchg_u",
        I32AtomicRmw16XchgU(MemArg<2>) : [0x00] : "i32.atomic.rmw16.xchg_u",
        I64AtomicRmw8XchgU(MemArg<1>) : [0x00] : "i64.atomic.rmw8.xchg_u",
        I64AtomicRmw16XchgU(MemArg<2>) : [0x00] : "i64.atomic.rmw16.xchg_u",
        I64AtomicRmw32XchgU(MemArg<4>) : [0x00] : "i64.atomic.rmw32.xchg_u",

        I32AtomicRmwCmpxchg(MemArg<4>) : [0x00] : "i32.atomic.rmw.cmpxchg",
        I64AtomicRmwCmpxchg(MemArg<8>) : [0x00] : "i64.atomic.rmw.cmpxchg",
        I32AtomicRmw8CmpxchgU(MemArg<1>) : [0x00] : "i32.atomic.rmw8.cmpxchg_u",
        I32AtomicRmw16CmpxchgU(MemArg<2>) : [0x00] : "i32.atomic.rmw16.cmpxchg_u",
        I64AtomicRmw8CmpxchgU(MemArg<1>) : [0x00] : "i64.atomic.rmw8.cmpxchg_u",
        I64AtomicRmw16CmpxchgU(MemArg<2>) : [0x00] : "i64.atomic.rmw16.cmpxchg_u",
        I64AtomicRmw32CmpxchgU(MemArg<4>) : [0x00] : "i64.atomic.rmw32.cmpxchg_u",
    }
}

#[derive(Debug, PartialEq)]
pub struct BlockType<'a> {
    pub label: Option<ast::Id<'a>>,
    pub ty: ast::TypeUse<'a>,
}

impl<'a> Parse<'a> for BlockType<'a> {
    fn parse(parser: Parser<'a>) -> Result<Self> {
        Ok(BlockType {
            label: parser.parse()?,
            ty: parser.parse()?,
        })
    }
}

#[derive(Debug, PartialEq)]
pub struct BrTableIndices<'a> {
    pub labels: Vec<ast::Index<'a>>,
    pub default: ast::Index<'a>,
}

impl<'a> Parse<'a> for BrTableIndices<'a> {
    fn parse(parser: Parser<'a>) -> Result<Self> {
        let mut labels = Vec::new();
        labels.push(parser.parse()?);
        while parser.peek::<ast::Index>() {
            labels.push(parser.parse()?);
        }
        let default = labels.pop().unwrap();
        Ok(BrTableIndices { labels, default })
    }
}

#[derive(Debug, PartialEq)]
pub struct MemArg {
    pub align: u32,
    pub offset: u32,
}

impl MemArg {
    fn parse(parser: Parser<'_>, default_align: u32) -> Result<Self> {
        fn parse_field(name: &str, parser: Parser<'_>) -> Result<Option<u32>> {
            parser.step(|c| {
                let (kw, rest) = match c.keyword() {
                    Some(p) => p,
                    None => return Ok((None, c)),
                };
                if !kw.starts_with(name) {
                    return Ok((None, c));
                }
                let kw = &kw[name.len()..];
                if !kw.starts_with("=") {
                    return Ok((None, c));
                }
                let num = &kw[1..];
                let num = if num.starts_with("0x") {
                    match u32::from_str_radix(&num[2..], 16) {
                        Ok(n) => n,
                        Err(_) => return Err(c.error("invalid number")),
                    }
                } else {
                    match num.parse() {
                        Ok(n) => n,
                        Err(_) => return Err(c.error("invalid number")),
                    }
                };

                Ok((Some(num), rest))
            })
        }

        Ok(MemArg {
            offset: parse_field("offset", parser)?.unwrap_or(0),
            align: parse_field("align", parser)?.unwrap_or(default_align),
        })
    }
}

#[derive(Debug, PartialEq)]
pub struct CallIndirect<'a> {
    pub table: Option<ast::Index<'a>>,
    pub ty: ast::TypeUse<'a>,
}

impl<'a> Parse<'a> for CallIndirect<'a> {
    fn parse(parser: Parser<'a>) -> Result<Self> {
        let mut table: Option<_> = parser.parse()?;
        let ty = parser.parse()?;
        // Turns out the official test suite at this time thinks table
        // identifiers comes first but wabt's test suites asserts differently
        // putting them second. Let's just handle both.
        if table.is_none() {
            table = parser.parse()?;
        }
        Ok(CallIndirect { table, ty })
    }
}
