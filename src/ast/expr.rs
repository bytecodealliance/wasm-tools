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

macro_rules! instructions {
    (pub enum Instruction<'a> {
        $(
            $name:ident $(($arg:ty))? = $instr:tt $( | $deprecated:tt )?,
        )*
    }) => (
        #[derive(Debug, PartialEq)]
        pub enum Instruction<'a> {
            $(
                $name $(( $arg ))?,
            )*

        }

        impl<'a> Parse<'a> for Instruction<'a> {
            fn parse(parser: Parser<'a>) -> Result<Self> {
                $(
                    #[allow(non_snake_case)]
                    fn $name<'a>(_parser: Parser<'a>) -> Result<Instruction<'a>> {
                        Ok(Instruction::$name $((_parser.parse::<$arg>()?))?)
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
    )
}

instructions! {
    pub enum Instruction<'a> {
        Block(BlockType<'a>) = "block",
        If(BlockType<'a>) = "if",
        Else(Option<ast::Id<'a>>) = "else",
        Loop(BlockType<'a>) = "loop",
        End(Option<ast::Id<'a>>) = "end",

        Unreachable = "unreachable",
        Nop = "nop",
        Br(ast::Index<'a>) = "br",
        BrIf(ast::Index<'a>) = "br_if",
        BrTable(BrTableIndices<'a>) = "br_table",
        Return = "return",
        Call(ast::Index<'a>) = "call",
        CallIndirect(ast::TypeUse<'a>) = "call_indirect",
        Drop = "drop",
        Select = "select",
        LocalSet(ast::Index<'a>) = "local.set",
        LocalGet(ast::Index<'a>) = "local.get" | "get_local",
        LocalTee(ast::Index<'a>) = "local.tee",
        GlobalSet(ast::Index<'a>) = "global.set",
        GlobalGet(ast::Index<'a>) = "global.get",

        I32Load(MemArg) = "i32.load",
        I64Load(MemArg) = "i64.load",
        F32Load(MemArg) = "f32.load",
        F64Load(MemArg) = "f64.load",
        I32Load8s(MemArg) = "i32.load8_s",
        I32Load8u(MemArg) = "i32.load8_u",
        I32Load16s(MemArg) = "i32.load16_s",
        I32Load16u(MemArg) = "i32.load16_u",
        I64Load8s(MemArg) = "i64.load8_s",
        I64Load8u(MemArg) = "i64.load8_u",
        I64Load16s(MemArg) = "i64.load16_s",
        I64Load16u(MemArg) = "i64.load16_u",
        I64Load32s(MemArg) = "i64.load32_s",
        I64Load32u(MemArg) = "i64.load32_u",
        I32Store(MemArg) = "i32.store",
        I64Store(MemArg) = "i64.store",
        F32Store(MemArg) = "f32.store",
        F64Store(MemArg) = "f64.store",
        I32Store8(MemArg) = "i32.store8",
        I32Store16(MemArg) = "i32.store16",
        I64Store8(MemArg) = "i64.store8",
        I64Store16(MemArg) = "i64.store16",
        I64Store32(MemArg) = "i64.store32",
        MemorySize = "memory.size",
        MemoryGrow = "memory.grow",
        I32Const(i32) = "i32.const",
        I64Const(i64) = "i64.const",
        F32Const(ast::Float32<'a>) = "f32.const",
        F64Const(ast::Float64<'a>) = "f64.const",

        I32Clz = "i32.clz",
        I32Ctz = "i32.ctz",
        I32Popcnt = "i32.popcnt",
        I32Add = "i32.add",
        I32Sub = "i32.sub",
        I32Mul = "i32.mul",
        I32DivS = "i32.div_s",
        I32DivU = "i32.div_u",
        I32RemS = "i32.rem_s",
        I32RemU = "i32.rem_u",
        I32And = "i32.and",
        I32Or = "i32.or",
        I32Xor = "i32.xor",
        I32Shl = "i32.shl",
        I32ShrS = "i32.shr_s",
        I32ShrU = "i32.shr_u",
        I32Rotl = "i32.rotl",
        I32Rotr = "i32.rotr",

        I64Clz = "i64.clz",
        I64Ctz = "i64.ctz",
        I64Popcnt = "i64.popcnt",
        I64Add = "i64.add",
        I64Sub = "i64.sub",
        I64Mul = "i64.mul",
        I64DivS = "i64.div_s",
        I64DivU = "i64.div_u",
        I64RemS = "i64.rem_s",
        I64RemU = "i64.rem_u",
        I64And = "i64.and",
        I64Or = "i64.or",
        I64Xor = "i64.xor",
        I64Shl = "i64.shl",
        I64ShrS = "i64.shr_s",
        I64ShrU = "i64.shr_u",
        I64Rotl = "i64.rotl",
        I64Rotr = "i64.rotr",

        F32Abs = "f32.abs",
        F32Neg = "f32.neg",
        F32Ceil = "f32.ceil",
        F32Floor = "f32.floor",
        F32Trunc = "f32.trunc",
        F32Nearest = "f32.nearest",
        F32Sqrt = "f32.sqrt",
        F32Add = "f32.add",
        F32Sub = "f32.sub",
        F32Mul = "f32.mul",
        F32Div = "f32.div",
        F32Min = "f32.min",
        F32Max = "f32.max",
        F32Copysign = "f32.copysign",

        F64Abs = "f64.abs",
        F64Neg = "f64.neg",
        F64Ceil = "f64.ceil",
        F64Floor = "f64.floor",
        F64Trunc = "f64.trunc",
        F64Nearest = "f64.nearest",
        F64Sqrt = "f64.sqrt",
        F64Add = "f64.add",
        F64Sub = "f64.sub",
        F64Mul = "f64.mul",
        F64Div = "f64.div",
        F64Min = "f64.min",
        F64Max = "f64.max",
        F64Copysign = "f64.copysign",

        I32Eqz = "i32.eqz",
        I32Eq = "i32.eq",
        I32Ne = "i32.ne",
        I32LtS = "i32.lt_s",
        I32LtU = "i32.lt_u",
        I32GtS = "i32.gt_s",
        I32GtU = "i32.gt_u",
        I32LeS = "i32.le_s",
        I32LeU = "i32.le_u",
        I32GeS = "i32.ge_s",
        I32GeU = "i32.ge_u",

        I64Eqz = "i64.eqz",
        I64Eq = "i64.eq",
        I64Ne = "i64.ne",
        I64LtS = "i64.lt_s",
        I64LtU = "i64.lt_u",
        I64GtS = "i64.gt_s",
        I64GtU = "i64.gt_u",
        I64LeS = "i64.le_s",
        I64LeU = "i64.le_u",
        I64GeS = "i64.ge_s",
        I64GeU = "i64.ge_u",

        F32Eq = "f32.eq",
        F32Ne = "f32.ne",
        F32Lt = "f32.lt",
        F32Gt = "f32.gt",
        F32Le = "f32.le",
        F32Ge = "f32.ge",

        F64Eq = "f64.eq",
        F64Ne = "f64.ne",
        F64Lt = "f64.lt",
        F64Gt = "f64.gt",
        F64Le = "f64.le",
        F64Ge = "f64.ge",

        I32WrapI64 = "i32.wrap_i64",
        I32TruncF32S = "i32.trunc_f32_s",
        I32TruncF32U = "i32.trunc_f32_u",
        I32TruncF64S = "i32.trunc_f64_s",
        I32TruncF64U = "i32.trunc_f64_u",
        I64ExtendI32S = "i32.extend_i32_s",
        I64ExtendI32U = "i32.extend_i32_u",
        I64TruncF32S = "i64.trunc_f32_s",
        I64TruncF32U = "i64.trunc_f32_u",
        I64TruncF64S = "i64.trunc_f64_s",
        I64TruncF64U = "i64.trunc_f64_u",
        F32ConvertI32S = "f32.convert_i32_s",
        F32ConvertI32U = "f32.convert_i32_u",
        F32ConvertI64S = "f32.convert_i64_s",
        F32ConvertI64U = "f32.convert_i64_u",
        F32DemoteF64 = "f32.demote_f64",
        F64ConvertI32S = "f64.convert_i32_s",
        F64ConvertI32U = "f64.convert_i32_u",
        F64ConvertI64S = "f64.convert_i64_s",
        F64ConvertI64U = "f64.convert_i64_u",
        F64PromoteF32 = "f64.promote_f32",
        I32ReinterpretF32 = "i32.reinterpret_f32",
        I64ReinterpretF64 = "i64.reinterpret_f64",
        F32ReinterpretI32 = "f32.reinterpret_i32",
        F64ReinterpretI64 = "f64.reinterpret_i64",

        AtomicNotify(MemArg) = "atomic.notify",
        I32AtomicWait(MemArg) = "i32.atomic.wait",
        I64AtomicWait(MemArg) = "i64.atomic.wait",
        AtomicFence = "atomic.fence",

        I32AtomicLoad(MemArg) = "i32.atomic.load",
        I64AtomicLoad(MemArg) = "i64.atomic.load",
        I32AtomicLoad8u(MemArg) = "i32.atomic.load8_u",
        I32AtomicLoad16u(MemArg) = "i32.atomic.load16_u",
        I64AtomicLoad8u(MemArg) = "i64.atomic.load8_u",
        I64AtomicLoad16u(MemArg) = "i64.atomic.load16_u",
        I64AtomicLoad32u(MemArg) = "i64.atomic.load32_u",
        I32AtomicStore(MemArg) = "i32.atomic.store",
        I64AtomicStore(MemArg) = "i64.atomic.store",
        I32AtomicStore8(MemArg) = "i32.atomic.store8",
        I32AtomicStore16(MemArg) = "i32.atomic.store16",
        I64AtomicStore8(MemArg) = "i64.atomic.store8",
        I64AtomicStore16(MemArg) = "i64.atomic.store16",
        I64AtomicStore32(MemArg) = "i64.atomic.store32",

        I32AtomicRmwAdd(MemArg) = "i32.atomic.rmw.add",
        I64AtomicRmwAdd(MemArg) = "i64.atomic.rmw.add",
        I32AtomicRmw8AddU(MemArg) = "i32.atomic.rmw8.add_u",
        I32AtomicRmw16AddU(MemArg) = "i32.atomic.rmw16.add_u",
        I64AtomicRmw8AddU(MemArg) = "i64.atomic.rmw8.add_u",
        I64AtomicRmw16AddU(MemArg) = "i64.atomic.rmw16.add_u",
        I64AtomicRmw32AddU(MemArg) = "i64.atomic.rmw32.add_u",

        I32AtomicRmwSub(MemArg) = "i32.atomic.rmw.sub",
        I64AtomicRmwSub(MemArg) = "i64.atomic.rmw.sub",
        I32AtomicRmw8SubU(MemArg) = "i32.atomic.rmw8.sub_u",
        I32AtomicRmw16SubU(MemArg) = "i32.atomic.rmw16.sub_u",
        I64AtomicRmw8SubU(MemArg) = "i64.atomic.rmw8.sub_u",
        I64AtomicRmw16SubU(MemArg) = "i64.atomic.rmw16.sub_u",
        I64AtomicRmw32SubU(MemArg) = "i64.atomic.rmw32.sub_u",

        I32AtomicRmwAnd(MemArg) = "i32.atomic.rmw.and",
        I64AtomicRmwAnd(MemArg) = "i64.atomic.rmw.and",
        I32AtomicRmw8AndU(MemArg) = "i32.atomic.rmw8.and_u",
        I32AtomicRmw16AndU(MemArg) = "i32.atomic.rmw16.and_u",
        I64AtomicRmw8AndU(MemArg) = "i64.atomic.rmw8.and_u",
        I64AtomicRmw16AndU(MemArg) = "i64.atomic.rmw16.and_u",
        I64AtomicRmw32AndU(MemArg) = "i64.atomic.rmw32.and_u",

        I32AtomicRmwOr(MemArg) = "i32.atomic.rmw.or",
        I64AtomicRmwOr(MemArg) = "i64.atomic.rmw.or",
        I32AtomicRmw8OrU(MemArg) = "i32.atomic.rmw8.or_u",
        I32AtomicRmw16OrU(MemArg) = "i32.atomic.rmw16.or_u",
        I64AtomicRmw8OrU(MemArg) = "i64.atomic.rmw8.or_u",
        I64AtomicRmw16OrU(MemArg) = "i64.atomic.rmw16.or_u",
        I64AtomicRmw32OrU(MemArg) = "i64.atomic.rmw32.or_u",

        I32AtomicRmwXor(MemArg) = "i32.atomic.rmw.xor",
        I64AtomicRmwXor(MemArg) = "i64.atomic.rmw.xor",
        I32AtomicRmw8XorU(MemArg) = "i32.atomic.rmw8.xor_u",
        I32AtomicRmw16XorU(MemArg) = "i32.atomic.rmw16.xor_u",
        I64AtomicRmw8XorU(MemArg) = "i64.atomic.rmw8.xor_u",
        I64AtomicRmw16XorU(MemArg) = "i64.atomic.rmw16.xor_u",
        I64AtomicRmw32XorU(MemArg) = "i64.atomic.rmw32.xor_u",

        I32AtomicRmwXchg(MemArg) = "i32.atomic.rmw.xchg",
        I64AtomicRmwXchg(MemArg) = "i64.atomic.rmw.xchg",
        I32AtomicRmw8XchgU(MemArg) = "i32.atomic.rmw8.xchg_u",
        I32AtomicRmw16XchgU(MemArg) = "i32.atomic.rmw16.xchg_u",
        I64AtomicRmw8XchgU(MemArg) = "i64.atomic.rmw8.xchg_u",
        I64AtomicRmw16XchgU(MemArg) = "i64.atomic.rmw16.xchg_u",
        I64AtomicRmw32XchgU(MemArg) = "i64.atomic.rmw32.xchg_u",

        I32AtomicRmwCmpxchg(MemArg) = "i32.atomic.rmw.cmpxchg",
        I64AtomicRmwCmpxchg(MemArg) = "i64.atomic.rmw.cmpxchg",
        I32AtomicRmw8CmpxchgU(MemArg) = "i32.atomic.rmw8.cmpxchg_u",
        I32AtomicRmw16CmpxchgU(MemArg) = "i32.atomic.rmw16.cmpxchg_u",
        I64AtomicRmw8CmpxchgU(MemArg) = "i64.atomic.rmw8.cmpxchg_u",
        I64AtomicRmw16CmpxchgU(MemArg) = "i64.atomic.rmw16.cmpxchg_u",
        I64AtomicRmw32CmpxchgU(MemArg) = "i64.atomic.rmw32.cmpxchg_u",
    }
}

#[derive(Debug, PartialEq)]
pub struct BlockType<'a> {
    pub label: Option<ast::Id<'a>>,
    pub params: Vec<ast::ValType>,
    pub results: Vec<ast::ValType>,
}

impl<'a> Parse<'a> for BlockType<'a> {
    fn parse(parser: Parser<'a>) -> Result<Self> {
        let label = parser.parse()?;
        let mut params = Vec::new();
        if parser.peek2::<kw::param>() {
            parser.parens(|p| {
                p.parse::<kw::param>()?;
                while !p.is_empty() {
                    params.push(p.parse()?);
                }
                Ok(())
            })?
        }
        let mut results = Vec::new();
        if parser.peek2::<kw::result>() {
            parser.parens(|p| {
                p.parse::<kw::result>()?;
                while !p.is_empty() {
                    results.push(p.parse()?);
                }
                Ok(())
            })?
        }
        Ok(BlockType { label, params, results })
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
    pub align: Option<u32>,
    pub offset: u32,
}

impl<'a> Parse<'a> for MemArg {
    fn parse(parser: Parser<'a>) -> Result<Self> {
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
            align: parse_field("align", parser)?,
        })
    }
}
