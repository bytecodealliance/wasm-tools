//! Nodes for intermediate representation of Wasm operators
//!
use egg::Id;
use std::fmt::Display;
use std::str::FromStr;

/// Each "mutable" operator of Wasm is defined here plus articial operators with a
/// a custom behavior.
///
/// You can define custom operators by adding a variant to this enum. Notice
/// that custom operator nodes can only be created from rewriting rules and not
/// form the direct translation of the Wasm binary. After you add a custom node,
/// you need to also implement its parsing from a `string` and its encoding to
/// Wasm. Let's assume for example that we want to implement a custom node that
/// insert three `nop` operators when it is written back to Wasm.
///
/// * We firsts need to define the custom node ```ThreeNops``` as a variant of
///   the [Lang] enum.
/// * Then, we modify the `children`, `children_mut` and `from_op_str` methods
/// the `impl egg::Language for Lang` as follows.
///   ```ignore
///    fn children(&self) -> &[Id] {
///         ...
///         Lang::ThreeNops => &[], // This variant has no children
///         ...
///    }
///
///    fn children_mut (&self) -> &[Id] {
///         ...
///         Lang::ThreeNops => &mut [], // This variant has no children
///         ...
///    }
///
///    fn from_op_str(op_str: &str, children: Vec<Id>) -> Result<Self, String> {
///         ...
///         "three_nops" => Ok(Lang::ThreeNops)
///         ...
///    }
///
///   ```
/// * The custom node needs to translated to Wasm. To do so, you need to modify
///   the `expr2wasm` function somehow as follow
///   ```ignore
///   Lang::ThreeNops  => {
///     newfunc.instruction(&Instruction::Nop);
///     newfunc.instruction(&Instruction::Nop);
///     newfunc.instruction(&Instruction::Nop);
///   }
///   ```
/// * The final step is to use this node in a rewriting rule. Inside the
///   [`rules.rs`]() file
///   ```ignore
///   rewrite!("three-nops";  "?x" => "(container ?x three_nops)"),
///   ```
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Clone)]
pub enum Lang {
    // binops integers
    /// i32.add operator
    I32Add([Id; 2]),
    /// i64.add operator
    I64Add([Id; 2]),
    /// i32.sub  operator
    I32Sub([Id; 2]),
    /// i64.sub  operator
    I64Sub([Id; 2]),
    /// i32.mul  operator
    I32Mul([Id; 2]),
    /// i64.mul  operator
    I64Mul([Id; 2]),
    /// i32.and  operator
    I32And([Id; 2]),
    /// i64.and  operator
    I64And([Id; 2]),
    /// i32.or  operator
    I32Or([Id; 2]),
    /// i64.or  operator
    I64Or([Id; 2]),
    /// i32.xor  operator
    I32Xor([Id; 2]),
    /// i64.xor  operator
    I64Xor([Id; 2]),
    /// i32.shl  operator
    I32Shl([Id; 2]),
    /// i64.shl  operator
    I64Shl([Id; 2]),
    /// i32.shru  operator
    I32ShrU([Id; 2]),
    /// i64.shru  operator
    I64ShrU([Id; 2]),
    /// i32.divu  operator
    I32DivU([Id; 2]),
    /// i64.divu  operator
    I64DivU([Id; 2]),
    /// i32.divs  operator
    I32DivS([Id; 2]),
    /// i64.divs  operator
    I64DivS([Id; 2]),
    /// i32.shrs  operator
    I32ShrS([Id; 2]),
    /// i64.shrs  operator
    I64ShrS([Id; 2]),
    /// i32.rotr  operator
    I32RotR([Id; 2]),
    /// i64.rotr  operator
    I64RotR([Id; 2]),
    /// i32.rotl  operator
    I32RotL([Id; 2]),
    /// i64.rotl  operator
    I64RotL([Id; 2]),
    /// i32.rems  operator
    I32RemS([Id; 2]),
    /// i64.rems  operator
    I64RemS([Id; 2]),
    /// i32.remu  operator
    I32RemU([Id; 2]),
    /// i64.remu  operator
    I64RemU([Id; 2]),
    /// i32.eq  operator
    I32Eq([Id; 2]),
    /// i64.eq  operator
    I64Eq([Id; 2]),
    /// i32.ne  operator
    I32Ne([Id; 2]),
    /// i64.ne  operator
    I64Ne([Id; 2]),
    /// i32.lts  operator
    I32LtS([Id; 2]),
    /// i64.lts  operator
    I64LtS([Id; 2]),
    /// i32.ltu  operator
    I32LtU([Id; 2]),
    /// i64.ltu  operator
    I64LtU([Id; 2]),
    /// i32.gts  operator
    I32GtS([Id; 2]),
    /// i64.gts  operator
    I64GtS([Id; 2]),
    /// i32.gtu  operator
    I32GtU([Id; 2]),
    /// i64.gtu  operator
    I64GtU([Id; 2]),
    /// i32.les  operator
    I32LeS([Id; 2]),
    /// i64.les  operator
    I64LeS([Id; 2]),
    /// i32.leu  operator
    I32LeU([Id; 2]),
    /// i64.leu  operator
    I64LeU([Id; 2]),
    /// i32.ges  operator
    I32GeS([Id; 2]),
    /// i64.ges  operator
    I64GeS([Id; 2]),
    /// i32.geu  operator
    I32GeU([Id; 2]),
    /// i64.geu  operator
    I64GeU([Id; 2]),
    // binops floats
    /// f32.add  operator
    F32Add([Id; 2]),
    /// f64.add  operator
    F64Add([Id; 2]),
    /// f32.sub  operator
    F32Sub([Id; 2]),
    /// f64.sub  operator
    F64Sub([Id; 2]),
    /// f32.mul  operator
    F32Mul([Id; 2]),
    /// f64.mul  operator
    F64Mul([Id; 2]),
    /// f32.div  operator
    F32Div([Id; 2]),
    /// f64.div  operator
    F64Div([Id; 2]),
    /// f32.min  operator
    F32Min([Id; 2]),
    /// f64.min  operator
    F64Min([Id; 2]),
    /// f32.max  operator
    F32Max([Id; 2]),
    /// f64.max  operator
    F64Max([Id; 2]),
    /// f32.copysign  operator
    F32Copysign([Id; 2]),
    /// f64.copysign  operator
    F64Copysign([Id; 2]),
    // frelops
    /// f32.eq  operator
    F32Eq([Id; 2]),
    /// f64.eq  operator
    F64Eq([Id; 2]),
    /// f32.ne  operator
    F32Ne([Id; 2]),
    /// f64.ne  operator
    F64Ne([Id; 2]),
    /// f32.lt  operator
    F32Lt([Id; 2]),
    /// f64.lt  operator
    F64Lt([Id; 2]),
    /// f32.gt  operator
    F32Gt([Id; 2]),
    /// f64.gt  operator
    F64Gt([Id; 2]),
    /// f32.le  operator
    F32Le([Id; 2]),
    /// f64.le  operator
    F64Le([Id; 2]),
    /// f32.ge  operator
    F32Ge([Id; 2]),
    /// f64.ge  operator
    F64Ge([Id; 2]),
    // unops integers
    /// i32.eqz  operator
    I32Eqz([Id; 1]),
    /// i64.eqz  operator
    I64Eqz([Id; 1]),

    /// i32.popcnt  operator
    I32Popcnt([Id; 1]),
    /// i64.popcnt  operator
    I64Popcnt([Id; 1]),
    /// i32.clz  operator
    I32Clz([Id; 1]),
    /// i32.ctz  operator
    I32Ctz([Id; 1]),
    /// i64.ctz  operator
    I64Ctz([Id; 1]),
    /// i64.clz  operator
    I64Clz([Id; 1]),

    // unops floats
    /// f32.abs  operator
    F32Abs([Id; 1]),
    /// f64.abs  operator
    F64Abs([Id; 1]),
    /// f32.neg  operator
    F32Neg([Id; 1]),
    /// f64.neg  operator
    F64Neg([Id; 1]),
    /// f32.sqrt  operator
    F32Sqrt([Id; 1]),
    /// f64.sqrt  operator
    F64Sqrt([Id; 1]),
    /// f32.ceil  operator
    F32Ceil([Id; 1]),
    /// f64.ceil  operator
    F64Ceil([Id; 1]),
    /// f32.floor  operator
    F32Floor([Id; 1]),
    /// f64.floor  operator
    F64Floor([Id; 1]),
    /// f32.trunc  operator
    F32Trunc([Id; 1]),
    /// f64.trunc  operator
    F64Trunc([Id; 1]),
    /// f32.nearest  operator
    F32Nearest([Id; 1]),
    /// f64.nearest  operator
    F64Nearest([Id; 1]),

    // Locals
    // Idx and value
    /// local.tee operator
    LocalTee(u32, Id),
    // Idx and value
    /// local.set operator
    LocalSet(u32, Id),
    /// local.get operator
    LocalGet(u32),

    // Globals
    // Idx and value
    /// global.set operator
    GlobalSet(u32, Id),
    /// global.get operator
    GlobalGet(u32),
    // conversion operators
    /// i32.wrap_i64 operator
    Wrap([Id; 1]),

    // conversion
    /// i32.extend.8s operator
    I32Extend8S([Id; 1]),
    /// i64.extend.8s operator
    I64Extend8S([Id; 1]),
    /// i32.extend.16s operator
    I32Extend16S([Id; 1]),
    /// i64.extend.16s operator
    I64Extend16S([Id; 1]),
    /// i64.extend.32s operator
    I64Extend32S([Id; 1]),
    /// i64.extendi.32s operator
    I64ExtendI32S([Id; 1]),
    /// i64.extendi.32u operator
    I64ExtendI32U([Id; 1]),
    /// i32.truncf.32s operator
    I32TruncF32S([Id; 1]),
    /// i32.truncf.32u operator
    I32TruncF32U([Id; 1]),
    /// i32.truncf.64s operator
    I32TruncF64S([Id; 1]),
    /// i32.truncf.64u operator
    I32TruncF64U([Id; 1]),
    /// i64.truncf.32s operator
    I64TruncF32S([Id; 1]),
    /// i64.truncf.32u operator
    I64TruncF32U([Id; 1]),
    /// i64.truncf.64s operator
    I64TruncF64S([Id; 1]),
    /// i64.truncf.64u operator
    I64TruncF64U([Id; 1]),
    /// f32.converti.32s operator
    F32ConvertI32S([Id; 1]),
    /// f32.converti.32u operator
    F32ConvertI32U([Id; 1]),
    /// f32.converti.64s operator
    F32ConvertI64S([Id; 1]),
    /// f32.converti.64u operator
    F32ConvertI64U([Id; 1]),
    /// f32.demote.f64 operator
    F32DemoteF64([Id; 1]),
    /// f64.converti.32s operator
    F64ConvertI32S([Id; 1]),
    /// f64.converti.32u operator
    F64ConvertI32U([Id; 1]),
    /// f64.converti.64s operator
    F64ConvertI64S([Id; 1]),
    /// f64.converti.64u operator
    F64ConvertI64U([Id; 1]),
    /// f64.promote.f32 operator
    F64PromoteF32([Id; 1]),
    /// i32.reinterpret.f32 operator
    I32ReinterpretF32([Id; 1]),
    /// i64.reinterpret.f64 operator
    I64ReinterpretF64([Id; 1]),
    /// f32.reinterpret.i32 operator
    F32ReinterpretI32([Id; 1]),
    /// f64.reinterpret.i64 operator
    F64ReinterpretI64([Id; 1]),
    /// i32.truncsatf.32s operator
    I32TruncSatF32S([Id; 1]),
    /// i32.truncsatf.32u operator
    I32TruncSatF32U([Id; 1]),
    /// i32.truncsatf.64s operator
    I32TruncSatF64S([Id; 1]),
    /// i32.truncsatf.64u operator
    I32TruncSatF64U([Id; 1]),
    /// i64.truncsatf.32s operator
    I64TruncSatF32S([Id; 1]),
    /// i64.truncsatf.32u operator
    I64TruncSatF32U([Id; 1]),
    /// i64.truncsatf.64s operator
    I64TruncSatF64S([Id; 1]),
    /// i64.truncsatf.64u operator
    I64TruncSatF64U([Id; 1]),

    // The u32 argument should be the function index
    /// call operator node
    Call(usize, Vec<Id>),
    /// drop operator
    Drop([Id; 1]),
    /// nop operator
    Nop,
    // Memory operations
    // loads
    /// i32.load operator
    I32Load {
        /// Inmediate static offset of the store operator
        static_offset: u64,
        /// Inmediate align value of the store operator
        align: u8,
        /// Inmediate mem value of the store operator
        mem: u32,
        /// Dynamic offset node id
        offset: Id,
    },
    /// i64.load operator
    I64Load {
        /// Inmediate static offset of the store operator
        static_offset: u64,
        /// Inmediate align value of the store operator
        align: u8,
        /// Inmediate mem value of the store operator
        mem: u32,
        /// Dynamic offset node id
        offset: Id,
    },
    /// f32.load operator
    F32Load {
        /// Inmediate static offset of the store operator
        static_offset: u64,
        /// Inmediate align value of the store operator
        align: u8,
        /// Inmediate mem value of the store operator
        mem: u32,
        /// Dynamic offset node id
        offset: Id,
    },
    /// f64.load operator
    F64Load {
        /// Inmediate static offset of the store operator
        static_offset: u64,
        /// Inmediate align value of the store operator
        align: u8,
        /// Inmediate mem value of the store operator
        mem: u32,
        /// Dynamic offset node id
        offset: Id,
    },
    /// i32.load8_s operator
    I32Load8S {
        /// Inmediate static offset of the store operator
        static_offset: u64,
        /// Inmediate align value of the store operator
        align: u8,
        /// Inmediate mem value of the store operator
        mem: u32,
        /// Dynamic offset node id
        offset: Id,
    },
    /// i32.load8_u operator
    I32Load8U {
        /// Inmediate static offset of the store operator
        static_offset: u64,
        /// Inmediate align value of the store operator
        align: u8,
        /// Inmediate mem value of the store operator
        mem: u32,
        /// Dynamic offset node id
        offset: Id,
    },
    /// i32.load16_s operator
    I32Load16S {
        /// Inmediate static offset of the store operator
        static_offset: u64,
        /// Inmediate align value of the store operator
        align: u8,
        /// Inmediate mem value of the store operator
        mem: u32,
        /// Dynamic offset node id
        offset: Id,
    },
    /// i32.load16_u operator
    I32Load16U {
        /// Inmediate static offset of the store operator
        static_offset: u64,
        /// Inmediate align value of the store operator
        align: u8,
        /// Inmediate mem value of the store operator
        mem: u32,
        /// Dynamic offset node id
        offset: Id,
    },
    /// i64.load8_s operator
    I64Load8S {
        /// Inmediate static offset of the store operator
        static_offset: u64,
        /// Inmediate align value of the store operator
        align: u8,
        /// Inmediate mem value of the store operator
        mem: u32,
        /// Dynamic offset node id
        offset: Id,
    },
    /// i64.load8_u operator
    I64Load8U {
        /// Inmediate static offset of the store operator
        static_offset: u64,
        /// Inmediate align value of the store operator
        align: u8,
        /// Inmediate mem value of the store operator
        mem: u32,
        /// Dynamic offset node id
        offset: Id,
    },
    /// i64.load16_s operator
    I64Load16S {
        /// Inmediate static offset of the store operator
        static_offset: u64,
        /// Inmediate align value of the store operator
        align: u8,
        /// Inmediate mem value of the store operator
        mem: u32,
        /// Dynamic offset node id
        offset: Id,
    },
    /// i64.load16_u operator
    I64Load16U {
        /// Inmediate static offset of the store operator
        static_offset: u64,
        /// Inmediate align value of the store operator
        align: u8,
        /// Inmediate mem value of the store operator
        mem: u32,
        /// Dynamic offset node id
        offset: Id,
    },
    /// i64.load32_s operator
    I64Load32S {
        /// Inmediate static offset of the store operator
        static_offset: u64,
        /// Inmediate align value of the store operator
        align: u8,
        /// Inmediate mem value of the store operator
        mem: u32,
        /// Dynamic offset node id
        offset: Id,
    },
    /// i64.load32_u operator
    I64Load32U {
        /// Inmediate static offset of the store operator
        static_offset: u64,
        /// Inmediate align value of the store operator
        align: u8,
        /// Inmediate mem value of the store operator
        mem: u32,
        /// Dynamic offset node id
        offset: Id,
    },

    /// i32.store operator
    I32Store {
        /// Inmediate static offset of the store operator
        static_offset: u64,
        /// Inmediate align value of the store operator
        align: u8,
        /// Inmediate mem value of the store operator
        mem: u32,
        /// Value and dynamic offset node Ids
        value_and_offset: [Id; 2],
    },
    /// i64.store operator
    I64Store {
        /// Inmediate static offset of the store operator
        static_offset: u64,
        /// Inmediate align value of the store operator
        align: u8,
        /// Inmediate mem value of the store operator
        mem: u32,
        /// Value and dynamic offset node Ids
        value_and_offset: [Id; 2],
    },
    /// f32.store operator
    F32Store {
        /// Inmediate static offset of the store operator
        static_offset: u64,
        /// Inmediate align value of the store operator
        align: u8,
        /// Inmediate mem value of the store operator
        mem: u32,
        /// Value and dynamic offset node Ids
        value_and_offset: [Id; 2],
    },
    /// f64.store operator
    F64Store {
        /// Inmediate static offset of the store operator
        static_offset: u64,
        /// Inmediate align value of the store operator
        align: u8,
        /// Inmediate mem value of the store operator
        mem: u32,
        /// Value and dynamic offset node Ids
        value_and_offset: [Id; 2],
    },
    /// i32.store8 operator
    I32Store8 {
        /// Inmediate static offset of the store operator
        static_offset: u64,
        /// Inmediate align value of the store operator
        align: u8,
        /// Inmediate mem value of the store operator
        mem: u32,
        /// Value and dynamic offset node Ids
        value_and_offset: [Id; 2],
    },
    /// i32.store16 operator
    I32Store16 {
        /// Inmediate static offset of the store operator
        static_offset: u64,
        /// Inmediate align value of the store operator
        align: u8,
        /// Inmediate mem value of the store operator
        mem: u32,
        /// Value and dynamic offset node Ids
        value_and_offset: [Id; 2],
    },
    /// i64.store8 operator
    I64Store8 {
        /// Inmediate static offset of the store operator
        static_offset: u64,
        /// Inmediate align value of the store operator
        align: u8,
        /// Inmediate mem value of the store operator
        mem: u32,
        /// Value and dynamic offset node Ids
        value_and_offset: [Id; 2],
    },
    /// i64.store16 operator
    I64Store16 {
        /// Inmediate static offset of the store operator
        static_offset: u64,
        /// Inmediate align value of the store operator
        align: u8,
        /// Inmediate mem value of the store operator
        mem: u32,
        /// Value and dynamic offset node Ids
        value_and_offset: [Id; 2],
    },
    /// i32.store32 operator
    I64Store32 {
        /// Inmediate static offset of the store operator
        static_offset: u64,
        /// Inmediate align value of the store operator
        align: u8,
        /// Inmediate mem value of the store operator
        mem: u32,
        /// Value and dynamic offset node Ids
        value_and_offset: [Id; 2],
    },
    /// Select operator
    Select([Id; 3]),
    /// Memory grow operator
    MemoryGrow {
        /// immediate mem value
        mem: u32,
        /// immediate mem byte value
        mem_byte: u8,
        /// by node idx
        by: Id,
    },
    /// Memory size operator
    MemorySize {
        /// immediate mem value
        mem: u32,
        /// immediate mem byte
        mem_byte: u8,
    },

    /// Add custom or others operator nodes below

    /// Custom mutation operations and instructions
    ///
    /// This operation represent a random i32 integer
    RandI32,
    /// This operation represent a random i64 integer
    RandI64,

    /// This instructions is used to define unknown operands, for example when the value can come from the join of several basic blocks in a dfg
    Undef,
    /// Takes one i32 constant operand and turn it into a sum of two random numbers whihch sum is the operand `i32.const x = i32.const r + i32.const (x - r) `
    UnfoldI32(Id),
    /// Takes one i64 constant operand and turn it into a sum of two random numbers whihch sum is the operand `i64.const x = i64.const r + i64.const (x - r) `
    UnfoldI64(Id),

    /// Propsoed custom mutator from issue #391, use-of-global
    I32UseGlobal(Id),
    /// Propsoed custom mutator from issue #391, use-of-global
    I64UseGlobal(Id),
    /// Propsoed custom mutator from issue #391, use-of-global
    F32UseGlobal(Id),
    /// Propsoed custom mutator from issue #391, use-of-global
    F64UseGlobal(Id),

    /// Just a wrapper of multiple nodes, when encoding to Wasm it is written as
    /// nothing. Its only responsibility is to express stack-neutral operations.
    ///
    /// For example, let's assume we want to insert a nop operation after or
    /// before another node (or both). `container` allows us to do this with the
    /// following rewrites:
    ///
    /// * Before: `?x => (container nop ?x)`
    /// * After: `?x => (container ?x nop)`
    /// * Before and after: `?x => (container nop ?x nop)`
    /// * Stack-nuetral insertion: `?x => (container (drop i32.rand) ?x) `
    Container(Vec<Id>),

    // End of custom mutation operations and instructions
    /// I32 constant node
    I32(i32),
    /// I64 constant node
    I64(i64),
    // Save bits
    /// F32 constant node
    F32(u32),
    /// F64 constant node
    F64(u64),
}

impl Display for Lang {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Lang::LocalSet(idx, _) => f.write_str(&format!("local.set.{}", idx)),
            Lang::LocalTee(idx, _) => f.write_str(&format!("local.tee.{}", idx)),
            Lang::I32(val) => f.write_str(&format!("{}_i32", val)),
            Lang::I64(val) => f.write_str(&format!("{}_i64", val)),
            Lang::F32(v) => f.write_str(&format!("{}_f32", *v)),
            Lang::F64(v) => f.write_str(&format!("{}_f64", *v)),
            Lang::I32Add(_) => f.write_str("i32.add"),
            Lang::I64Add(_) => f.write_str("i64.add"),
            Lang::I32Sub(_) => f.write_str("i32.sub"),
            Lang::I64Sub(_) => f.write_str("i64.sub"),
            Lang::I32Mul(_) => f.write_str("i32.mul"),
            Lang::I64Mul(_) => f.write_str("i64.mul"),
            Lang::I32And(_) => f.write_str("i32.and"),
            Lang::I64And(_) => f.write_str("i64.and"),
            Lang::I32Or(_) => f.write_str("i32.or"),
            Lang::I64Or(_) => f.write_str("i64.or"),
            Lang::I32Xor(_) => f.write_str("i32.xor"),
            Lang::I64Xor(_) => f.write_str("i64.xor"),
            Lang::I32Shl(_) => f.write_str("i32.shl"),
            Lang::I64Shl(_) => f.write_str("i64.shl"),
            Lang::I32ShrU(_) => f.write_str("i32.shr_u"),
            Lang::I64ShrU(_) => f.write_str("i64.shr_u"),
            Lang::I32DivU(_) => f.write_str("i32.div_u"),
            Lang::I64DivU(_) => f.write_str("i64.div_u"),
            Lang::I32DivS(_) => f.write_str("i32.div_s"),
            Lang::I64DivS(_) => f.write_str("i64.div_s"),
            Lang::I32ShrS(_) => f.write_str("i32.shr_s"),
            Lang::I64ShrS(_) => f.write_str("i64.shr_s"),
            Lang::I32RotR(_) => f.write_str("i32.rotr"),
            Lang::I64RotR(_) => f.write_str("i64.rotr"),
            Lang::I32RotL(_) => f.write_str("i32.rotl"),
            Lang::I64RotL(_) => f.write_str("i64.rotl"),
            Lang::I32RemS(_) => f.write_str("i32.rem_s"),
            Lang::I64RemS(_) => f.write_str("i64.rem_s"),
            Lang::I32RemU(_) => f.write_str("i32.rem_u"),
            Lang::I64RemU(_) => f.write_str("i64.rem_u"),
            Lang::I32Eqz(_) => f.write_str("i32.eqz"),
            Lang::I64Eqz(_) => f.write_str("i64.eqz"),
            Lang::I32Eq(_) => f.write_str("i32.eq"),
            Lang::I64Eq(_) => f.write_str("i64.eq"),
            Lang::I32Ne(_) => f.write_str("i32.ne"),
            Lang::I64Ne(_) => f.write_str("i64.ne"),
            Lang::I32LtS(_) => f.write_str("i32.lt_s"),
            Lang::I64LtS(_) => f.write_str("i64.lt_s"),
            Lang::I32LtU(_) => f.write_str("i32.lt_u"),
            Lang::I64LtU(_) => f.write_str("i64.lt_u"),
            Lang::I32GtS(_) => f.write_str("i32.gt_s"),
            Lang::I64GtS(_) => f.write_str("i64.gt_s"),
            Lang::I32GtU(_) => f.write_str("i32.gt_u"),
            Lang::I64GtU(_) => f.write_str("i64.gt_u"),
            Lang::I32LeS(_) => f.write_str("i32.le_s"),
            Lang::I64LeS(_) => f.write_str("i64.le_s"),
            Lang::I32LeU(_) => f.write_str("i32.le_u"),
            Lang::I64LeU(_) => f.write_str("i64.le_u"),
            Lang::I32GeS(_) => f.write_str("i32.ge_s"),
            Lang::I64GeS(_) => f.write_str("i64.ge_s"),
            Lang::I32GeU(_) => f.write_str("i32.ge_u"),
            Lang::I64GeU(_) => f.write_str("i64.ge_u"),
            Lang::I32Popcnt(_) => f.write_str("i32.popcnt"),
            Lang::I64Popcnt(_) => f.write_str("i64.popcnt"),
            Lang::F32Add(_) => f.write_str("f32.add"),
            Lang::F64Add(_) => f.write_str("f64.add"),
            Lang::F32Sub(_) => f.write_str("f32.sub"),
            Lang::F64Sub(_) => f.write_str("f64.sub"),
            Lang::F32Mul(_) => f.write_str("f32.mul"),
            Lang::F64Mul(_) => f.write_str("f64.mul"),
            Lang::F32Div(_) => f.write_str("f32.div"),
            Lang::F64Div(_) => f.write_str("f64.div"),
            Lang::F32Min(_) => f.write_str("f32.min"),
            Lang::F64Min(_) => f.write_str("f64.min"),
            Lang::F32Max(_) => f.write_str("f32.max"),
            Lang::F64Max(_) => f.write_str("f64.max"),
            Lang::F32Copysign(_) => f.write_str("f32.copysign"),
            Lang::F64Copysign(_) => f.write_str("f64.copysign"),
            Lang::F32Eq(_) => f.write_str("f32.eq"),
            Lang::F64Eq(_) => f.write_str("f64.eq"),
            Lang::F32Ne(_) => f.write_str("f32.ne"),
            Lang::F64Ne(_) => f.write_str("f64.ne"),
            Lang::F32Lt(_) => f.write_str("f32.lt"),
            Lang::F64Lt(_) => f.write_str("f64.lt"),
            Lang::F32Gt(_) => f.write_str("f32.gt"),
            Lang::F64Gt(_) => f.write_str("f64.gt"),
            Lang::F32Le(_) => f.write_str("f32.le"),
            Lang::F64Le(_) => f.write_str("f64.le"),
            Lang::F32Ge(_) => f.write_str("f32.ge"),
            Lang::F64Ge(_) => f.write_str("f64.ge"),
            Lang::I32Clz(_) => f.write_str("i32.clz"),
            Lang::I32Ctz(_) => f.write_str("i32.ctz"),
            Lang::I64Ctz(_) => f.write_str("i64.ctz"),
            Lang::I64Clz(_) => f.write_str("i64.clz"),
            Lang::F32Abs(_) => f.write_str("f32.abs"),
            Lang::F64Abs(_) => f.write_str("f64.abs"),
            Lang::F32Neg(_) => f.write_str("f32.neg"),
            Lang::F64Neg(_) => f.write_str("f64.neg"),
            Lang::F32Sqrt(_) => f.write_str("f32.sqrt"),
            Lang::F64Sqrt(_) => f.write_str("f64.sqrt"),
            Lang::F32Ceil(_) => f.write_str("f32.ceil"),
            Lang::F64Ceil(_) => f.write_str("f64.ceil"),
            Lang::F32Floor(_) => f.write_str("f32.floor"),
            Lang::F64Floor(_) => f.write_str("f64.floor"),
            Lang::F32Trunc(_) => f.write_str("f32.trunc"),
            Lang::F64Trunc(_) => f.write_str("f64.trunc"),
            Lang::F32Nearest(_) => f.write_str("f32.nearest"),
            Lang::F64Nearest(_) => f.write_str("f64.nearest"),
            Lang::LocalGet(idx) => f.write_str(&format!("local.get.{}", idx)),
            Lang::GlobalGet(idx) => f.write_str(&format!("global.get.{}", idx)),
            Lang::GlobalSet(idx, _) => f.write_str(&format!("global.set.{}", idx)),
            Lang::Wrap(_) => f.write_str("wrap"),
            Lang::I32Extend8S(_) => f.write_str("i32.extend8_s"),
            Lang::I64Extend8S(_) => f.write_str("i64.extend8_s"),
            Lang::I32Extend16S(_) => f.write_str("i32.extend16_s"),
            Lang::I64Extend16S(_) => f.write_str("i64.extend16_s"),
            Lang::I64Extend32S(_) => f.write_str("i64.extend32_s"),
            Lang::I64ExtendI32S(_) => f.write_str("i64.extendi32_s"),
            Lang::I64ExtendI32U(_) => f.write_str("i64.extendi32_u"),
            Lang::I32TruncF32S(_) => f.write_str("i32.truncf32_s"),
            Lang::I32TruncF32U(_) => f.write_str("i32.truncf32_u"),
            Lang::I32TruncF64S(_) => f.write_str("i32.truncf64_s"),
            Lang::I32TruncF64U(_) => f.write_str("i32.truncf64_u"),
            Lang::I64TruncF32S(_) => f.write_str("i64.truncf32_s"),
            Lang::I64TruncF32U(_) => f.write_str("i64.truncf32_u"),
            Lang::I64TruncF64S(_) => f.write_str("i64.truncf64_s"),
            Lang::I64TruncF64U(_) => f.write_str("i64.truncf64_u"),
            Lang::F32ConvertI32S(_) => f.write_str("f32.converti32_s"),
            Lang::F32ConvertI32U(_) => f.write_str("f32.converti32_u"),
            Lang::F32ConvertI64S(_) => f.write_str("f32.converti64_s"),
            Lang::F32ConvertI64U(_) => f.write_str("f32.converti64_u"),
            Lang::F32DemoteF64(_) => f.write_str("f32.demotef64"),
            Lang::F64ConvertI32S(_) => f.write_str("f64.converti32_s"),
            Lang::F64ConvertI32U(_) => f.write_str("f64.converti32_u"),
            Lang::F64ConvertI64S(_) => f.write_str("f64.converti64_s"),
            Lang::F64ConvertI64U(_) => f.write_str("f64.converti64_u"),
            Lang::F64PromoteF32(_) => f.write_str("f64.promotef32"),
            Lang::I32ReinterpretF32(_) => f.write_str("i32.reinterpretf32"),
            Lang::I64ReinterpretF64(_) => f.write_str("i64.reinterpretf64"),
            Lang::F32ReinterpretI32(_) => f.write_str("f32.reinterpreti32"),
            Lang::F64ReinterpretI64(_) => f.write_str("f64.reinterpreti64"),
            Lang::I32TruncSatF32S(_) => f.write_str("i32.truncsatf32_s"),
            Lang::I32TruncSatF32U(_) => f.write_str("i32.truncsatf32_u"),
            Lang::I32TruncSatF64S(_) => f.write_str("i32.truncsatf64_s"),
            Lang::I32TruncSatF64U(_) => f.write_str("i32.truncsatf64_u"),
            Lang::I64TruncSatF32S(_) => f.write_str("i64.truncsatf32_s"),
            Lang::I64TruncSatF32U(_) => f.write_str("i64.truncsatf32_u"),
            Lang::I64TruncSatF64S(_) => f.write_str("i64.truncsatf64_s"),
            Lang::I64TruncSatF64U(_) => f.write_str("i64.truncsatf64_u"),
            Lang::Call(idx, _) => f.write_str(&format!("call.{}", idx)),
            Lang::Drop(_) => f.write_str("drop"),
            Lang::I32Load {
                static_offset,
                align,
                mem,
                offset: _,
            } => f.write_str(&format!("i32.load.{}.{}.{}", static_offset, align, mem)),
            Lang::I64Load {
                static_offset,
                align,
                mem,
                offset: _,
            } => f.write_str(&format!("i64.load.{}.{}.{}", static_offset, align, mem)),
            Lang::F32Load {
                static_offset,
                align,
                mem,
                offset: _,
            } => f.write_str(&format!("f32.load.{}.{}.{}", static_offset, align, mem)),
            Lang::F64Load {
                static_offset,
                align,
                mem,
                offset: _,
            } => f.write_str(&format!("f64.load.{}.{}.{}", static_offset, align, mem)),
            Lang::I32Load8S {
                static_offset,
                align,
                mem,
                offset: _,
            } => f.write_str(&format!("i32.load8s.{}.{}.{}", static_offset, align, mem)),
            Lang::I32Load8U {
                static_offset,
                align,
                mem,
                offset: _,
            } => f.write_str(&format!("i32.load8u.{}.{}.{}", static_offset, align, mem)),
            Lang::I32Load16S {
                static_offset,
                align,
                mem,
                offset: _,
            } => f.write_str(&format!("i32.load16s.{}.{}.{}", static_offset, align, mem)),
            Lang::I32Load16U {
                static_offset,
                align,
                mem,
                offset: _,
            } => f.write_str(&format!("i32.load16u.{}.{}.{}", static_offset, align, mem)),
            Lang::I64Load8S {
                static_offset,
                align,
                mem,
                offset: _,
            } => f.write_str(&format!("i64.load8s.{}.{}.{}", static_offset, align, mem)),
            Lang::I64Load8U {
                static_offset,
                align,
                mem,
                offset: _,
            } => f.write_str(&format!("i64.load8u.{}.{}.{}", static_offset, align, mem)),
            Lang::I64Load16S {
                static_offset,
                align,
                mem,
                offset: _,
            } => f.write_str(&format!("i64.load16s.{}.{}.{}", static_offset, align, mem)),
            Lang::I64Load16U {
                static_offset,
                align,
                mem,
                offset: _,
            } => f.write_str(&format!("i64.load16u.{}.{}.{}", static_offset, align, mem)),
            Lang::I64Load32S {
                static_offset,
                align,
                mem,
                offset: _,
            } => f.write_str(&format!("i64.load32s.{}.{}.{}", static_offset, align, mem)),
            Lang::I64Load32U {
                static_offset,
                align,
                mem,
                offset: _,
            } => f.write_str(&format!("i64.load32u.{}.{}.{}", static_offset, align, mem)),
            Lang::I32Store {
                static_offset,
                align,
                mem,
                value_and_offset: _,
            } => f.write_str(&format!("i32.store.{}.{}.{}", static_offset, align, mem)),
            Lang::I64Store {
                static_offset,
                align,
                mem,
                value_and_offset: _,
            } => f.write_str(&format!("i64.store.{}.{}.{}", static_offset, align, mem)),
            Lang::F32Store {
                static_offset,
                align,
                mem,
                value_and_offset: _,
            } => f.write_str(&format!("f32.store.{}.{}.{}", static_offset, align, mem)),
            Lang::F64Store {
                static_offset,
                align,
                mem,
                value_and_offset: _,
            } => f.write_str(&format!("f64.store.{}.{}.{}", static_offset, align, mem)),
            Lang::I32Store8 {
                static_offset,
                align,
                mem,
                value_and_offset: _,
            } => f.write_str(&format!("i32.store8.{}.{}.{}", static_offset, align, mem)),
            Lang::I32Store16 {
                static_offset,
                align,
                mem,
                value_and_offset: _,
            } => f.write_str(&format!("i32.store16.{}.{}.{}", static_offset, align, mem)),
            Lang::I64Store8 {
                static_offset,
                align,
                mem,
                value_and_offset: _,
            } => f.write_str(&format!("i64.store8.{}.{}.{}", static_offset, align, mem)),
            Lang::I64Store16 {
                static_offset,
                align,
                mem,
                value_and_offset: _,
            } => f.write_str(&format!("i64.store16.{}.{}.{}", static_offset, align, mem)),
            Lang::I64Store32 {
                static_offset,
                align,
                mem,
                value_and_offset: _,
            } => f.write_str(&format!("i64.store32.{}.{}.{}", static_offset, align, mem)),

            Lang::RandI32 => f.write_str("i32.rand"),
            Lang::RandI64 => f.write_str("i64.rand"),
            Lang::Undef => f.write_str("undef"),
            Lang::UnfoldI32(_) => f.write_str("i32.unfold"),
            Lang::UnfoldI64(_) => f.write_str("i64.unfold"),
            Lang::Nop => f.write_str("nop"),
            Lang::Container(_) => f.write_str("container"),
            Lang::Select(_) => f.write_str("select"),
            Lang::MemoryGrow { mem, mem_byte, .. } => {
                f.write_str(&format!("memory.grow.{}.{}", mem, mem_byte))
            }
            Lang::MemorySize { mem, mem_byte } => {
                f.write_str(&format!("memory.size.{}.{}", mem, mem_byte))
            }
            Lang::I32UseGlobal(_) => f.write_str("i32.use_of_global"),
            Lang::I64UseGlobal(_) => f.write_str("i64.use_of_global"),
            Lang::F32UseGlobal(_) => f.write_str("f32.use_of_global"),
            Lang::F64UseGlobal(_) => f.write_str("f64.use_of_global"),
        }
    }
}

impl Lang {
    /// Parse type annotated integers in the form
    /// $i_(i32|i64)
    pub fn parse_integer(op_str: &str) -> Result<Self, String> {
        // Check for type annotation in the tail
        if op_str.len() < 4 {
            return Err(format!("Missing type annotation for integer {}", op_str));
        }

        let n = &op_str[..op_str.len() - 4];
        let tail = &op_str[op_str.len() - 4..];

        match tail {
            "_i32" => Ok(Lang::I32(
                i32::from_str(n).expect("Invalid integer parsing radix 10"),
            )),
            "_i64" => Ok(Lang::I64(
                i64::from_str(n).expect("Invalid integer parsing radix 10"),
            )),
            // Notice that the rewriting rules should be written in the integer representation of the float bits
            "_f32" => Ok(Lang::F32(u32::from_str(n).expect("Invalid float parsing"))),
            "_f64" => Ok(Lang::F64(u64::from_str(n).expect("Invalid float parsing"))),
            // Add other types here
            _ => Err(format!("Invalid type annotation for {:?}", op_str)),
        }
    }

    /// Parses index operations written in the textual form
    /// local.(get|set|tee).$i
    /// global.(get|set).$i
    pub fn parse_index_op(op_str: &str, children: &Vec<Id>) -> Result<Self, String> {
        let splat = op_str.split('.');
        let ops = splat.collect::<Vec<_>>();
        if ops.len() != 3 {
            return Err(format!("Invalid index based operation {}", op_str));
        }

        // In theory indices can have eclasses as well
        // If we want to have index-change-like mutators
        let i = u32::from_str(ops[2]).unwrap();
        match &ops[..2] {
            ["local", "get"] => Ok(Lang::LocalGet(i)),
            ["local", "set"] => Ok(Lang::LocalSet(i, children[0])),
            ["local", "tee"] => Ok(Lang::LocalTee(i, children[0])),
            ["global", "get"] => Ok(Lang::GlobalGet(i)),
            ["global", "set"] => Ok(Lang::GlobalSet(i, children[0])),
            _ => Err(format!("Invalid index based operation {:?}", op_str)),
        }
    }

    /// Parses index mem operations written in the textual form
    /// `(i32|i64|...).(store|load).$static_offset.$align.$mem`.
    pub fn parse_mem_op(op_str: &str, children: &Vec<Id>) -> Result<Self, String> {
        let splat = op_str.split('.');
        let ops = splat.collect::<Vec<_>>();
        if ops.len() != 5 {
            return Err(format!("Invalid mem operation operation {}", op_str));
        }

        let static_offset = u64::from_str(ops[2]).unwrap();
        let align = u8::from_str(ops[3]).unwrap();
        let mem = u32::from_str(ops[4]).unwrap();

        match &ops[..2] {
            ["i32", "load"] => Ok(Lang::I32Load {
                static_offset,
                align,
                mem,
                offset: children[0],
            }),
            ["i64", "load"] => Ok(Lang::I64Load {
                static_offset,
                align,
                mem,
                offset: children[0],
            }),
            ["f32", "load"] => Ok(Lang::F32Load {
                static_offset,
                align,
                mem,
                offset: children[0],
            }),
            ["f64", "load"] => Ok(Lang::F64Load {
                static_offset,
                align,
                mem,
                offset: children[0],
            }),
            ["i32", "load8s"] => Ok(Lang::I32Load8S {
                static_offset,
                align,
                mem,
                offset: children[0],
            }),
            ["i32", "load8u"] => Ok(Lang::I32Load8U {
                static_offset,
                align,
                mem,
                offset: children[0],
            }),
            ["i32", "load16s"] => Ok(Lang::I32Load16S {
                static_offset,
                align,
                mem,
                offset: children[0],
            }),
            ["i32", "load16u"] => Ok(Lang::I32Load16U {
                static_offset,
                align,
                mem,
                offset: children[0],
            }),
            ["i64", "load8s"] => Ok(Lang::I64Load8S {
                static_offset,
                align,
                mem,
                offset: children[0],
            }),
            ["i64", "load8u"] => Ok(Lang::I64Load8U {
                static_offset,
                align,
                mem,
                offset: children[0],
            }),
            ["i64", "load16s"] => Ok(Lang::I64Load16S {
                static_offset,
                align,
                mem,
                offset: children[0],
            }),
            ["i64", "load16u"] => Ok(Lang::I64Load16U {
                static_offset,
                align,
                mem,
                offset: children[0],
            }),
            ["i64", "load32s"] => Ok(Lang::I64Load32S {
                static_offset,
                align,
                mem,
                offset: children[0],
            }),
            ["i64", "load32u"] => Ok(Lang::I64Load32U {
                static_offset,
                align,
                mem,
                offset: children[0],
            }),
            ["i32", "store"] => Ok(Lang::I32Store {
                static_offset,
                align,
                mem,
                value_and_offset: [children[1], children[0]],
            }),
            ["i64", "store"] => Ok(Lang::I64Store {
                static_offset,
                align,
                mem,
                value_and_offset: [children[1], children[0]],
            }),
            ["f32", "store"] => Ok(Lang::F32Store {
                static_offset,
                align,
                mem,
                value_and_offset: [children[1], children[0]],
            }),
            ["f64", "store"] => Ok(Lang::F64Store {
                static_offset,
                align,
                mem,
                value_and_offset: [children[1], children[0]],
            }),
            ["i32", "store8"] => Ok(Lang::I32Store8 {
                static_offset,
                align,
                mem,
                value_and_offset: [children[1], children[0]],
            }),
            ["i32", "store16"] => Ok(Lang::I32Store16 {
                static_offset,
                align,
                mem,
                value_and_offset: [children[1], children[0]],
            }),
            ["i64", "store8"] => Ok(Lang::I64Store8 {
                static_offset,
                align,
                mem,
                value_and_offset: [children[1], children[0]],
            }),
            ["i64", "store16"] => Ok(Lang::I64Store16 {
                static_offset,
                align,
                mem,
                value_and_offset: [children[1], children[0]],
            }),
            ["i64", "store32"] => Ok(Lang::I64Store32 {
                static_offset,
                align,
                mem,
                value_and_offset: [children[1], children[0]],
            }),
            _ => Err(format!("Invalid index based operation {:?}", op_str)),
        }
    }

    /// Parses call operators: `call.$i`
    pub fn parse_call(op_str: &str, children: &Vec<Id>) -> Result<Self, String> {
        let splat = op_str.split('.');
        let ops = splat.collect::<Vec<_>>();
        if ops.len() != 2 {
            return Err(format!("Invalid call operation {}", op_str));
        }
        let index = usize::from_str(ops[1]).expect("Invlid function index");

        match ops[0] {
            "call" => Ok(Lang::Call(index, children.clone())),
            _ => Err(format!("Invalid call operation {:?}", op_str)),
        }
    }

    /// Parses memory grow or size operator: `memory.(grow|size).$mem.$membyte`
    pub fn parse_memory_sg(op_str: &str, children: &Vec<Id>) -> Result<Self, String> {
        let splat = op_str.split('.');
        let ops = splat.collect::<Vec<_>>();
        if ops.len() != 4 {
            return Err(format!("Invalid memory (grow|size) operator {}", op_str));
        }

        // In theory indices can have eclasses as well
        // If we want to have index-change-like mutators
        let mem = u32::from_str(ops[2]).unwrap();
        let mem_byte = u8::from_str(ops[3]).unwrap();
        match &ops[..2] {
            ["memory", "grow"] => Ok(Lang::MemoryGrow {
                mem,
                mem_byte,
                by: children[0],
            }),
            ["memory", "size"] => Ok(Lang::MemorySize { mem, mem_byte }),
            _ => Err(format!("Invalid index based operation {:?}", op_str)),
        }
    }
}

// To match memory like nodes by its inmediates
macro_rules! match_mem {
    ($l:ident, $self: ident, $other: ident) => {
        if let (
            Lang::$l {
                static_offset,
                align,
                mem,
                ..
            },
            Lang::$l {
                static_offset: static_offset2,
                align: align2,
                mem: mem2,
                ..
            },
        ) = ($self, $other)
        {
            return static_offset == static_offset2 && align == align2 && mem == mem2;
        }
    };
}

impl egg::Language for Lang {
    fn matches(&self, other: &Self) -> bool {
        match_mem!(I32Load, self, other);
        match_mem!(I64Load, self, other);
        match_mem!(F32Load, self, other);
        match_mem!(F64Load, self, other);
        match_mem!(I32Load8S, self, other);
        match_mem!(I32Load8U, self, other);
        match_mem!(I32Load16S, self, other);
        match_mem!(I32Load16U, self, other);
        match_mem!(I64Load8S, self, other);
        match_mem!(I64Load8U, self, other);
        match_mem!(I64Load16S, self, other);
        match_mem!(I64Load16U, self, other);
        match_mem!(I64Load32S, self, other);
        match_mem!(I64Load32U, self, other);
        match_mem!(I32Store, self, other);
        match_mem!(I64Store, self, other);
        match_mem!(F32Store, self, other);
        match_mem!(F64Store, self, other);
        match_mem!(I32Store8, self, other);
        match_mem!(I32Store16, self, other);
        match_mem!(I64Store8, self, other);
        match_mem!(I64Store16, self, other);
        match_mem!(I64Store32, self, other);

        match (self, other) {
            (Lang::I32(v), Lang::I32(v2)) => v == v2,
            (Lang::I64(v), Lang::I64(v2)) => v == v2,
            (Lang::F32(v), Lang::F32(v2)) => v == v2,
            (Lang::F64(v), Lang::F64(v2)) => v == v2,
            (Lang::GlobalGet(v), Lang::GlobalGet(v2)) => v == v2,
            (Lang::LocalGet(v), Lang::LocalGet(v2)) => v == v2,
            (Lang::GlobalSet(v, _), Lang::GlobalSet(v2, _))
            | (Lang::LocalSet(v, _), Lang::LocalSet(v2, _))
            | (Lang::LocalTee(v, _), Lang::LocalTee(v2, _)) => v == v2,
            (Lang::Call(v, _), Lang::Call(v2, _)) => v == v2,
            (
                Lang::MemoryGrow { mem_byte, mem, .. },
                Lang::MemoryGrow {
                    mem_byte: mem_byte2,
                    mem: mem2,
                    ..
                },
            ) => mem == mem2 && mem_byte == mem_byte2,
            (
                Lang::MemorySize { mem_byte, mem },
                Lang::MemorySize {
                    mem_byte: mem_byte2,
                    mem: mem2,
                },
            ) => mem == mem2 && mem_byte == mem_byte2,
            (Lang::Container(v), Lang::Container(v2)) => v.len() == v2.len(),
            _ => ::std::mem::discriminant(self) == ::std::mem::discriminant(other),
        }
    }

    fn children(&self) -> &[Id] {
        match &self {
            // binops
            Lang::I64Add(operands)
            | Lang::I32Sub(operands)
            | Lang::I64Sub(operands)
            | Lang::I32Mul(operands)
            | Lang::I64Mul(operands)
            | Lang::I32And(operands)
            | Lang::I64And(operands)
            | Lang::I32Or(operands)
            | Lang::I64Or(operands)
            | Lang::I32Xor(operands)
            | Lang::I64Xor(operands)
            | Lang::I32Shl(operands)
            | Lang::I64Shl(operands)
            | Lang::I32ShrU(operands)
            | Lang::I64ShrU(operands)
            | Lang::I32DivU(operands)
            | Lang::I64DivU(operands)
            | Lang::I32DivS(operands)
            | Lang::I64DivS(operands)
            | Lang::I32ShrS(operands)
            | Lang::I64ShrS(operands)
            | Lang::I32RotR(operands)
            | Lang::I64RotR(operands)
            | Lang::I32RotL(operands)
            | Lang::I64RotL(operands)
            | Lang::I32RemS(operands)
            | Lang::I64RemS(operands)
            | Lang::I32RemU(operands)
            | Lang::I64RemU(operands)
            | Lang::I32Eq(operands)
            | Lang::I64Eq(operands)
            | Lang::I32Ne(operands)
            | Lang::I64Ne(operands)
            | Lang::I32LtS(operands)
            | Lang::I64LtS(operands)
            | Lang::I32LtU(operands)
            | Lang::I64LtU(operands)
            | Lang::I32GtS(operands)
            | Lang::I64GtS(operands)
            | Lang::I32GtU(operands)
            | Lang::I64GtU(operands)
            | Lang::I32LeS(operands)
            | Lang::I64LeS(operands)
            | Lang::I32LeU(operands)
            | Lang::I64LeU(operands)
            | Lang::I32GeS(operands)
            | Lang::I64GeS(operands)
            | Lang::I32GeU(operands)
            | Lang::I64GeU(operands)
            | Lang::I32Add(operands)
            | Lang::F32Add(operands)
            | Lang::F64Add(operands)
            | Lang::F32Sub(operands)
            | Lang::F64Sub(operands)
            | Lang::F32Mul(operands)
            | Lang::F64Mul(operands)
            | Lang::F32Div(operands)
            | Lang::F64Div(operands)
            | Lang::F32Min(operands)
            | Lang::F64Min(operands)
            | Lang::F32Max(operands)
            | Lang::F64Max(operands)
            | Lang::F32Copysign(operands)
            | Lang::F64Copysign(operands)
            | Lang::F32Eq(operands)
            | Lang::F64Eq(operands)
            | Lang::F32Ne(operands)
            | Lang::F64Ne(operands)
            | Lang::F32Lt(operands)
            | Lang::F64Lt(operands)
            | Lang::F32Gt(operands)
            | Lang::F64Gt(operands)
            | Lang::F32Le(operands)
            | Lang::F64Le(operands)
            | Lang::F32Ge(operands)
            | Lang::F64Ge(operands) => operands,
            // unops
            Lang::Drop(operands)
            | Lang::I32Extend8S(operands)
            | Lang::I64Extend8S(operands)
            | Lang::I32Extend16S(operands)
            | Lang::I64Extend16S(operands)
            | Lang::I64Extend32S(operands)
            | Lang::I64ExtendI32S(operands)
            | Lang::I64ExtendI32U(operands)
            | Lang::I64Popcnt(operands)
            | Lang::I32Eqz(operands)
            | Lang::I64Eqz(operands)
            | Lang::I32Popcnt(operands)
            | Lang::I32Clz(operands)
            | Lang::I32Ctz(operands)
            | Lang::I64Ctz(operands)
            | Lang::I64Clz(operands)
            | Lang::F32Abs(operands)
            | Lang::F64Abs(operands)
            | Lang::F32Neg(operands)
            | Lang::F64Neg(operands)
            | Lang::F32Sqrt(operands)
            | Lang::F64Sqrt(operands)
            | Lang::F32Ceil(operands)
            | Lang::F64Ceil(operands)
            | Lang::F32Floor(operands)
            | Lang::F64Floor(operands)
            | Lang::F32Trunc(operands)
            | Lang::F64Trunc(operands)
            | Lang::F32Nearest(operands)
            | Lang::F64Nearest(operands)
            | Lang::I32TruncF32S(operands)
            | Lang::I32TruncF32U(operands)
            | Lang::I32TruncF64S(operands)
            | Lang::I32TruncF64U(operands)
            | Lang::I64TruncF32S(operands)
            | Lang::I64TruncF32U(operands)
            | Lang::I64TruncF64S(operands)
            | Lang::I64TruncF64U(operands)
            | Lang::F32ConvertI32S(operands)
            | Lang::F32ConvertI32U(operands)
            | Lang::F32ConvertI64S(operands)
            | Lang::F32ConvertI64U(operands)
            | Lang::F32DemoteF64(operands)
            | Lang::F64ConvertI32S(operands)
            | Lang::F64ConvertI32U(operands)
            | Lang::F64ConvertI64S(operands)
            | Lang::F64ConvertI64U(operands)
            | Lang::F64PromoteF32(operands)
            | Lang::I32ReinterpretF32(operands)
            | Lang::I64ReinterpretF64(operands)
            | Lang::F32ReinterpretI32(operands)
            | Lang::F64ReinterpretI64(operands)
            | Lang::I32TruncSatF32S(operands)
            | Lang::I32TruncSatF32U(operands)
            | Lang::I32TruncSatF64S(operands)
            | Lang::I32TruncSatF64U(operands)
            | Lang::I64TruncSatF32S(operands)
            | Lang::I64TruncSatF32U(operands)
            | Lang::I64TruncSatF64S(operands)
            | Lang::I64TruncSatF64U(operands) => operands,
            Lang::GlobalSet(_, val) | Lang::LocalTee(_, val) => std::slice::from_ref(val),
            Lang::LocalSet(_, val) => std::slice::from_ref(val),
            Lang::LocalGet(_) => &[],
            Lang::GlobalGet(_) => &[],
            Lang::Wrap(operands) => operands,
            Lang::Call(_, operands) => operands,
            Lang::I32Load {
                offset,
                static_offset: _,
                align: _,
                mem: _,
            }
            | Lang::I64Load {
                offset,
                static_offset: _,
                align: _,
                mem: _,
            }
            | Lang::F32Load {
                static_offset: _,
                align: _,
                mem: _,
                offset,
            }
            | Lang::F64Load {
                static_offset: _,
                align: _,
                mem: _,
                offset,
            }
            | Lang::I32Load8S {
                static_offset: _,
                align: _,
                mem: _,
                offset,
            }
            | Lang::I32Load8U {
                static_offset: _,
                align: _,
                mem: _,
                offset,
            }
            | Lang::I32Load16S {
                static_offset: _,
                align: _,
                mem: _,
                offset,
            }
            | Lang::I32Load16U {
                static_offset: _,
                align: _,
                mem: _,
                offset,
            }
            | Lang::I64Load8S {
                static_offset: _,
                align: _,
                mem: _,
                offset,
            }
            | Lang::I64Load8U {
                static_offset: _,
                align: _,
                mem: _,
                offset,
            }
            | Lang::I64Load16S {
                static_offset: _,
                align: _,
                mem: _,
                offset,
            }
            | Lang::I64Load16U {
                static_offset: _,
                align: _,
                mem: _,
                offset,
            }
            | Lang::I64Load32S {
                static_offset: _,
                align: _,
                mem: _,
                offset,
            }
            | Lang::I64Load32U {
                static_offset: _,
                align: _,
                mem: _,
                offset,
            } => std::slice::from_ref(offset),
            Lang::I32Store {
                value_and_offset,
                static_offset: _,
                align: _,
                mem: _,
            }
            | Lang::I64Store {
                value_and_offset,
                static_offset: _,
                align: _,
                mem: _,
            }
            | Lang::F32Store {
                value_and_offset,
                static_offset: _,
                align: _,
                mem: _,
            }
            | Lang::F64Store {
                value_and_offset,
                static_offset: _,
                align: _,
                mem: _,
            }
            | Lang::I32Store8 {
                value_and_offset,
                static_offset: _,
                align: _,
                mem: _,
            }
            | Lang::I32Store16 {
                value_and_offset,
                static_offset: _,
                align: _,
                mem: _,
            }
            | Lang::I64Store8 {
                value_and_offset,
                static_offset: _,
                align: _,
                mem: _,
            }
            | Lang::I64Store16 {
                value_and_offset,
                static_offset: _,
                align: _,
                mem: _,
            }
            | Lang::I64Store32 {
                value_and_offset,
                static_offset: _,
                align: _,
                mem: _,
            } => value_and_offset,
            Lang::RandI32 => &[],
            Lang::RandI64 => &[],
            Lang::Undef => &[],
            Lang::UnfoldI32(operand) | Lang::UnfoldI64(operand) => std::slice::from_ref(operand),
            Lang::I32(_) => &[],
            Lang::I64(_) => &[],
            Lang::F32(_) => &[],
            Lang::F64(_) => &[],
            Lang::Nop => &[],
            Lang::Container(operands) => operands,
            Lang::Select(operands) => operands,
            Lang::MemoryGrow { by, .. } => std::slice::from_ref(by),
            Lang::MemorySize { .. } => &[],
            Lang::I32UseGlobal(arg)
            | Lang::I64UseGlobal(arg)
            | Lang::F32UseGlobal(arg)
            | Lang::F64UseGlobal(arg) => std::slice::from_ref(arg),
        }
    }

    fn children_mut(&mut self) -> &mut [Id] {
        match self {
            Lang::I64Add(operands)
            | Lang::I32Sub(operands)
            | Lang::I64Sub(operands)
            | Lang::I32Mul(operands)
            | Lang::I64Mul(operands)
            | Lang::I32And(operands)
            | Lang::I64And(operands)
            | Lang::I32Or(operands)
            | Lang::I64Or(operands)
            | Lang::I32Xor(operands)
            | Lang::I64Xor(operands)
            | Lang::I32Shl(operands)
            | Lang::I64Shl(operands)
            | Lang::I32ShrU(operands)
            | Lang::I64ShrU(operands)
            | Lang::I32DivU(operands)
            | Lang::I64DivU(operands)
            | Lang::I32DivS(operands)
            | Lang::I64DivS(operands)
            | Lang::I32ShrS(operands)
            | Lang::I64ShrS(operands)
            | Lang::I32RotR(operands)
            | Lang::I64RotR(operands)
            | Lang::I32RotL(operands)
            | Lang::I64RotL(operands)
            | Lang::I32RemS(operands)
            | Lang::I64RemS(operands)
            | Lang::I32RemU(operands)
            | Lang::I64RemU(operands)
            | Lang::I32Eq(operands)
            | Lang::I64Eq(operands)
            | Lang::I32Ne(operands)
            | Lang::I64Ne(operands)
            | Lang::I32LtS(operands)
            | Lang::I64LtS(operands)
            | Lang::I32LtU(operands)
            | Lang::I64LtU(operands)
            | Lang::I32GtS(operands)
            | Lang::I64GtS(operands)
            | Lang::I32GtU(operands)
            | Lang::I64GtU(operands)
            | Lang::I32LeS(operands)
            | Lang::I64LeS(operands)
            | Lang::I32LeU(operands)
            | Lang::I64LeU(operands)
            | Lang::I32GeS(operands)
            | Lang::I64GeS(operands)
            | Lang::I32GeU(operands)
            | Lang::I64GeU(operands)
            | Lang::I32Add(operands)
            | Lang::F32Add(operands)
            | Lang::F64Add(operands)
            | Lang::F32Sub(operands)
            | Lang::F64Sub(operands)
            | Lang::F32Mul(operands)
            | Lang::F64Mul(operands)
            | Lang::F32Div(operands)
            | Lang::F64Div(operands)
            | Lang::F32Min(operands)
            | Lang::F64Min(operands)
            | Lang::F32Max(operands)
            | Lang::F64Max(operands)
            | Lang::F32Copysign(operands)
            | Lang::F64Copysign(operands)
            | Lang::F32Eq(operands)
            | Lang::F64Eq(operands)
            | Lang::F32Ne(operands)
            | Lang::F64Ne(operands)
            | Lang::F32Lt(operands)
            | Lang::F64Lt(operands)
            | Lang::F32Gt(operands)
            | Lang::F64Gt(operands)
            | Lang::F32Le(operands)
            | Lang::F64Le(operands)
            | Lang::F32Ge(operands)
            | Lang::F64Ge(operands) => operands,

            Lang::UnfoldI32(val) | Lang::UnfoldI64(val) | Lang::LocalTee(_, val) => {
                std::slice::from_mut(val)
            }
            Lang::GlobalSet(_, val) | Lang::LocalSet(_, val) => std::slice::from_mut(val),
            Lang::LocalGet(_) => &mut [],
            Lang::GlobalGet(_) => &mut [],
            Lang::Drop(operands)
            | Lang::I32Popcnt(operands)
            | Lang::I64Popcnt(operands)
            | Lang::I32Eqz(operands)
            | Lang::I64Eqz(operands)
            | Lang::I32Clz(operands)
            | Lang::I32Ctz(operands)
            | Lang::I64Ctz(operands)
            | Lang::I64Clz(operands)
            | Lang::F32Abs(operands)
            | Lang::F64Abs(operands)
            | Lang::F32Neg(operands)
            | Lang::F64Neg(operands)
            | Lang::F32Sqrt(operands)
            | Lang::F64Sqrt(operands)
            | Lang::F32Ceil(operands)
            | Lang::F64Ceil(operands)
            | Lang::F32Floor(operands)
            | Lang::F64Floor(operands)
            | Lang::F32Trunc(operands)
            | Lang::F64Trunc(operands)
            | Lang::F32Nearest(operands)
            | Lang::F64Nearest(operands)
            | Lang::I32TruncF32S(operands)
            | Lang::I32TruncF32U(operands)
            | Lang::I32TruncF64S(operands)
            | Lang::I32TruncF64U(operands)
            | Lang::I64TruncF32S(operands)
            | Lang::I64TruncF32U(operands)
            | Lang::I64TruncF64S(operands)
            | Lang::I64TruncF64U(operands)
            | Lang::F32ConvertI32S(operands)
            | Lang::F32ConvertI32U(operands)
            | Lang::F32ConvertI64S(operands)
            | Lang::F32ConvertI64U(operands)
            | Lang::F32DemoteF64(operands)
            | Lang::F64ConvertI32S(operands)
            | Lang::F64ConvertI32U(operands)
            | Lang::F64ConvertI64S(operands)
            | Lang::F64ConvertI64U(operands)
            | Lang::F64PromoteF32(operands)
            | Lang::I32ReinterpretF32(operands)
            | Lang::I64ReinterpretF64(operands)
            | Lang::F32ReinterpretI32(operands)
            | Lang::F64ReinterpretI64(operands)
            | Lang::I32TruncSatF32S(operands)
            | Lang::I32TruncSatF32U(operands)
            | Lang::I32TruncSatF64S(operands)
            | Lang::I32TruncSatF64U(operands)
            | Lang::I64TruncSatF32S(operands)
            | Lang::I64TruncSatF32U(operands)
            | Lang::I64TruncSatF64S(operands)
            | Lang::I64TruncSatF64U(operands)
            | Lang::I32Extend8S(operands)
            | Lang::I64Extend8S(operands)
            | Lang::I32Extend16S(operands)
            | Lang::I64Extend16S(operands)
            | Lang::I64Extend32S(operands)
            | Lang::I64ExtendI32S(operands)
            | Lang::I64ExtendI32U(operands)
            | Lang::Wrap(operands) => operands,
            Lang::Call(_, operands) => operands,
            Lang::I32Load {
                offset,
                static_offset: _,
                align: _,
                mem: _,
            }
            | Lang::I64Load {
                offset,
                static_offset: _,
                align: _,
                mem: _,
            }
            | Lang::F32Load {
                static_offset: _,
                align: _,
                mem: _,
                offset,
            }
            | Lang::F64Load {
                static_offset: _,
                align: _,
                mem: _,
                offset,
            }
            | Lang::I32Load8S {
                static_offset: _,
                align: _,
                mem: _,
                offset,
            }
            | Lang::I32Load8U {
                static_offset: _,
                align: _,
                mem: _,
                offset,
            }
            | Lang::I32Load16S {
                static_offset: _,
                align: _,
                mem: _,
                offset,
            }
            | Lang::I32Load16U {
                static_offset: _,
                align: _,
                mem: _,
                offset,
            }
            | Lang::I64Load8S {
                static_offset: _,
                align: _,
                mem: _,
                offset,
            }
            | Lang::I64Load8U {
                static_offset: _,
                align: _,
                mem: _,
                offset,
            }
            | Lang::I64Load16S {
                static_offset: _,
                align: _,
                mem: _,
                offset,
            }
            | Lang::I64Load16U {
                static_offset: _,
                align: _,
                mem: _,
                offset,
            }
            | Lang::I64Load32S {
                static_offset: _,
                align: _,
                mem: _,
                offset,
            }
            | Lang::I64Load32U {
                static_offset: _,
                align: _,
                mem: _,
                offset,
            } => std::slice::from_mut(offset),
            Lang::I32Store {
                value_and_offset,
                static_offset: _,
                align: _,
                mem: _,
            }
            | Lang::I64Store {
                value_and_offset,
                static_offset: _,
                align: _,
                mem: _,
            }
            | Lang::F32Store {
                value_and_offset,
                static_offset: _,
                align: _,
                mem: _,
            }
            | Lang::F64Store {
                value_and_offset,
                static_offset: _,
                align: _,
                mem: _,
            }
            | Lang::I32Store8 {
                value_and_offset,
                static_offset: _,
                align: _,
                mem: _,
            }
            | Lang::I32Store16 {
                value_and_offset,
                static_offset: _,
                align: _,
                mem: _,
            }
            | Lang::I64Store8 {
                value_and_offset,
                static_offset: _,
                align: _,
                mem: _,
            }
            | Lang::I64Store16 {
                value_and_offset,
                static_offset: _,
                align: _,
                mem: _,
            }
            | Lang::I64Store32 {
                value_and_offset,
                static_offset: _,
                align: _,
                mem: _,
            } => value_and_offset,
            Lang::RandI32 => &mut [],
            Lang::RandI64 => &mut [],
            Lang::Undef => &mut [],
            Lang::I32(_) => &mut [],
            Lang::I64(_) => &mut [],
            Lang::F32(_) => &mut [],
            Lang::F64(_) => &mut [],
            Lang::Nop => &mut [],
            Lang::Container(operands) => operands,
            Lang::Select(operands) => operands,
            Lang::MemoryGrow {
                mem: _,
                mem_byte: _,
                by,
            } => std::slice::from_mut(by),
            Lang::MemorySize {
                mem: _,
                mem_byte: _,
            } => &mut [],
            Lang::I32UseGlobal(arg)
            | Lang::I64UseGlobal(arg)
            | Lang::F32UseGlobal(arg)
            | Lang::F64UseGlobal(arg) => std::slice::from_mut(arg),
        }
    }

    fn for_each<F: FnMut(Id)>(&self, f: F) {
        self.children().iter().copied().for_each(f)
    }

    fn for_each_mut<F: FnMut(&mut Id)>(&mut self, f: F) {
        self.children_mut().iter_mut().for_each(f)
    }

    fn display_op(&self) -> &dyn std::fmt::Display {
        self
    }

    fn from_op_str(op_str: &str, children: Vec<Id>) -> Result<Self, String> {
        match op_str {
            // binops
            "i32.add" => Ok(Lang::I32Add([children[0], children[1]])),
            "i64.add" => Ok(Lang::I64Add([children[0], children[1]])),
            "i32.sub" => Ok(Lang::I32Sub([children[0], children[1]])),
            "i64.sub" => Ok(Lang::I64Sub([children[0], children[1]])),
            "i32.mul" => Ok(Lang::I32Mul([children[0], children[1]])),
            "i64.mul" => Ok(Lang::I64Mul([children[0], children[1]])),
            "i32.and" => Ok(Lang::I32And([children[0], children[1]])),
            "i64.and" => Ok(Lang::I64And([children[0], children[1]])),
            "i32.or" => Ok(Lang::I32Or([children[0], children[1]])),
            "i64.or" => Ok(Lang::I64Or([children[0], children[1]])),
            "i32.xor" => Ok(Lang::I32Xor([children[0], children[1]])),
            "i64.xor" => Ok(Lang::I64Xor([children[0], children[1]])),
            "i32.shl" => Ok(Lang::I32Shl([children[0], children[1]])),
            "i64.shl" => Ok(Lang::I64Shl([children[0], children[1]])),
            "i32.shr_u" => Ok(Lang::I32ShrU([children[0], children[1]])),
            "i64.shr_u" => Ok(Lang::I64ShrU([children[0], children[1]])),
            "i32.div_u" => Ok(Lang::I32DivU([children[0], children[1]])),
            "i64.div_u" => Ok(Lang::I64DivU([children[0], children[1]])),
            "i32.div_s" => Ok(Lang::I32DivS([children[0], children[1]])),
            "i64.div_s" => Ok(Lang::I64DivS([children[0], children[1]])),
            "i32.shr_s" => Ok(Lang::I32ShrS([children[0], children[1]])),
            "i64.shr_s" => Ok(Lang::I64ShrS([children[0], children[1]])),
            "i32.rotr" => Ok(Lang::I32RotR([children[0], children[1]])),
            "i64.rotr" => Ok(Lang::I64RotR([children[0], children[1]])),
            "i32.rotl" => Ok(Lang::I32RotL([children[0], children[1]])),
            "i64.rotl" => Ok(Lang::I64RotL([children[0], children[1]])),
            "i32.rem_s" => Ok(Lang::I32RemS([children[0], children[1]])),
            "i64.rem_s" => Ok(Lang::I64RemS([children[0], children[1]])),
            "i32.rem_u" => Ok(Lang::I32RemU([children[0], children[1]])),
            "i64.rem_u" => Ok(Lang::I64RemU([children[0], children[1]])),
            "i32.eq" => Ok(Lang::I32Eq([children[0], children[1]])),
            "i64.eq" => Ok(Lang::I64Eq([children[0], children[1]])),
            "i32.ne" => Ok(Lang::I32Ne([children[0], children[1]])),
            "i64.ne" => Ok(Lang::I64Ne([children[0], children[1]])),
            "i32.lt_s" => Ok(Lang::I32LtS([children[0], children[1]])),
            "i64.lt_s" => Ok(Lang::I64LtS([children[0], children[1]])),
            "i32.lt_u" => Ok(Lang::I32LtU([children[0], children[1]])),
            "i64.lt_u" => Ok(Lang::I64LtU([children[0], children[1]])),
            "i32.gt_s" => Ok(Lang::I32GtS([children[0], children[1]])),
            "i64.gt_s" => Ok(Lang::I64GtS([children[0], children[1]])),
            "i32.gt_u" => Ok(Lang::I32GtU([children[0], children[1]])),
            "i64.gt_u" => Ok(Lang::I64GtU([children[0], children[1]])),
            "i32.le_s" => Ok(Lang::I32LeS([children[0], children[1]])),
            "i64.le_s" => Ok(Lang::I64LeS([children[0], children[1]])),
            "i32.le_u" => Ok(Lang::I32LeU([children[0], children[1]])),
            "i64.le_u" => Ok(Lang::I64LeU([children[0], children[1]])),
            "i32.ge_s" => Ok(Lang::I32GeS([children[0], children[1]])),
            "i64.ge_s" => Ok(Lang::I64GeS([children[0], children[1]])),
            "i32.ge_u" => Ok(Lang::I32GeU([children[0], children[1]])),
            "i64.ge_u" => Ok(Lang::I64GeU([children[0], children[1]])),
            // binops floats
            "f32.add" => Ok(Lang::F32Add([children[0], children[1]])),
            "f64.add" => Ok(Lang::F64Add([children[0], children[1]])),
            "f32.sub" => Ok(Lang::F32Sub([children[0], children[1]])),
            "f64.sub" => Ok(Lang::F64Sub([children[0], children[1]])),
            "f32.mul" => Ok(Lang::F32Mul([children[0], children[1]])),
            "f64.mul" => Ok(Lang::F64Mul([children[0], children[1]])),
            "f32.div" => Ok(Lang::F32Div([children[0], children[1]])),
            "f64.div" => Ok(Lang::F64Div([children[0], children[1]])),
            "f32.min" => Ok(Lang::F32Min([children[0], children[1]])),
            "f64.min" => Ok(Lang::F64Min([children[0], children[1]])),
            "f32.max" => Ok(Lang::F32Max([children[0], children[1]])),
            "f64.max" => Ok(Lang::F64Max([children[0], children[1]])),
            "f32.copysign" => Ok(Lang::F32Copysign([children[0], children[1]])),
            "f64.copysign" => Ok(Lang::F64Copysign([children[0], children[1]])),
            // frelops
            "f32.eq" => Ok(Lang::F32Eq([children[0], children[1]])),
            "f64.eq" => Ok(Lang::F64Eq([children[0], children[1]])),
            "f32.ne" => Ok(Lang::F32Ne([children[0], children[1]])),
            "f64.ne" => Ok(Lang::F64Ne([children[0], children[1]])),
            "f32.lt" => Ok(Lang::F32Lt([children[0], children[1]])),
            "f64.lt" => Ok(Lang::F64Lt([children[0], children[1]])),
            "f32.gt" => Ok(Lang::F32Gt([children[0], children[1]])),
            "f64.gt" => Ok(Lang::F64Gt([children[0], children[1]])),
            "f32.le" => Ok(Lang::F32Le([children[0], children[1]])),
            "f64.le" => Ok(Lang::F64Le([children[0], children[1]])),
            "f32.ge" => Ok(Lang::F32Ge([children[0], children[1]])),
            "f64.ge" => Ok(Lang::F64Ge([children[0], children[1]])),
            //unop
            "i64.eqz" => Ok(Lang::I64Eqz([children[0]])),
            "i32.eqz" => Ok(Lang::I32Eqz([children[0]])),

            "i32.popcnt" => Ok(Lang::I32Popcnt([children[0]])),
            "i64.popcnt" => Ok(Lang::I64Popcnt([children[0]])),
            "i32.clz" => Ok(Lang::I32Clz([children[0]])),
            "i32.ctz" => Ok(Lang::I32Ctz([children[0]])),
            "i64.ctz" => Ok(Lang::I64Ctz([children[0]])),
            "i64.clz" => Ok(Lang::I64Clz([children[0]])),

            // unops floats
            "f32.abs" => Ok(Lang::F32Abs([children[0]])),
            "f64.abs" => Ok(Lang::F64Abs([children[0]])),
            "f32.neg" => Ok(Lang::F32Neg([children[0]])),
            "f64.neg" => Ok(Lang::F64Neg([children[0]])),
            "f32.sqrt" => Ok(Lang::F32Sqrt([children[0]])),
            "f64.sqrt" => Ok(Lang::F64Sqrt([children[0]])),
            "f32.ceil" => Ok(Lang::F32Ceil([children[0]])),
            "f64.ceil" => Ok(Lang::F64Ceil([children[0]])),
            "f32.floor" => Ok(Lang::F32Floor([children[0]])),
            "f64.floor" => Ok(Lang::F64Floor([children[0]])),
            "f32.trunc" => Ok(Lang::F32Trunc([children[0]])),
            "f64.trunc" => Ok(Lang::F64Trunc([children[0]])),
            "f32.nearest" => Ok(Lang::F32Nearest([children[0]])),
            "f64.nearest" => Ok(Lang::F64Nearest([children[0]])),
            // more conversion
            "i32.extend8_s" => Ok(Lang::I32Extend8S([children[0]])),
            "i64.extend8_s" => Ok(Lang::I64Extend8S([children[0]])),
            "i32.extend16_s" => Ok(Lang::I32Extend16S([children[0]])),
            "i64.extend16_s" => Ok(Lang::I64Extend16S([children[0]])),
            "i64.extend32_s" => Ok(Lang::I64Extend32S([children[0]])),
            "i64.extendi32_s" => Ok(Lang::I64ExtendI32S([children[0]])),
            "i64.extendi32_u" => Ok(Lang::I64ExtendI32U([children[0]])),
            "wrap" => Ok(Lang::Wrap([children[0]])),
            "i32.truncf32_s" => Ok(Lang::I32TruncF32S([children[0]])),
            "i32.truncf32_u" => Ok(Lang::I32TruncF32U([children[0]])),
            "i32.truncf64_s" => Ok(Lang::I32TruncF64S([children[0]])),
            "i32.truncf64_u" => Ok(Lang::I32TruncF64U([children[0]])),
            "i64.truncf32_s" => Ok(Lang::I64TruncF32S([children[0]])),
            "i64.truncf32_u" => Ok(Lang::I64TruncF32U([children[0]])),
            "i64.truncf64_s" => Ok(Lang::I64TruncF64S([children[0]])),
            "i64.truncf64_u" => Ok(Lang::I64TruncF64U([children[0]])),
            "f32.converti32_s" => Ok(Lang::F32ConvertI32S([children[0]])),
            "f32.converti32_u" => Ok(Lang::F32ConvertI32U([children[0]])),
            "f32.converti64_s" => Ok(Lang::F32ConvertI64S([children[0]])),
            "f32.converti64_u" => Ok(Lang::F32ConvertI64U([children[0]])),
            "f32.demotef64" => Ok(Lang::F32DemoteF64([children[0]])),
            "f64.converti32_s" => Ok(Lang::F64ConvertI32S([children[0]])),
            "f64.converti32_u" => Ok(Lang::F64ConvertI32U([children[0]])),
            "f64.converti64_s" => Ok(Lang::F64ConvertI64S([children[0]])),
            "f64.converti64_u" => Ok(Lang::F64ConvertI64U([children[0]])),
            "f64.promotef32" => Ok(Lang::F64PromoteF32([children[0]])),
            "i32.reinterpretf32" => Ok(Lang::I32ReinterpretF32([children[0]])),
            "i64.reinterpretf64" => Ok(Lang::I64ReinterpretF64([children[0]])),
            "f32.reinterpreti32" => Ok(Lang::F32ReinterpretI32([children[0]])),
            "f64.reinterpreti64" => Ok(Lang::F64ReinterpretI64([children[0]])),
            "i32.truncsatf32_s" => Ok(Lang::I32TruncSatF32S([children[0]])),
            "i32.truncsatf32_u" => Ok(Lang::I32TruncSatF32U([children[0]])),
            "i32.truncsatf64_s" => Ok(Lang::I32TruncSatF64S([children[0]])),
            "i32.truncsatf64_u" => Ok(Lang::I32TruncSatF64U([children[0]])),
            "i64.truncsatf32_s" => Ok(Lang::I64TruncSatF32S([children[0]])),
            "i64.truncsatf32_u" => Ok(Lang::I64TruncSatF32U([children[0]])),
            "i64.truncsatf64_s" => Ok(Lang::I64TruncSatF64S([children[0]])),
            "i64.truncsatf64_u" => Ok(Lang::I64TruncSatF64U([children[0]])),
            // Special nodes :)
            "i32.unfold" => Ok(Lang::UnfoldI32(children[0])),
            "i64.unfold" => Ok(Lang::UnfoldI64(children[0])),
            "i32.rand" => Ok(Lang::RandI32),
            "i64.rand" => Ok(Lang::RandI64),
            "undef" => Ok(Lang::Undef),
            "drop" => Ok(Lang::Drop([children[0]])),
            "nop" => Ok(Lang::Nop),
            "container" => Ok(Lang::Container(children)),
            "select" => Ok(Lang::Select([children[0], children[1], children[2]])),
            "i32.use_of_global" => Ok(Lang::I32UseGlobal(children[0])),
            "i64.use_of_global" => Ok(Lang::I64UseGlobal(children[0])),
            "f32.use_of_global" => Ok(Lang::F32UseGlobal(children[0])),
            "f64.use_of_global" => Ok(Lang::F64UseGlobal(children[0])),
            _ => Lang::parse_call(op_str, &children)
                .or(Lang::parse_memory_sg(op_str, &children))
                .or(Lang::parse_mem_op(op_str, &children))
                .or(Lang::parse_index_op(op_str, &children))
                .or(Lang::parse_integer(op_str))
                .or(Err(format!("Invalid token {:?}", op_str))),
        }
    }

    fn len(&self) -> usize {
        self.children().len()
    }

    fn is_leaf(&self) -> bool {
        self.children().is_empty()
    }

    fn update_children<F: FnMut(Id) -> Id>(&mut self, mut f: F) {
        self.for_each_mut(|id| *id = f(*id))
    }

    fn map_children<F: FnMut(Id) -> Id>(mut self, f: F) -> Self {
        self.update_children(f);
        self
    }

    fn fold<F, T>(&self, init: T, mut f: F) -> T
    where
        F: FnMut(T, Id) -> T,
        T: Clone,
    {
        let mut acc = init;
        self.for_each(|id| acc = f(acc.clone(), id));
        acc
    }

    fn to_recexpr<'a, F>(&self, mut child_recexpr: F) -> egg::RecExpr<Self>
    where
        Self: 'a,
        F: FnMut(Id) -> &'a [Self],
    {
        fn build<L: egg::Language>(to: &mut egg::RecExpr<L>, from: &[L]) -> Id {
            let last = from.last().unwrap().clone();
            let new_node = last.map_children(|id| {
                let i = usize::from(id) + 1;
                build(to, &from[0..i])
            });
            to.add(new_node)
        }

        let mut expr = egg::RecExpr::default();
        let node = self
            .clone()
            .map_children(|id| build(&mut expr, child_recexpr(id)));
        expr.add(node);
        expr
    }
}

impl Default for Lang {
    fn default() -> Self {
        Lang::Undef
    }
}

#[cfg(test)]
mod tests {
    use egg::{Id, Language};

    use crate::mutators::peephole::eggsy::lang::Lang;

    #[test]
    fn test_parsing2() {
        let pairs = vec![
            [Lang::GlobalGet(0), Lang::GlobalGet(1)],
            [
                Lang::LocalSet(0, Id::from(0)),
                Lang::LocalSet(1, Id::from(1)),
            ],
            [Lang::LocalGet(0), Lang::LocalGet(1)],
            [
                Lang::GlobalSet(0, Id::from(0)),
                Lang::GlobalSet(1, Id::from(1)),
            ],
            [
                Lang::I32Load {
                    mem: 1,
                    align: 0,
                    static_offset: 120,
                    offset: Id::from(0),
                },
                Lang::I32Load {
                    mem: 1,
                    align: 0,
                    static_offset: 0,
                    offset: Id::from(0),
                },
            ],
            [Lang::LocalGet(0), Lang::LocalGet(1)],
        ];
        for [l, r] in pairs {
            assert_eq!(l.matches(&r), false);
        }
    }
}
