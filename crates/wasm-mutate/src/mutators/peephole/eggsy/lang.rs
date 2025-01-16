//! Nodes for intermediate representation of Wasm operators
//!
use egg::Id;
use std::fmt::{self, Display};
use std::str::FromStr;
use wasm_encoder::{AbstractHeapType, HeapType};

/// This is a macro used to define the `Lang` enum.
///
/// The goal of this macro is to synthesize an implementation of the
/// `egg::Language` trait which involves parsing strings, displaying operands,
/// determining whether operands are equal, and getting the child nodes of
/// operands. The macro here attempts to synthesize all these operations purely
/// from the structure of each variant. Some more details are below on the
/// definition of `enum Lang`.
///
/// It's recommended to probably gloss over this macro. Ideally you can follow
/// the pattern of all other existing variants and ignore this. If you can't
/// ignore this macro I can apologize in advance because it's a pretty gnarly
/// macro. While it seems to work it's "macro soup" incarnate.
macro_rules! lang {
    (
        $(#[$attr:meta])*
        pub enum $lang:ident {
            $(
                $(#[$docs:meta])*
                $case:ident $(($($inner:tt)*))? = $name:tt,
            )*
        }
    ) => {
        $(#[$attr])*
        pub enum $lang {
            $(
                $(#[$docs])*
                $case $(($($inner)*))?,
            )*
        }

        impl Display for $lang {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                match self {
                    $(
                        lang!(@first _x $lang::$case $(($($inner)*))?) => {
                            lang!(@fmt f _x $name $($($inner)*)?)
                        }
                    )*
                }
            }
        }

        impl egg::Language for Lang {
            fn matches(&self, other: &Self) -> bool {
                match (self, other) {
                    $(
                        (
                            lang!(@first _x $lang::$case $(($($inner)*))?),
                            lang!(@first _y $lang::$case $(($($inner)*))?),
                        ) => lang!(@matches _x _y $($($inner)*)?),
                    )*

                    _ => false,
                }
            }

            fn children(&self) -> &[Id] {
                match self {
                    $(
                        lang!(@last _x $lang::$case $(($($inner)*))?) => {
                            lang!(@children _x $($($inner)*)?)
                        }
                    )*
                }
            }

            fn children_mut(&mut self) -> &mut [Id] {
                match self {
                    $(
                        lang!(@last _x $lang::$case $(($($inner)*))?) => {
                            lang!(@children_mut _x $($($inner)*)?)
                        }
                    )*
                }
            }

            fn display_op(&self) -> &dyn std::fmt::Display {
                self
            }

            fn from_op_str(op_str: &str, children: Vec<Id>) -> Result<Self, String> {
                $(
                    lang!(@from_op_str op_str children $name $lang::$case $(($($inner)*))?);
                )*
                Err(format!("invalid token {:?}", op_str))
            }
        }
    };

    // Helper to generate a match-pattern that extracts the first variant as
    // $x, and with no types it matches nothing.
    (@first $x:ident $lang:ident :: $case:ident) => ($lang::$case);
    (@first $x:ident $lang:ident :: $case:ident ($($t:tt)* )) => ($lang::$case($x, ..));

    // Same as above, but matches the last variant. Note that for variants
    // with one type this will return that binding.
    (@last $x:ident $lang:ident :: $case:ident) => ($lang::$case);
    (@last $x:ident $lang:ident :: $case:ident ($($t:tt)* )) => ($lang::$case(.., $x));

    // Implementation of `children` and `children_mut`
    //
    // These largely delegate the `Children` trait to perform the actual
    // slicing, but only if the last argument of the binding (which is input
    // via `$x` here) looks like a child.
    (@children $x:ident $($t:tt)*) => (
        lang!(@if_last_is_child ($($t)*) { Children::as_slice($x) } else { &[] })
    );
    (@children_mut $x:ident $($t:tt)*) => (
        lang!(@if_last_is_child ($($t)*) { Children::as_slice_mut($x) } else { &mut [] })
    );

    // Implementation of `display_op`

    (@fmt $fmt:ident $payload:ident $opname:tt) => ($fmt.write_str($opname));
    (@fmt $fmt:ident $payload:ident $opname:tt $($t:tt)*) => (
        lang!(@if_first_is_child ($($t)*) {
            // If the first type argument is a child then this variant has no
            // payload so our string representation is simply the operation.
            $fmt.write_str($opname)
        } else {
            // ... otherwise if the first type, `$payload`, isn't a child then
            // it's part of the static payload and is included after the
            // operand with a `.`
            write!($fmt, "{}.{}", $opname, $payload)
        })
    );

    // Implementation of `matches`

    // special case for empty variants
    (@matches $x:ident $y:ident) => (true);
    // If the first element, which is bound by x/y, is a child then no equality
    // check is necessary, otherwise equality is performed.
    (@matches $x:ident $y:ident $($t:tt)*) => (
        lang!(@if_first_is_child ($($t)*) { true } else { $x == $y })
    );

    // Macro selection to condition on whether the last element of the tuple is
    // a list of children or not.

    // 2 types? Conventionally the last is always a list of children
    (@if_last_is_child ($t1:ty, $t2:ty) { $($t:tt)* } else { $($f:tt)* }) => ($($t)*);
    // 1 type? Well if the first looks like a list of children then it's a child
    (@if_last_is_child ([Id; $n:tt]) { $($t:tt)* } else { $($f:tt)* }) => ($($t)*);
    (@if_last_is_child (Vec<Id>) { $($t:tt)* } else { $($f:tt)* }) => ($($t)*);
    (@if_last_is_child (Id) { $($t:tt)* } else { $($f:tt)* }) => ($($t)*);
    // ... otherwise all other types and 0 types the last type isn't a child
    (@if_last_is_child ($($ty:tt)*) { $($t:tt)* } else { $($f:tt)* }) => ($($f)*);

    // Macro selection to condition on whether the first element of the tuple is
    // a list of children or not.

    // 2 types? Conventionally the first is never a child
    (@if_first_is_child ($t1:ty, $t2:ty) { $($t:tt)* } else { $($f:tt)* }) => ($($f)*);
    // 1 type? Check to see if the first looks like a child or not
    (@if_first_is_child ([Id; $n:tt]) { $($t:tt)* } else { $($f:tt)* }) => ($($t)*);
    (@if_first_is_child (Vec<Id>) { $($t:tt)* } else { $($f:tt)* }) => ($($t)*);
    (@if_first_is_child (Id) { $($t:tt)* } else { $($f:tt)* }) => ($($t)*);
    // ... otherwise all other types or 0 types don't have a child as tthe first type
    (@if_first_is_child ($($ty:tt)*) { $($t:tt)* } else { $($f:tt)* }) => ($($f)*);

    // Implementation of `from_op_str`.

    // Special case for no payload. The `$op` must match exactly and
    // `$children` must be empty.
    (@from_op_str $op:ident $children:ident $name:tt $lang:ident :: $case:ident) => ({
        if $op == $name {
            if $children.len() != 0 {
                return Err(format!("expected zero children"))
            }
            return Ok($lang::$case);
        }
    });

    (@from_op_str $op:ident $children:ident $name:tt $lang:ident::$case:ident($($t:tt)*)) => (
        lang!(@if_first_is_child ($($t)*) {
            // If the first type is a child, then there's no payload and we
            // simply match `$op` to `$name` and fallibly convert the list of
            // children to the correct type.
            if $op == $name {
                return Ok($lang::$case(Children::from($children)?))
            }
        } else {
            // Otherwise if the first type isn't a child, and this isn't an
            // empty variant handled above, our operator has a payload. Look
            // to see whether this is the correct operator by stripping the
            // prefix.
            if let Some(suffix) = $op.strip_prefix(concat!($name, ".")) {
                lang!(@if_last_is_child ($($t)*) {{
                    // If the last part of this type is a child list then the
                    // first part parses `suffix` and children goes into the
                    // list of children.
                    return Ok($lang::$case(
                        match suffix.parse() {
                            Ok(e) => e,
                            Err(e) => return Err(e.to_string()),
                        },
                        Children::from($children)?,
                    ));
                }} else {{
                    // If the last type isn't a child list then it means this
                    // node has no children. Verify as such and then parse the
                    // suffix for the static information.
                    if $children.len() != 0 {
                        return Err(format!("expected zero children"))
                    }
                    return Ok($lang::$case(
                        match suffix.parse() {
                            Ok(e) => e,
                            Err(e) => return Err(e.to_string()),
                        },
                    ));
                }})
            }
        })
    );
}

lang! {
    /// Each "mutable" operator of Wasm is defined here plus articial operators
    /// with a a custom behavior.
    ///
    /// As a note this enum is actually defined by the `lang!` macro above. The
    /// definition here is intended to look similar to the generated
    /// definition, but some conventions must be upheld for now:
    ///
    /// * Variants must either be `A`, `A(B)`, or `A(B, C)`.
    /// * For `A(B)` then `B` can either be a "list" of children or static
    ///   information.
    /// * For `A(B, C)`, if specified `C` must be a "list" of children.
    ///
    /// The "list" of children can be either `Id`, `[Id; N]`, or `Vec<Id>`. When
    /// `B` is not a list of children it must implement both `Display` and
    /// `FromStr`.
    ///
    /// You can define custom operators by adding a variant to this enum.
    /// Notice that custom operator nodes can only be created from rewriting
    /// rules and not form the direct translation of the Wasm binary. Let's
    /// assume for example that we want to implement a custom node that insert
    /// three `nop` operators when it is written back to Wasm.
    ///
    /// * We firsts need to define the custom node ```ThreeNops``` as a variant of
    ///   the [Lang] enum.
    ///   ```ignore
    ///   ThreeNops = "three.nops",
    ///   ```
    /// * The custom node needs to translated to Wasm. To do so, you need to modify
    ///   the `expr2wasm` function somehow as follow
    ///   ```ignore
    ///   Lang::ThreeNops => {
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
    ///
    /// Most other details about the `Lang` should be generated from the enum
    /// variant definition and you can otherwise be guided by various compiler
    /// errors.
    #[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Clone)]
    pub enum Lang {
        // binops integers
        /// i32.add operator
        I32Add([Id; 2]) = "i32.add",
        /// i64.add operator
        I64Add([Id; 2]) = "i64.add",
        /// i32.sub  operator
        I32Sub([Id; 2]) = "i32.sub",
        /// i64.sub  operator
        I64Sub([Id; 2]) = "i64.sub",
        /// i32.mul  operator
        I32Mul([Id; 2]) = "i32.mul",
        /// i64.mul  operator
        I64Mul([Id; 2]) = "i64.mul",
        /// i32.and  operator
        I32And([Id; 2]) = "i32.and",
        /// i64.and  operator
        I64And([Id; 2]) = "i64.and",
        /// i32.or  operator
        I32Or([Id; 2]) = "i32.or",
        /// i64.or  operator
        I64Or([Id; 2]) = "i64.or",
        /// i32.xor  operator
        I32Xor([Id; 2]) = "i32.xor",
        /// i64.xor  operator
        I64Xor([Id; 2]) = "i64.xor",
        /// i32.shl  operator
        I32Shl([Id; 2]) = "i32.shl",
        /// i64.shl  operator
        I64Shl([Id; 2]) = "i64.shl",
        /// i32.shru  operator
        I32ShrU([Id; 2]) = "i32.shr_u",
        /// i64.shru  operator
        I64ShrU([Id; 2]) = "i64.shr_u",
        /// i32.divu  operator
        I32DivU([Id; 2]) = "i32.div_u",
        /// i64.divu  operator
        I64DivU([Id; 2]) = "i64.div_u",
        /// i32.divs  operator
        I32DivS([Id; 2]) = "i32.div_s",
        /// i64.divs  operator
        I64DivS([Id; 2]) = "i64.div_s",
        /// i32.shrs  operator
        I32ShrS([Id; 2]) = "i32.shr_s",
        /// i64.shrs  operator
        I64ShrS([Id; 2]) = "i64.shr_s",
        /// i32.rotr  operator
        I32RotR([Id; 2]) = "i32.rotr",
        /// i64.rotr  operator
        I64RotR([Id; 2]) = "i64.rotr",
        /// i32.rotl  operator
        I32RotL([Id; 2]) = "i32.rotl",
        /// i64.rotl  operator
        I64RotL([Id; 2]) = "i64.rotl",
        /// i32.rems  operator
        I32RemS([Id; 2]) = "i32.rem_s",
        /// i64.rems  operator
        I64RemS([Id; 2]) = "i64.rem_s",
        /// i32.remu  operator
        I32RemU([Id; 2]) = "i32.rem_u",
        /// i64.remu  operator
        I64RemU([Id; 2]) = "i64.rem_u",
        /// i32.eq  operator
        I32Eq([Id; 2]) = "i32.eq",
        /// i64.eq  operator
        I64Eq([Id; 2]) = "i64.eq",
        /// i32.ne  operator
        I32Ne([Id; 2]) = "i32.ne",
        /// i64.ne  operator
        I64Ne([Id; 2]) = "i64.ne",
        /// i32.lts  operator
        I32LtS([Id; 2]) = "i32.lt_s",
        /// i64.lts  operator
        I64LtS([Id; 2]) = "i64.lt_s",
        /// i32.ltu  operator
        I32LtU([Id; 2]) = "i32.lt_u",
        /// i64.ltu  operator
        I64LtU([Id; 2]) = "i64.lt_u",
        /// i32.gts  operator
        I32GtS([Id; 2]) = "i32.gt_s",
        /// i64.gts  operator
        I64GtS([Id; 2]) = "i64.gt_s",
        /// i32.gtu  operator
        I32GtU([Id; 2]) = "i32.gt_u",
        /// i64.gtu  operator
        I64GtU([Id; 2]) = "i64.gt_u",
        /// i32.les  operator
        I32LeS([Id; 2]) = "i32.le_s",
        /// i64.les  operator
        I64LeS([Id; 2]) = "i64.le_s",
        /// i32.leu  operator
        I32LeU([Id; 2]) = "i32.le_u",
        /// i64.leu  operator
        I64LeU([Id; 2]) = "i64.le_u",
        /// i32.ges  operator
        I32GeS([Id; 2]) = "i32.ge_s",
        /// i64.ges  operator
        I64GeS([Id; 2]) = "i64.ge_s",
        /// i32.geu  operator
        I32GeU([Id; 2]) = "i32.ge_u",
        /// i64.geu  operator
        I64GeU([Id; 2]) = "i64.ge_u",
        // binops floats
        /// f32.add  operator
        F32Add([Id; 2]) = "f32.add",
        /// f64.add  operator
        F64Add([Id; 2]) = "f64.add",
        /// f32.sub  operator
        F32Sub([Id; 2]) = "f32.sub",
        /// f64.sub  operator
        F64Sub([Id; 2]) = "f64.sub",
        /// f32.mul  operator
        F32Mul([Id; 2]) = "f32.mul",
        /// f64.mul  operator
        F64Mul([Id; 2]) = "f64.mul",
        /// f32.div  operator
        F32Div([Id; 2]) = "f32.div",
        /// f64.div  operator
        F64Div([Id; 2]) = "f64.div",
        /// f32.min  operator
        F32Min([Id; 2]) = "f32.min",
        /// f64.min  operator
        F64Min([Id; 2]) = "f64.min",
        /// f32.max  operator
        F32Max([Id; 2]) = "f32.max",
        /// f64.max  operator
        F64Max([Id; 2]) = "f64.max",
        /// f32.copysign  operator
        F32Copysign([Id; 2]) = "f32.copysign",
        /// f64.copysign  operator
        F64Copysign([Id; 2]) = "f64.copysign",

        // unops v128
        /// v128.not operator
        V128Not([Id; 1]) = "v128.not",
        /// v128.any_true operator
        V128AnyTrue([Id; 1]) = "v128.any_true",

        // binops v128
        /// v128.bitselect operator
        V128And([Id; 2]) = "v128.and",
        /// v128.and_not operator
        V128AndNot([Id; 2]) = "v128.and_not",
        /// v128.or operator
        V128Or([Id; 2]) = "v128.or",
        /// v128.xor operator
        V128Xor([Id; 2]) = "v128.xor",

        // ternary ops v128
        /// v128.bitselect operator
        V128Bitselect([Id; 3]) = "v128.bitselect",

        // frelops
        /// f32.eq  operator
        F32Eq([Id; 2]) = "f32.eq",
        /// f64.eq  operator
        F64Eq([Id; 2]) = "f64.eq",
        /// f32.ne  operator
        F32Ne([Id; 2]) = "f32.ne",
        /// f64.ne  operator
        F64Ne([Id; 2]) = "f64.ne",
        /// f32.lt  operator
        F32Lt([Id; 2]) = "f32.lt",
        /// f64.lt  operator
        F64Lt([Id; 2]) = "f64.lt",
        /// f32.gt  operator
        F32Gt([Id; 2]) = "f32.gt",
        /// f64.gt  operator
        F64Gt([Id; 2]) = "f64.gt",
        /// f32.le  operator
        F32Le([Id; 2]) = "f32.le",
        /// f64.le  operator
        F64Le([Id; 2]) = "f64.le",
        /// f32.ge  operator
        F32Ge([Id; 2]) = "f32.ge",
        /// f64.ge  operator
        F64Ge([Id; 2]) = "f64.ge",

        // unops integers
        /// i32.eqz  operator
        I32Eqz([Id; 1]) = "i32.eqz",
        /// i64.eqz  operator
        I64Eqz([Id; 1]) = "i64.eqz",

        /// i32.popcnt  operator
        I32Popcnt([Id; 1]) = "i32.popcnt",
        /// i64.popcnt  operator
        I64Popcnt([Id; 1]) = "i64.popcnt",
        /// i32.clz  operator
        I32Clz([Id; 1]) = "i32.clz",
        /// i32.ctz  operator
        I32Ctz([Id; 1]) = "i32.ctz",
        /// i64.ctz  operator
        I64Ctz([Id; 1]) = "i64.ctz",
        /// i64.clz  operator
        I64Clz([Id; 1]) = "i64.clz",

        // unops floats
        /// f32.abs  operator
        F32Abs([Id; 1]) = "f32.abs",
        /// f64.abs  operator
        F64Abs([Id; 1]) = "f64.abs",
        /// f32.neg  operator
        F32Neg([Id; 1]) = "f32.neg",
        /// f64.neg  operator
        F64Neg([Id; 1]) = "f64.neg",
        /// f32.sqrt  operator
        F32Sqrt([Id; 1]) = "f32.sqrt",
        /// f64.sqrt  operator
        F64Sqrt([Id; 1]) = "f64.sqrt",
        /// f32.ceil  operator
        F32Ceil([Id; 1]) = "f32.ceil",
        /// f64.ceil  operator
        F64Ceil([Id; 1]) = "f64.ceil",
        /// f32.floor  operator
        F32Floor([Id; 1]) = "f32.floor",
        /// f64.floor  operator
        F64Floor([Id; 1]) = "f64.floor",
        /// f32.trunc  operator
        F32Trunc([Id; 1]) = "f32.trunc",
        /// f64.trunc  operator
        F64Trunc([Id; 1]) = "f64.trunc",
        /// f32.nearest  operator
        F32Nearest([Id; 1]) = "f32.nearest",
        /// f64.nearest  operator
        F64Nearest([Id; 1]) = "f64.nearest",

        // Locals
        // Idx and value
        /// local.tee operator
        LocalTee(u32, Id) = "local.tee",
        // Idx and value
        /// local.set operator
        LocalSet(u32, Id) = "local.set",
        /// local.get operator
        LocalGet(u32) = "local.get",

        // Globals
        // Idx and value
        /// global.set operator
        GlobalSet(u32, Id) = "global.set",
        /// global.get operator
        GlobalGet(u32) = "global.get",

        // conversion operators
        /// i32.wrap_i64 operator
        Wrap([Id; 1]) = "wrap",

        // conversion
        /// i32.extend.8s operator
        I32Extend8S([Id; 1]) = "i32.extend8_s",
        /// i64.extend.8s operator
        I64Extend8S([Id; 1]) = "i64.extend8_s",
        /// i32.extend.16s operator
        I32Extend16S([Id; 1]) = "i32.extend16_s",
        /// i64.extend.16s operator
        I64Extend16S([Id; 1]) = "i64.extend16_s",
        /// i64.extend.32s operator
        I64Extend32S([Id; 1]) = "i64.extend32_s",
        /// i64.extendi.32s operator
        I64ExtendI32S([Id; 1]) = "i64.extend_i32_s",
        /// i64.extendi.32u operator
        I64ExtendI32U([Id; 1]) = "i64.extend_i32_u",
        /// i32.truncf.32s operator
        I32TruncF32S([Id; 1]) = "i32.trunc_f32_s",
        /// i32.truncf.32u operator
        I32TruncF32U([Id; 1]) = "i32.trunc_f32_u",
        /// i32.truncf.64s operator
        I32TruncF64S([Id; 1]) = "i32.trunc_f64_s",
        /// i32.truncf.64u operator
        I32TruncF64U([Id; 1]) = "i32.trunc_f64_u",
        /// i64.truncf.32s operator
        I64TruncF32S([Id; 1]) = "i64.trunc_f32_s",
        /// i64.truncf.32u operator
        I64TruncF32U([Id; 1]) = "i64.trunc_f32_u",
        /// i64.truncf.64s operator
        I64TruncF64S([Id; 1]) = "i64.trunc_f64_s",
        /// i64.truncf.64u operator
        I64TruncF64U([Id; 1]) = "i64.trunc_f64_u",
        /// f32.converti.32s operator
        F32ConvertI32S([Id; 1]) = "f32.convert_i32_s",
        /// f32.converti.32u operator
        F32ConvertI32U([Id; 1]) = "f32.convert_i32_u",
        /// f32.converti.64s operator
        F32ConvertI64S([Id; 1]) = "f32.convert_i64_s",
        /// f32.converti.64u operator
        F32ConvertI64U([Id; 1]) = "f32.convert_i64_u",
        /// f32.demote.f64 operator
        F32DemoteF64([Id; 1]) = "f32.demote_f64",
        /// f64.converti.32s operator
        F64ConvertI32S([Id; 1]) = "f64.convert_i32_s",
        /// f64.converti.32u operator
        F64ConvertI32U([Id; 1]) = "f64.convert_i32_u",
        /// f64.converti.64s operator
        F64ConvertI64S([Id; 1]) = "f64.convert_i64_s",
        /// f64.converti.64u operator
        F64ConvertI64U([Id; 1]) = "f64.convert_i64_u",
        /// f64.promote.f32 operator
        F64PromoteF32([Id; 1]) = "f64.promote_f32",
        /// i32.reinterpret.f32 operator
        I32ReinterpretF32([Id; 1]) = "i32.reinterpret_f32",
        /// i64.reinterpret.f64 operator
        I64ReinterpretF64([Id; 1]) = "i64.reinterpret_f64",
        /// f32.reinterpret.i32 operator
        F32ReinterpretI32([Id; 1]) = "f32.reinterpret_i32",
        /// f64.reinterpret.i64 operator
        F64ReinterpretI64([Id; 1]) = "f64.reinterpret_i64",
        /// i32.truncsatf.32s operator
        I32TruncSatF32S([Id; 1]) = "i32.trunc_sat_f32_s",
        /// i32.truncsatf.32u operator
        I32TruncSatF32U([Id; 1]) = "i32.trunc_sat_f32_u",
        /// i32.truncsatf.64s operator
        I32TruncSatF64S([Id; 1]) = "i32.trunc_sat_f64_s",
        /// i32.truncsatf.64u operator
        I32TruncSatF64U([Id; 1]) = "i32.trunc_sat_f64_u",
        /// i64.truncsatf.32s operator
        I64TruncSatF32S([Id; 1]) = "i64.trunc_sat_f32_s",
        /// i64.truncsatf.32u operator
        I64TruncSatF32U([Id; 1]) = "i64.trunc_sat_f32_u",
        /// i64.truncsatf.64s operator
        I64TruncSatF64S([Id; 1]) = "i64.trunc_sat_f64_s",
        /// i64.truncsatf.64u operator
        I64TruncSatF64U([Id; 1]) = "i64.trunc_sat_f64_u",

        // The u32 argument should be the function index
        /// call operator node
        Call(u32, Vec<Id>) = "call",
        /// drop operator
        Drop([Id; 1]) = "drop",
        /// nop operator
        Nop = "nop",

        // Memory operations
        // loads
        /// i32.load operator
        I32Load(MemArg, Id) = "i32.load",
        /// i64.load operator
        I64Load(MemArg, Id) = "i64.load",
        /// f32.load operator
        F32Load(MemArg, Id) = "f32.load",
        /// f64.load operator
        F64Load(MemArg, Id) = "f64.load",
        /// i32.load8_s operator
        I32Load8S(MemArg, Id) = "i32.load8_s",
        /// i32.load8_u operator
        I32Load8U(MemArg, Id) = "i32.load8_u",
        /// i32.load16_s operator
        I32Load16S(MemArg, Id) = "i32.load16_s",
        /// i32.load16_u operator
        I32Load16U(MemArg, Id) = "i32.load16_u",
        /// i64.load8_s operator
        I64Load8S(MemArg, Id) = "i64.load8_s",
        /// i64.load8_u operator
        I64Load8U(MemArg, Id) = "i64.load8_u",
        /// i64.load16_s operator
        I64Load16S(MemArg, Id) = "i64.load16_s",
        /// i64.load16_u operator
        I64Load16U(MemArg, Id) = "i64.load16_u",
        /// i64.load32_s operator
        I64Load32S(MemArg, Id) = "i64.load32_s",
        /// i64.load32_u operator
        I64Load32U(MemArg, Id) = "i64.load32_u",

        /// i32.store operator
        I32Store(MemArg, [Id; 2]) = "i32.store",
        /// i64.store operator
        I64Store(MemArg, [Id; 2]) = "i64.store",
        /// f32.store operator
        F32Store(MemArg, [Id; 2]) = "f32.store",
        /// f64.store operator
        F64Store(MemArg, [Id; 2]) = "f64.store",
        /// i32.store8 operator
        I32Store8(MemArg, [Id; 2]) = "i32.store8",
        /// i32.store16 operator
        I32Store16(MemArg, [Id; 2]) = "i32.store16",
        /// i64.store8 operator
        I64Store8(MemArg, [Id; 2]) = "i64.store8",
        /// i64.store16 operator
        I64Store16(MemArg, [Id; 2]) = "i64.store16",
        /// i64.store32 operator
        I64Store32(MemArg, [Id; 2]) = "i64.store32",

        /// Select operator
        Select([Id; 3]) = "select",
        /// Memory grow operator
        MemoryGrow(u32, Id) = "memory.grow",
        /// Memory size operator
        MemorySize(u32) = "memory.size",

        MemoryInit(MemoryInit, [Id; 3]) = "memory.init",
        DataDrop(u32) = "data.drop",
        MemoryCopy(MemoryCopy, [Id; 3]) = "memory.copy",
        MemoryFill(u32, [Id; 3]) = "memory.fill",

        TableInit(TableInit, [Id; 3]) = "table.init",
        ElemDrop(u32) = "elem.drop",
        TableCopy(TableCopy, [Id; 3]) = "table.copy",
        TableFill(u32, [Id; 3]) = "table.fill",
        TableGet(u32, Id) = "table.get",
        TableSet(u32, [Id; 2]) = "table.set",
        TableGrow(u32, [Id; 2]) = "table.grow",
        TableSize(u32) = "table.size",
        RefIsNull(Id) = "ref.is_null",

        V128Load(MemArg, Id) = "v128.load",
        V128Load8x8S(MemArg, Id) = "v128.load8x8_s",
        V128Load8x8U(MemArg, Id) = "v128.load8x8_u",
        V128Load16x4S(MemArg, Id) = "v128.load16x4_s",
        V128Load16x4U(MemArg, Id) = "v128.load16x4_u",
        V128Load32x2S(MemArg, Id) = "v128.load32x2_s",
        V128Load32x2U(MemArg, Id) = "v128.load32x2_u",
        V128Load8Splat(MemArg, Id) = "v128.load8_splat",
        V128Load16Splat(MemArg, Id) = "v128.load16_splat",
        V128Load32Splat(MemArg, Id) = "v128.load32_splat",
        V128Load64Splat(MemArg, Id) = "v128.load64_splat",
        V128Load32Zero(MemArg, Id) = "v128.load32_zero",
        V128Load64Zero(MemArg, Id) = "v128.load64_zero",
        V128Store(MemArg, [Id; 2]) = "v128.store",
        V128Load8Lane(MemArgLane, [Id; 2]) = "v128.load8_lane",
        V128Load16Lane(MemArgLane, [Id; 2]) = "v128.load16_lane",
        V128Load32Lane(MemArgLane, [Id; 2]) = "v128.load32_lane",
        V128Load64Lane(MemArgLane, [Id; 2]) = "v128.load64_lane",
        V128Store8Lane(MemArgLane, [Id; 2]) = "v128.store8_lane",
        V128Store16Lane(MemArgLane, [Id; 2]) = "v128.store16_lane",
        V128Store32Lane(MemArgLane, [Id; 2]) = "v128.store32_lane",
        V128Store64Lane(MemArgLane, [Id; 2]) = "v128.store64_lane",

        I8x16ExtractLaneS(u8, Id) = "i8x16.extract_lane_s",
        I8x16ExtractLaneU(u8, Id) = "i8x16.extract_lane_u",
        I8x16ReplaceLane(u8, [Id; 2]) = "i8x16.replace_lane",
        I16x8ExtractLaneS(u8, Id) = "i16x8.extract_lane_s",
        I16x8ExtractLaneU(u8, Id) = "i16x8.extract_lane_u",
        I16x8ReplaceLane(u8, [Id; 2]) = "i16x8.replace_lane",
        I32x4ExtractLane(u8, Id) = "i32x4.extract_lane",
        I32x4ReplaceLane(u8, [Id; 2]) = "i32x4.replace_lane",
        I64x2ExtractLane(u8, Id) = "i64x2.extract_lane",
        I64x2ReplaceLane(u8, [Id; 2]) = "i64x2.replace_lane",
        F32x4ExtractLane(u8, Id) = "f32x4.extract_lane",
        F32x4ReplaceLane(u8, [Id; 2]) = "f32x4.replace_lane",
        F64x2ExtractLane(u8, Id) = "f64x2.extract_lane",
        F64x2ReplaceLane(u8, [Id; 2]) = "f64x2.replace_lane",

        I8x16Swizzle([Id; 2]) = "i8x16.swizzle",
        I8x16Shuffle(Shuffle, [Id; 2]) = "i8x16.shuffle",
        I8x16Splat([Id; 1]) = "i8x16.splat",
        I16x8Splat([Id; 1]) = "i16x8.splat",
        I32x4Splat([Id; 1]) = "i32x4.splat",
        I64x2Splat([Id; 1]) = "i64x2.splat",
        F32x4Splat([Id; 1]) = "f32x4.splat",
        F64x2Splat([Id; 1]) = "f64x2.splat",

        I8x16Eq([Id; 2]) = "i8x16.eq",
        I8x16Ne([Id; 2]) = "i8x16.ne",
        I8x16LtS([Id; 2]) = "i8x16.lt_s",
        I8x16LtU([Id; 2]) = "i8x16.lt_u",
        I8x16GtS([Id; 2]) = "i8x16.gt_s",
        I8x16GtU([Id; 2]) = "i8x16.gt_u",
        I8x16LeS([Id; 2]) = "i8x16.le_s",
        I8x16LeU([Id; 2]) = "i8x16.le_u",
        I8x16GeS([Id; 2]) = "i8x16.ge_s",
        I8x16GeU([Id; 2]) = "i8x16.ge_u",

        I16x8Eq([Id; 2]) = "i16x8.eq",
        I16x8Ne([Id; 2]) = "i16x8.ne",
        I16x8LtS([Id; 2]) = "i16x8.lt_s",
        I16x8LtU([Id; 2]) = "i16x8.lt_u",
        I16x8GtS([Id; 2]) = "i16x8.gt_s",
        I16x8GtU([Id; 2]) = "i16x8.gt_u",
        I16x8LeS([Id; 2]) = "i16x8.le_s",
        I16x8LeU([Id; 2]) = "i16x8.le_u",
        I16x8GeS([Id; 2]) = "i16x8.ge_s",
        I16x8GeU([Id; 2]) = "i16x8.ge_u",

        I32x4Eq([Id; 2]) = "i32x4.eq",
        I32x4Ne([Id; 2]) = "i32x4.ne",
        I32x4LtS([Id; 2]) = "i32x4.lt_s",
        I32x4LtU([Id; 2]) = "i32x4.lt_u",
        I32x4GtS([Id; 2]) = "i32x4.gt_s",
        I32x4GtU([Id; 2]) = "i32x4.gt_u",
        I32x4LeS([Id; 2]) = "i32x4.le_s",
        I32x4LeU([Id; 2]) = "i32x4.le_u",
        I32x4GeS([Id; 2]) = "i32x4.ge_s",
        I32x4GeU([Id; 2]) = "i32x4.ge_u",

        I64x2Eq([Id; 2]) = "i64x2.eq",
        I64x2Ne([Id; 2]) = "i64x2.ne",
        I64x2LtS([Id; 2]) = "i64x2.lt_s",
        I64x2GtS([Id; 2]) = "i64x2.gt_s",
        I64x2LeS([Id; 2]) = "i64x2.le_s",
        I64x2GeS([Id; 2]) = "i64x2.ge_s",

        F32x4Eq([Id; 2]) = "f32x4.eq",
        F32x4Ne([Id; 2]) = "f32x4.ne",
        F32x4Lt([Id; 2]) = "f32x4.lt",
        F32x4Gt([Id; 2]) = "f32x4.gt",
        F32x4Le([Id; 2]) = "f32x4.le",
        F32x4Ge([Id; 2]) = "f32x4.ge",

        I8x16Abs([Id; 1]) = "i8x16.abs",
        I8x16Neg([Id; 1]) = "i8x16.neg",
        I8x16Popcnt([Id; 1]) = "i8x16.popcnt",
        I8x16AllTrue([Id; 1]) = "i8x16.all_true",
        I8x16Bitmask([Id; 1]) = "i8x16.bitmask",
        I8x16NarrowI16x8S([Id; 2]) = "i8x16.narrow_i16x8_s",
        I8x16NarrowI16x8U([Id; 2]) = "i8x16.narrow_i16x8_u",
        I8x16Shl([Id; 2]) = "i8x16.shl",
        I8x16ShrS([Id; 2]) = "i8x16.shr_s",
        I8x16ShrU([Id; 2]) = "i8x16.shr_u",
        I8x16Add([Id; 2]) = "i8x16.add",
        I8x16AddSatS([Id; 2]) = "i8x16.add_sat_s",
        I8x16AddSatU([Id; 2]) = "i8x16.add_sat_u",
        I8x16Sub([Id; 2]) = "i8x16.sub",
        I8x16SubSatS([Id; 2]) = "i8x16.sub_sat_s",
        I8x16SubSatU([Id; 2]) = "i8x16.sub_sat_u",
        I8x16MinS([Id; 2]) = "i8x16.min_s",
        I8x16MinU([Id; 2]) = "i8x16.min_u",
        I8x16MaxS([Id; 2]) = "i8x16.max_s",
        I8x16MaxU([Id; 2]) = "i8x16.max_u",
        I8x16AvgrU([Id; 2]) = "i8x16.avgr_u",

        I16x8ExtAddPairwiseI8x16S([Id; 1]) = "i16x8.extadd_pairwise_i8x16_s",
        I16x8ExtAddPairwiseI8x16U([Id; 1]) = "i16x8.extadd_pairwise_i8x16_u",
        I16x8Abs([Id; 1]) = "i16x8.abs",
        I16x8Neg([Id; 1]) = "i16x8.neg",
        I16x8Q15MulrSatS([Id; 2]) = "i16x8.q15mulr_sat_s",
        I16x8AllTrue([Id; 1]) = "i16x8.all_true",
        I16x8Bitmask([Id; 1]) = "i16x8.bitmask",
        I16x8NarrowI32x4S([Id; 2]) = "i16x8.narrow_i32x4_s",
        I16x8NarrowI32x4U([Id; 2]) = "i16x8.narrow_i32x4_u",
        I16x8ExtendLowI8x16S([Id; 1]) = "i16x8.extend_low_i8x16_s",
        I16x8ExtendHighI8x16S([Id; 1]) = "i16x8.extend_high_i8x16_s",
        I16x8ExtendLowI8x16U([Id; 1]) = "i16x8.extend_low_i8x16_u",
        I16x8ExtendHighI8x16U([Id; 1]) = "i16x8.extend_high_i8x16_u",
        I16x8Shl([Id; 2]) = "i16x8.shl",
        I16x8ShrS([Id; 2]) = "i16x8.shr_s",
        I16x8ShrU([Id; 2]) = "i16x8.shr_u",
        I16x8Add([Id; 2]) = "i16x8.add",
        I16x8AddSatS([Id; 2]) = "i16x8.add_sat_s",
        I16x8AddSatU([Id; 2]) = "i16x8.add_sat_u",
        I16x8Sub([Id; 2]) = "i16x8.sub",
        I16x8SubSatS([Id; 2]) = "i16x8.sub_sat_s",
        I16x8SubSatU([Id; 2]) = "i16x8.sub_sat_u",
        I16x8Mul([Id; 2]) = "i16x8.mul",
        I16x8MinS([Id; 2]) = "i16x8.min_s",
        I16x8MinU([Id; 2]) = "i16x8.min_u",
        I16x8MaxS([Id; 2]) = "i16x8.max_s",
        I16x8MaxU([Id; 2]) = "i16x8.max_u",
        I16x8AvgrU([Id; 2]) = "i16x8.avgr_u",
        I16x8ExtMulLowI8x16S([Id; 2]) = "i16x8.extmul_low_i8x16_s",
        I16x8ExtMulHighI8x16S([Id; 2]) = "i16x8.extmul_low_i8x16_s",
        I16x8ExtMulLowI8x16U([Id; 2]) = "i16x8.extmul_low_i8x16_u",
        I16x8ExtMulHighI8x16U([Id; 2]) = "i16x8.extmul_low_i8x16_u",

        I32x4ExtAddPairwiseI16x8S([Id; 1]) = "i32x4.extadd_pairwise_i16x8_s",
        I32x4ExtAddPairwiseI16x8U([Id; 1]) = "i32x4.extadd_pairwise_i16x8_u",
        I32x4Abs([Id; 1]) = "i32x4.abs",
        I32x4Neg([Id; 1]) = "i32x4.neg",
        I32x4AllTrue([Id; 1]) = "i32x4.all_true",
        I32x4Bitmask([Id; 1]) = "i32x4.bitmask",
        I32x4ExtendLowI16x8S([Id; 1]) = "i32x4.extend_low_i16x8_s",
        I32x4ExtendHighI16x8S([Id; 1]) = "i32x4.extend_high_i16x8_s",
        I32x4ExtendLowI16x8U([Id; 1]) = "i32x4.extend_low_i16x8_u",
        I32x4ExtendHighI16x8U([Id; 1]) = "i32x4.extend_high_i16x8_u",
        I32x4Shl([Id; 2]) = "i32x4.shl",
        I32x4ShrS([Id; 2]) = "i32x4.shr_s",
        I32x4ShrU([Id; 2]) = "i32x4.shr_u",
        I32x4Add([Id; 2]) = "i32x4.add",
        I32x4Sub([Id; 2]) = "i32x4.sub",
        I32x4Mul([Id; 2]) = "i32x4.mul",
        I32x4MinS([Id; 2]) = "i32x4.min_s",
        I32x4MinU([Id; 2]) = "i32x4.min_u",
        I32x4MaxS([Id; 2]) = "i32x4.max_s",
        I32x4MaxU([Id; 2]) = "i32x4.max_u",
        I32x4DotI16x8S([Id; 2]) = "i32x4.dot_i16x8_s",
        I32x4ExtMulLowI16x8S([Id; 2]) = "i32x4.extmul_low_i16x8_s",
        I32x4ExtMulHighI16x8S([Id; 2]) = "i32x4.extmul_low_i16x8_s",
        I32x4ExtMulLowI16x8U([Id; 2]) = "i32x4.extmul_low_i16x8_u",
        I32x4ExtMulHighI16x8U([Id; 2]) = "i32x4.extmul_low_i16x8_u",

        I64x2Abs([Id; 1]) = "i64x2.abs",
        I64x2Neg([Id; 1]) = "i64x2.neg",
        I64x2AllTrue([Id; 1]) = "i64x2.all_true",
        I64x2Bitmask([Id; 1]) = "i64x2.bitmask",
        I64x2ExtendLowI32x4S([Id; 1]) = "i64x2.extend_low_i32x4_s",
        I64x2ExtendHighI32x4S([Id; 1]) = "i64x2.extend_high_i32x4_s",
        I64x2ExtendLowI32x4U([Id; 1]) = "i64x2.extend_low_i32x4_u",
        I64x2ExtendHighI32x4U([Id; 1]) = "i64x2.extend_high_i32x4_u",
        I64x2Shl([Id; 2]) = "i64x2.shl",
        I64x2ShrS([Id; 2]) = "i64x2.shr_s",
        I64x2ShrU([Id; 2]) = "i64x2.shr_u",
        I64x2Add([Id; 2]) = "i64x2.add",
        I64x2Sub([Id; 2]) = "i64x2.sub",
        I64x2Mul([Id; 2]) = "i64x2.mul",
        I64x2ExtMulLowI32x4S([Id; 2]) = "i64x2.extmul_low_i32x4_s",
        I64x2ExtMulHighI32x4S([Id; 2]) = "i64x2.extmul_low_i32x4_s",
        I64x2ExtMulLowI32x4U([Id; 2]) = "i64x2.extmul_low_i32x4_u",
        I64x2ExtMulHighI32x4U([Id; 2]) = "i64x2.extmul_low_i32x4_u",

        F64x2Eq([Id; 2]) = "f64x2.eq",
        F64x2Ne([Id; 2]) = "f64x2.ne",
        F64x2Lt([Id; 2]) = "f64x2.lt",
        F64x2Gt([Id; 2]) = "f64x2.gt",
        F64x2Le([Id; 2]) = "f64x2.le",
        F64x2Ge([Id; 2]) = "f64x2.ge",

        F32x4Ceil([Id; 1]) = "f32x4.ceil",
        F32x4Floor([Id; 1]) = "f32x4.floor",
        F32x4Trunc([Id; 1]) = "f32x4.trunc",
        F32x4Nearest([Id; 1]) = "f32x4.nearest",
        F32x4Abs([Id; 1]) = "f32x4.abs",
        F32x4Neg([Id; 1]) = "f32x4.neg",
        F32x4Sqrt([Id; 1]) = "f32x4.sqrt",
        F32x4Add([Id; 2]) = "f32x4.add",
        F32x4Sub([Id; 2]) = "f32x4.sub",
        F32x4Mul([Id; 2]) = "f32x4.mul",
        F32x4Div([Id; 2]) = "f32x4.div",
        F32x4Min([Id; 2]) = "f32x4.min",
        F32x4Max([Id; 2]) = "f32x4.max",
        F32x4PMin([Id; 2]) = "f32x4.pmin",
        F32x4PMax([Id; 2]) = "f32x4.pmax",

        F64x2Ceil([Id; 1]) = "f64x2.ceil",
        F64x2Floor([Id; 1]) = "f64x2.floor",
        F64x2Trunc([Id; 1]) = "f64x2.trunc",
        F64x2Nearest([Id; 1]) = "f64x2.nearest",
        F64x2Abs([Id; 1]) = "f64x2.abs",
        F64x2Neg([Id; 1]) = "f64x2.neg",
        F64x2Sqrt([Id; 1]) = "f64x2.sqrt",
        F64x2Add([Id; 2]) = "f64x2.add",
        F64x2Sub([Id; 2]) = "f64x2.sub",
        F64x2Mul([Id; 2]) = "f64x2.mul",
        F64x2Div([Id; 2]) = "f64x2.div",
        F64x2Min([Id; 2]) = "f64x2.min",
        F64x2Max([Id; 2]) = "f64x2.max",
        F64x2PMin([Id; 2]) = "f64x2.pmin",
        F64x2PMax([Id; 2]) = "f64x2.pmax",

        I32x4TruncSatF32x4S([Id; 1]) = "i32x4.trunc_sat_f32x4_s",
        I32x4TruncSatF32x4U([Id; 1]) = "i32x4.trunc_sat_f32x4_u",
        F32x4ConvertI32x4S([Id; 1]) = "f32x4.convert_i32x4_s",
        F32x4ConvertI32x4U([Id; 1]) = "f32x4.convert_i32x4_u",
        I32x4TruncSatF64x2SZero([Id; 1]) = "i32x4.trunc_sat_f64x2_s_zero",
        I32x4TruncSatF64x2UZero([Id; 1]) = "i32x4.trunc_sat_f64x2_u_zero",
        F64x2ConvertLowI32x4S([Id; 1]) = "f64x2.convert_low_i32x4_s",
        F64x2ConvertLowI32x4U([Id; 1]) = "f64x2.convert_low_i32x4_u",
        F32x4DemoteF64x2Zero([Id; 1]) = "f32x4.demote_f64x2_zero",
        F64x2PromoteLowF32x4([Id; 1]) = "f64x2.promote_low_f32x4",

        // relaxed-simd instructions

        I8x16RelaxedSwizzle([Id; 2]) = "i8x16.relaxed_swizzle",
        I32x4RelaxedTruncF32x4S([Id; 1]) = "i32x4.relaxed_trunc_f32x4_s",
        I32x4RelaxedTruncF32x4U([Id; 1]) = "i32x4.relaxed_trunc_f32x4_u",
        I32x4RelaxedTruncF64x2SZero([Id; 1]) = "i32x4.relaxed_trunc_f64x2_s_zero",
        I32x4RelaxedTruncF64x2UZero([Id; 1]) = "i32x4.relaxed_trunc_f64x2_u_zero",
        F32x4RelaxedMadd([Id; 3]) = "f32x4.relaxed_madd",
        F32x4RelaxedNmadd([Id; 3]) = "f32x4.relaxed_nmadd",
        F64x2RelaxedMadd([Id; 3]) = "f64x2.relaxed_madd",
        F64x2RelaxedNmadd([Id; 3]) = "f64x2.relaxed_nmadd",
        I8x16RelaxedLaneselect([Id; 3]) = "i8x16.relaxed_laneselect",
        I16x8RelaxedLaneselect([Id; 3]) = "i16x8.relaxed_laneselect",
        I32x4RelaxedLaneselect([Id; 3]) = "i32x4.relaxed_laneselect",
        I64x2RelaxedLaneselect([Id; 3]) = "i64x2.relaxed_laneselect",
        F32x4RelaxedMin([Id; 2]) = "f32x4.relaxed_min",
        F32x4RelaxedMax([Id; 2]) = "f32x4.relaxed_max",
        F64x2RelaxedMin([Id; 2]) = "f64x2.relaxed_min",
        F64x2RelaxedMax([Id; 2]) = "f64x2.relaxed_max",
        I16x8RelaxedQ15mulrS([Id; 2]) = "i16x8.relaxed_q15_mulr_s",
        I16x8RelaxedDotI8x16I7x16S([Id; 2]) = "i16x8.relaxed_dot_i8x16_i7x16_s",
        I32x4RelaxedDotI8x16I7x16AddS([Id; 3]) = "i32x4.relaxed_dot_i8x16_i7x16_add_s",

        /// Add custom or others operator nodes below

        /// Custom mutation operations and instructions
        ///
        /// This operation represents a random i32 integer
        RandI32 = "i32.rand",
        /// This operation represents a random i64 integer
        RandI64 = "i64.rand",
        /// This operation represents a random f32
        RandF32 = "f32.rand",
        /// This operation represents a random f64
        RandF64 = "f64.rand",

        /// This instructions is used to define unknown operands, for example
        /// when the value can come from the join of several basic blocks in a
        /// dfg
        Undef = "undef",

        /// Takes one i32 constant operand and turn it into a sum of two random
        /// numbers whihch sum is the operand `i32.const x = i32.const r +
        /// i32.const (x - r) `
        UnfoldI32(Id) = "i32.unfold",
        /// Takes one i64 constant operand and turn it into a sum of two random
        /// numbers whihch sum is the operand `i64.const x = i64.const r +
        /// i64.const (x - r) `
        UnfoldI64(Id) = "i64.unfold",

        /// Propsoed custom mutator from issue #391, use-of-global
        I32UseGlobal(Id) = "i32.use_of_global",
        /// Propsoed custom mutator from issue #391, use-of-global
        I64UseGlobal(Id) = "i64.use_of_global",
        /// Propsoed custom mutator from issue #391, use-of-global
        F32UseGlobal(Id) = "f32.use_of_global",
        /// Propsoed custom mutator from issue #391, use-of-global
        F64UseGlobal(Id) = "f64.use_of_global",

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
        Container(Vec<Id>) = "container",

        // End of custom mutation operations and instructions
        /// I32 constant node
        I32(i32) = "i32.const",
        /// I64 constant node
        I64(i64) = "i64.const",

        // Save bits
        /// F32 constant node
        F32(F32) = "f32.const",
        /// F64 constant node
        F64(F64) = "f64.const",
        /// V128 constant node
        V128(i128) = "v128.const",
        /// constant ref.null node
        RefNull(RefType) = "ref.null",
        /// constant ref.func node
        RefFunc(u32) = "ref.func",
    }
}

impl Default for Lang {
    fn default() -> Self {
        Lang::Undef
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord, Debug)]
pub struct F32(u32);

impl F32 {
    pub fn to_f32(self) -> f32 {
        f32::from_bits(self.0)
    }
}

impl FromStr for F32 {
    type Err = <f32 as FromStr>::Err;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Ok(F32(f32::from_str(&s.replace(",", "."))?.to_bits()))
    }
}

impl From<f32> for F32 {
    fn from(f: f32) -> Self {
        Self(f.to_bits())
    }
}

impl From<wasmparser::Ieee32> for F32 {
    fn from(f: wasmparser::Ieee32) -> Self {
        Self(f.bits())
    }
}

impl fmt::Display for F32 {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        fmt::Display::fmt(&self.to_f32(), f)
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord, Debug)]
pub struct F64(u64);

impl F64 {
    pub fn to_f64(self) -> f64 {
        f64::from_bits(self.0)
    }
}

impl FromStr for F64 {
    type Err = <f64 as FromStr>::Err;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Ok(F64(f64::from_str(&s.replace(",", "."))?.to_bits()))
    }
}

impl From<f64> for F64 {
    fn from(f: f64) -> Self {
        Self(f.to_bits())
    }
}

impl From<wasmparser::Ieee64> for F64 {
    fn from(f: wasmparser::Ieee64) -> Self {
        Self(f.bits())
    }
}

impl fmt::Display for F64 {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        fmt::Display::fmt(&self.to_f64(), f)
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord, Debug)]
pub enum RefType {
    Func,
    Extern,
}

impl From<RefType> for HeapType {
    fn from(rt: RefType) -> Self {
        match rt {
            RefType::Func => HeapType::Abstract {
                shared: false,
                ty: AbstractHeapType::Func,
            },
            RefType::Extern => HeapType::Abstract {
                shared: false,
                ty: AbstractHeapType::Extern,
            },
        }
    }
}

impl FromStr for RefType {
    type Err = Box<dyn std::error::Error + Send + Sync>;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "func" => Ok(RefType::Func),
            "extern" => Ok(RefType::Extern),
            s => Err(format!("{} is not a valid reference type", s).into()),
        }
    }
}

impl fmt::Display for RefType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(match self {
            RefType::Func => "func",
            RefType::Extern => "extern",
        })
    }
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Clone)]
pub struct MemArg {
    /// Inmediate static offset of the store operator
    pub static_offset: u64,
    /// Inmediate align value of the store operator
    pub align: u8,
    /// Inmediate mem value of the store operator
    pub mem: u32,
}

impl fmt::Display for MemArg {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}.{}.{}", self.static_offset, self.align, self.mem)
    }
}

impl FromStr for MemArg {
    type Err = String;
    fn from_str(s: &str) -> Result<MemArg, String> {
        let mut parts = s.split('.');
        let static_offset = parts
            .next()
            .ok_or_else(|| format!("expected more static instr info"))?
            .parse::<u64>()
            .map_err(|e| e.to_string())?;
        let align = parts
            .next()
            .ok_or_else(|| format!("expected more static instr info"))?
            .parse::<u8>()
            .map_err(|e| e.to_string())?;
        let mem = parts
            .next()
            .ok_or_else(|| format!("expected more static instr info"))?
            .parse::<u32>()
            .map_err(|e| e.to_string())?;

        if parts.next().is_some() {
            return Err(format!("too much info after instruction"));
        }

        Ok(MemArg {
            static_offset,
            align,
            mem,
        })
    }
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Clone)]
pub struct MemArgLane {
    /// Other memarg information
    pub memarg: MemArg,
    /// Immediate value for the simd vector lane this is operating on.
    pub lane: u8,
}

impl fmt::Display for MemArgLane {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}.{}", self.memarg, self.lane)
    }
}

impl FromStr for MemArgLane {
    type Err = String;
    fn from_str(s: &str) -> Result<MemArgLane, String> {
        let mut parts = s.rsplitn(2, '.');
        let lane = parts
            .next()
            .ok_or_else(|| format!("expected more static instr info"))?
            .parse::<u8>()
            .map_err(|e| e.to_string())?;
        let memarg = parts
            .next()
            .ok_or_else(|| format!("expected more static instr info"))?
            .parse::<MemArg>()
            .map_err(|e| e.to_string())?;
        if parts.next().is_some() {
            return Err(format!("too much info after instruction"));
        }

        Ok(MemArgLane { lane, memarg })
    }
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Clone)]
pub struct Shuffle {
    pub indices: [u8; 16],
}

impl fmt::Display for Shuffle {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for (i, idx) in self.indices.iter().enumerate() {
            if i != 0 {
                write!(f, ".")?;
            }
            write!(f, "{idx}")?;
        }
        Ok(())
    }
}

impl FromStr for Shuffle {
    type Err = String;
    fn from_str(s: &str) -> Result<Shuffle, String> {
        let indices = s
            .split('.')
            .map(|part| part.parse::<u8>())
            .collect::<Result<Vec<_>, _>>()
            .map_err(|_| format!("failed to parse integer"))?;
        let indices: &[u8; 16] = indices
            .as_slice()
            .try_into()
            .map_err(|_| format!("wrong number of lanes"))?;

        Ok(Shuffle { indices: *indices })
    }
}

trait Children: Sized {
    fn from(list: Vec<Id>) -> Result<Self, String>;
    fn as_slice(&self) -> &[Id];
    fn as_slice_mut(&mut self) -> &mut [Id];
}

impl Children for Id {
    fn from(mut list: Vec<Id>) -> Result<Id, String> {
        if list.len() != 1 {
            Err(format!("expected one child node, got {}", list.len()))
        } else {
            Ok(list.remove(0))
        }
    }
    fn as_slice(&self) -> &[Id] {
        std::slice::from_ref(self)
    }
    fn as_slice_mut(&mut self) -> &mut [Id] {
        std::slice::from_mut(self)
    }
}

impl<const N: usize> Children for [Id; N] {
    fn from(list: Vec<Id>) -> Result<Self, String> {
        if let Ok(result) = list.try_into() {
            Ok(result)
        } else {
            Err(format!("expected {} child nodes", N))
        }
    }
    fn as_slice(&self) -> &[Id] {
        self
    }
    fn as_slice_mut(&mut self) -> &mut [Id] {
        self
    }
}

impl Children for Vec<Id> {
    fn from(list: Vec<Id>) -> Result<Self, String> {
        Ok(list)
    }
    fn as_slice(&self) -> &[Id] {
        self
    }
    fn as_slice_mut(&mut self) -> &mut [Id] {
        self
    }
}

fn parse_pair<T: FromStr, U: FromStr>(s: &str) -> Result<(T, U), String>
where
    T::Err: fmt::Display,
    U::Err: fmt::Display,
{
    let mut parts = s.split('.');
    let t = parts
        .next()
        .ok_or_else(|| format!("expected more static instr info"))?
        .parse::<T>()
        .map_err(|e| e.to_string())?;
    let u = parts
        .next()
        .ok_or_else(|| format!("expected more static instr info"))?
        .parse::<U>()
        .map_err(|e| e.to_string())?;

    if parts.next().is_some() {
        return Err(format!("too much info after instruction"));
    }

    Ok((t, u))
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Clone)]
pub struct TableCopy {
    pub src: u32,
    pub dst: u32,
}

impl fmt::Display for TableCopy {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}.{}", self.src, self.dst)
    }
}

impl FromStr for TableCopy {
    type Err = String;

    fn from_str(s: &str) -> Result<TableCopy, String> {
        let (src, dst) = parse_pair(s)?;
        Ok(TableCopy { src, dst })
    }
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Clone)]
pub struct MemoryCopy {
    pub src: u32,
    pub dst: u32,
}

impl fmt::Display for MemoryCopy {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}.{}", self.src, self.dst)
    }
}

impl FromStr for MemoryCopy {
    type Err = String;

    fn from_str(s: &str) -> Result<MemoryCopy, String> {
        let (src, dst) = parse_pair(s)?;
        Ok(MemoryCopy { src, dst })
    }
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Clone)]
pub struct TableInit {
    pub table: u32,
    pub segment: u32,
}

impl fmt::Display for TableInit {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}.{}", self.table, self.segment)
    }
}

impl FromStr for TableInit {
    type Err = String;

    fn from_str(s: &str) -> Result<TableInit, String> {
        let (table, segment) = parse_pair(s)?;
        Ok(TableInit { table, segment })
    }
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Clone)]
pub struct MemoryInit {
    pub memory: u32,
    pub segment: u32,
}

impl fmt::Display for MemoryInit {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}.{}", self.memory, self.segment)
    }
}

impl FromStr for MemoryInit {
    type Err = String;

    fn from_str(s: &str) -> Result<MemoryInit, String> {
        let (memory, segment) = parse_pair(s)?;
        Ok(MemoryInit { memory, segment })
    }
}

#[cfg(test)]
mod tests {
    use super::{Lang, MemArg, RefType};
    use egg::{Id, Language};

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
                Lang::I32Load(
                    MemArg {
                        mem: 1,
                        align: 0,
                        static_offset: 120,
                    },
                    Id::from(0),
                ),
                Lang::I32Load(
                    MemArg {
                        mem: 1,
                        align: 0,
                        static_offset: 0,
                    },
                    Id::from(0),
                ),
            ],
            [Lang::LocalGet(0), Lang::LocalGet(1)],
            [Lang::RefNull(RefType::Func), Lang::RefNull(RefType::Extern)],
        ];
        for [l, r] in pairs {
            assert_eq!(l.matches(&r), false);
        }
    }
}
