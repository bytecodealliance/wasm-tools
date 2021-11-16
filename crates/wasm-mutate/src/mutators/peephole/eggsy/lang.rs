use egg::Id;
use std::fmt::Display;
use std::str::FromStr;

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Clone)]
pub enum Lang {
    I32Add([Id; 2]),
    I64Add([Id; 2]),
    I32Sub([Id; 2]),
    I64Sub([Id; 2]),
    I32Mul([Id; 2]),
    I64Mul([Id; 2]),
    I32And([Id; 2]),
    I64And([Id; 2]),
    I32Or([Id; 2]),
    I64Or([Id; 2]),
    I32Xor([Id; 2]),
    I64Xor([Id; 2]),
    I32Shl([Id; 2]),
    I64Shl([Id; 2]),
    I32ShrU([Id; 2]),
    I64ShrU([Id; 2]),
    I32DivU([Id; 2]),
    I64DivU([Id; 2]),
    I32DivS([Id; 2]),
    I64DivS([Id; 2]),
    I32ShrS([Id; 2]),
    I64ShrS([Id; 2]),
    I32RotR([Id; 2]),
    I64RotR([Id; 2]),
    I32RotL([Id; 2]),
    I64RotL([Id; 2]),
    I32RemS([Id; 2]),
    I64RemS([Id; 2]),
    I32RemU([Id; 2]),
    I64RemU([Id; 2]),
    // testop
    I32Eqz([Id; 1]),
    I64Eqz([Id; 1]),
    // relop
    I32Eq([Id; 2]),
    I64Eq([Id; 2]),
    I32Ne([Id; 2]),
    I64Ne([Id; 2]),

    I32LtS([Id; 2]),
    I64LtS([Id; 2]),
    I32LtU([Id; 2]),
    I64LtU([Id; 2]),

    I32GtS([Id; 2]),
    I64GtS([Id; 2]),

    I32GtU([Id; 2]),
    I64GtU([Id; 2]),
    I32LeS([Id; 2]),
    I64LeS([Id; 2]),

    I32LeU([Id; 2]),
    I64LeU([Id; 2]),
    I32GeS([Id; 2]),
    I64GeS([Id; 2]),
    I32GeU([Id; 2]),
    I64GeU([Id; 2]),

    I32Popcnt([Id; 1]),
    I64Popcnt([Id; 1]),

    // Locals
    // Idx and value
    LocalTee(u32, Id),
    // Idx and value
    LocalSet(u32, Id),
    LocalGet(u32),

    // Globals
    // Idx and value
    GlobalSet(u32, Id),
    GlobalGet(u32),
    // conversion operators
    Wrap([Id; 1]),

    // more conversion
    I32Extend8S([Id; 1]),
    I64Extend8S([Id; 1]),
    I32Extend16S([Id; 1]),
    I64Extend16S([Id; 1]),
    I64Extend32S([Id; 1]),
    I64ExtendI32S([Id; 1]),
    I64ExtendI32U([Id; 1]),

    // The u32 argument should be the function index
    Call(usize, Vec<Id>),
    Drop([Id; 1]),
    // Memory operations
    I32Load {
        static_offset: u64,
        align: u8,
        mem: u32,
        offset: Id,
    },
    I64Load {
        static_offset: u64,
        align: u8,
        mem: u32,
        offset: Id,
    },
    I32Store {
        static_offset: u64,
        align: u8,
        mem: u32,
        value_and_offset: [Id; 2],
    },
    I64Store {
        static_offset: u64,
        align: u8,
        mem: u32,
        value_and_offset: [Id; 2],
    },
    // TODO add the others

    // Custom mutation operations and instructions
    //
    /*
        This operation represent a random number, if its used, every time is should represent the same random number
    */
    RandI32,
    RandI64,
    /*
        This instructions is used to define unknown operands, for example when the value can come from the join of several basic blocks in a dfg
    */
    Undef,
    /*
        Takes one constant operand and turn it into a sum of two random numbers whihch sum is the operand `i32.const x = i32.const r + i32.const (x - r) `
    */
    UnfoldI32(Id),
    UnfoldI64(Id),
    // End of custom mutation operations and instructions
    I32(i32),
    I64(i64),
}

impl Display for Lang {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Lang::LocalSet(idx, _) => f.write_str(&format!("local.set {}", idx)),
            Lang::LocalTee(idx, _) => f.write_str(&format!("local.tee {}", idx)),
            Lang::I32(val) => f.write_str(&format!("{}_i32", val)),
            Lang::I64(val) => f.write_str(&format!("{}_i64", val)),
            _ => f.write_str(&format!("{:?}", self)),
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
            // Add other types here
            _ => Err(format!("Invalid type annotation for {:?}", op_str)),
        }
    }

    /// Parses index operations written in the textual form
    /// local.(get|set|tee).$i
    /// global.(get|set).$i
    ///
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
    /// (i32|i64|...).(store|load).$static_offset.$align.$mem
    ///
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
            // TODO, add the other mem operations here
            _ => Err(format!("Invalid index based operation {:?}", op_str)),
        }
    }

    /// Parses call operators
    /// call.$i
    ///
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
}

impl egg::Language for Lang {
    fn matches(&self, other: &Self) -> bool {
        ::std::mem::discriminant(self) == ::std::mem::discriminant(other)
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
            | Lang::I32Add(operands) => operands,
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
            | Lang::I32Popcnt(operands) => operands,
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
            } => value_and_offset,
            Lang::RandI32 => &[],
            Lang::RandI64 => &[],
            Lang::Undef => &[],
            Lang::UnfoldI32(operand) | Lang::UnfoldI64(operand) => std::slice::from_ref(operand),
            Lang::I32(_) => &[],
            Lang::I64(_) => &[],
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
            | Lang::I32Add(operands) => operands,

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
            } => value_and_offset,
            Lang::RandI32 => &mut [],
            Lang::RandI64 => &mut [],
            Lang::Undef => &mut [],
            Lang::I32(_) => &mut [],
            Lang::I64(_) => &mut [],
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
            //unop
            "i64.eqz" => Ok(Lang::I64Eqz([children[0]])),
            "i32.eqz" => Ok(Lang::I32Eqz([children[0]])),

            "i32.popcnt" => Ok(Lang::I32Popcnt([children[0]])),
            "i64.popcnt" => Ok(Lang::I64Popcnt([children[0]])),
            // more conversion
            "i32.extend8_s" => Ok(Lang::I32Extend8S([children[0]])),
            "i64.extend8_s" => Ok(Lang::I64Extend8S([children[0]])),
            "i32.extend16_s" => Ok(Lang::I32Extend16S([children[0]])),
            "i64.extend16_s" => Ok(Lang::I64Extend16S([children[0]])),
            "i64.extend32_s" => Ok(Lang::I64Extend32S([children[0]])),
            "i64.extendi32_s" => Ok(Lang::I64ExtendI32S([children[0]])),
            "i64.extendi32_u" => Ok(Lang::I64ExtendI32U([children[0]])),
            "i32.wrapi64" => Ok(Lang::Wrap([children[0]])),
            // Special nodes :)
            "i32.unfold" => Ok(Lang::UnfoldI32(children[0])),
            "i64.unfold" => Ok(Lang::UnfoldI64(children[0])),
            "i32.rand" => Ok(Lang::RandI32),
            "i64.rand" => Ok(Lang::RandI64),
            "undef" => Ok(Lang::Undef),
            "drop" => Ok(Lang::Drop([children[0]])),
            _ => Lang::parse_call(op_str, &children)
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
