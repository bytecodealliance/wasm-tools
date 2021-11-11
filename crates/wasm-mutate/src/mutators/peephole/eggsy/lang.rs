use egg::{define_language, Id, Symbol};

// Language definition for a piece of Wasm
define_language! {
    pub enum Lang {
        // Define Wasm language here progressively
        "i32add" = I32Add([Id; 2]),
        "i64add" = I64Add([Id; 2]),
        "i32sub" = I32Sub([Id; 2]),
        "i32sub" = I64Sub([Id; 2]),
        "i32mul" = I32Mul([Id; 2]),
        "i64mul" = I64Mul([Id; 2]),
        "i32and" = I32And([Id; 2]),
        "i64and" = I64And([Id; 2]),
        "i32or" = I32Or([Id; 2]),
        "i64or" = I64Or([Id; 2]),
        "i32xor" = I32Xor([Id; 2]),
        "i64xor" = I64Xor([Id; 2]),
        "i32shl" = I32Shl([Id; 2]),
        "i64shl" = I64Shl([Id; 2]),
        "i32shr_u" = I32ShrU([Id; 2]),
        "i64shr_u" = I64ShrU([Id; 2]),
        "i32div_u" = I32DivU([Id; 2]),
        "i64div_u" = I64DivU([Id; 2]),
        "i32div_s" = I32DivS([Id; 2]),
        "i64div_s" = I64DivS([Id; 2]),
        "i32shr_s" = I32ShrS([Id; 2]),
        "i64shr_s" = I64ShrS([Id; 2]),
        "i32rotr" = I32RotR([Id; 2]),
        "i64rotr" = I64RotR([Id; 2]),
        "i32rotl" = I32RotL([Id; 2]),
        "i64rotl" = I64RotL([Id; 2]),
        "i32rem_s" = I32RemS([Id; 2]),
        "i64rem_s" = I64RemS([Id; 2]),
        "i32rem_u" = I32RemU([Id; 2]),
        "i64rem_u" = I64RemU([Id; 2]),
        // testop
        "i32eqz" = I32Eqz([Id; 1]),
        "i64eqz" = I64Eqz([Id; 1]),
        // relop
        "i32eq" = I32Eq([Id; 2]),
        "i64eq" = I64Eq([Id; 2]),
        "i32ne" = I32Ne([Id; 2]),
        "i64ne" = I64Ne([Id; 2]),

        "i32lt_s" = I32LtS([Id; 2]),
        "i64lt_s" = I64LtS([Id; 2]),
        "i32lt_u" = I32LtU([Id; 2]),
        "i64lt_u" = I64LtU([Id; 2]),

        "i32gt_s" = I32GtS([Id; 2]),
        "i64gt_s" = I64GtS([Id; 2]),

        "i32gt_u" = I32GtU([Id; 2]),
        "i64gt_u" = I64GtU([Id; 2]),
        "i32le_s" = I32LeS([Id; 2]),
        "i64le_s" = I64LeS([Id; 2]),

        "i32le_u" = I32LeU([Id; 2]),
        "i64le_u" = I64LeU([Id; 2]),
        "i32ge_s" = I32GeS([Id; 2]),
        "i64ge_s" = I64GeS([Id; 2]),
        "i32ge_u" = I32GeU([Id; 2]),
        "i64ge_u" = I64GeU([Id; 2]),

        "i32popcnt" = I32Popcnt([Id; 1]),
        "i64popcnt" = I64Popcnt([Id; 1]),

        // Locals
        "tee" = Tee([Id; 2]),
        "set" = Set([Id; 1]),
        "global_set" = GlobalSet([Id; 1]),
        // conversion operators
        "wrap" = Wrap([Id; 1]),

        // more conversion
        "i32extend8s" = I32Extend8S([Id; 1]),
        "i64extend8s" = I64Extend8S([Id; 1]),
        "i32extend16s" = I32Extend16S([Id; 1]),
        "i64extend16s" = I64Extend16S([Id; 1]),
        "i64extend32s" = I64Extend32S([Id; 1]),
        "i64extendi32s" = I64ExtendI32S([Id; 1]),
        "i64extendi32u" = I64ExtendI32U([Id; 1]),

        // The firsts Id should be the function index
        "call" = Call(Vec<Id>),
        "drop" = Drop([Id; 1]),
        // Memory operations
        "i32load" = I32Load([Id;4]), // dynamic offset, offset, align mem value
        "i64load" = I64Load([Id;4]), // dynamic offset, offset, align mem value
        // TODO add the others

        // Custom mutation operations and instructions
        //
        /*
            This operation represent a random number, if its used, every time is should represent the same random number
        */
        "rand" = Rand,
        /*
            This instructions is used to define unknown operands, for example when the value can come from the join of several basic blocks in a dfg
        */
        "undef" = Undef,
        /*
            Takes one constant operand and turn it into a sum of two random numbers whihch sum is the operand `i32.const x = i32.const r + i32.const (x - r) `
        */
        "unfold" = Unfold(Id),
        // End of custom mutation operations and instructions

        Const(i64),
        I32(i32),
        I64(i64),
        // NB: must be last since variants are parsed in order.
        Symbol(Symbol),

        // Use the following to internally pass arguments that dont need to be
        // parsed as number constants. Since variants will be parsed in order,
        // this wont be created directly from `parse`
        Arg(u64), // TODO, create this as a children-having instruction
    }
}

impl Default for Lang {
    fn default() -> Self {
        Lang::Undef
    }
}
