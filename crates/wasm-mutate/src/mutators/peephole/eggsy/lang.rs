use egg::{define_language, Id, Symbol};

// Language definition for a piece of Wasm
define_language! {
    pub enum Lang {
        // Define Wasm language here progressively
        "add" = Add([Id; 2]),
        "sub" = Sub([Id; 2]),
        "mul" = Mul([Id; 2]),
        "and" = And([Id; 2]),
        "or" = Or([Id; 2]),
        "xor" = Xor([Id; 2]),
        "shl" = Shl([Id; 2]),
        "shr_u" = ShrU([Id; 2]),
        "div_u" = DivU([Id; 2]),
        "div_s" = DivS([Id; 2]),
        "shr_s" = ShrS([Id; 2]),
        "popcnt" = Popcnt(Id),
        "drop" = Drop,
        // Memory operations
        "load" = ILoad([Id;4]), // dynamic offset, offset, align mem value
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

        Num(i64),
        // NB: must be last since variants are parsed in order.
        Symbol(Symbol),

        // Use the following to internally pass arguments and not parsed as number constants
        // Since variants will be parsed in order, this wont be created directly from `parse`
        Arg(u64), // TODO, create this as a children-having instruction
    }
}

impl Default for Lang {
    fn default() -> Self {
        Lang::Undef
    }
}
