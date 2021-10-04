use crate::mutators::peephole::RecExpr;
use egg::{define_language, Id, Symbol};

/// Language definition for a piece of Wasm
define_language! {
    pub enum Lang {
        // Define Wasm language here progressively
        "i32.add" = I32Add([Id; 2]),
        "i32.sub" = I32Sub([Id; 2]),
        "i32.mul" = I32Mul([Id; 2]),
        "i32.and" = I32And([Id; 2]),
        "i32.or" = I32Or([Id; 2]),
        "i32.xor" = I32Xor([Id; 2]),
        "i32.shl" = I32Shl([Id; 2]),
        "i32.shr_u" = I32ShrU([Id; 2]),
        "i32.popcnt" = I32Popcnt(Id),
        "drop" = Drop,
        // Memory operations
        "i.load" = ILoad(Id),
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
        "unfold" = Unfold([Id; 1]),
        // End of custom mutation operations and instructions

        I32Const(i32),
        // NB: must be last since variants are parsed in order.
        Symbol(Symbol),
    }
}
