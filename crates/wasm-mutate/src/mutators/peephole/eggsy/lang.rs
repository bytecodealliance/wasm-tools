use crate::mutators::peephole::RecExpr;
use egg::{define_language, Id, Symbol};

// egg language definition
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

        // Memory operations
        "i.load" = ILoad(Id),
        // TODO add the others

        // Custom mutation operations and instructions
        "rand" = Rand, // This operation represent a random number, if its used, every time is should represent the same random number
        "unfold" = Unfold([Id; 1]),
        "skip" = Prev, // Use this operator as a helper, it means to respect previously wasm code probably to use it as an operand
        I32Const(i32),

        // NB: must be last since variants are parsed in order.
        Symbol(Symbol),
    }
}
