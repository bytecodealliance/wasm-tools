use egg::{rewrite, Rewrite};

use crate::{module::PrimitiveTypeInfo, WasmMutate};

use super::{
    eggsy::{analysis::PeepholeMutationAnalysis, lang::Lang},
    PeepholeMutator,
};

impl PeepholeMutator {
    /// Returns the rewriting rules
    /// Define new fules here for the peephole mutator
    pub fn get_rules(&self, config: &WasmMutate) -> Vec<Rewrite<Lang, PeepholeMutationAnalysis>> {
        let mut rules = vec![
            rewrite!("unfold-2";  "?x" => "(i32.unfold ?x)" if self.is_const("?x") if self.is_type("?x", PrimitiveTypeInfo::I32) ),
            rewrite!("unfold-3";  "?x" => "(i64.unfold ?x)" if self.is_const("?x") if self.is_type("?x", PrimitiveTypeInfo::I64) ),
            // TODO Pop out the offset

            // Rmove the full subtree of the drop
            rewrite!("drop1";  "(drop ?x)" => "(drop i32.rand)" if self.is_type("?x", PrimitiveTypeInfo::I32)),
            rewrite!("drop2";  "(drop ?x)" => "(drop i64.rand)" if self.is_type("?x", PrimitiveTypeInfo::I64)),
            rewrite!("drop3";  "(drop ?x)" => "(drop 0_f32)" if self.is_type("?x", PrimitiveTypeInfo::F32)),
            rewrite!("drop4";  "(drop ?x)" => "(drop 0_f32)" if self.is_type("?x", PrimitiveTypeInfo::F64)),
        ];
        // Use a custom instruction-mutator for this
        // This specific rewriting rule has a condition, it should be appplied if the operand is a constant
        rules.extend(rewrite!("strength-undo";  "(i32.shl ?x 1_i32)" <=> "(i32.mul ?x 2_i32)"));
        rules.extend(rewrite!("strength-undo01";  "(i64.shl ?x 1_i64)" <=> "(i64.mul ?x 2_i64)"));

        rules.extend(rewrite!("strength-undo1";  "(i32.shl ?x 2_i32)" <=> "(i32.mul ?x 4_i32)"));
        rules.extend(rewrite!("strength-undo12";  "(i64.shl ?x 2_i64)" <=> "(i64.mul ?x 4_i64)"));

        rules.extend(rewrite!("strength-undo2";  "(i32.shl ?x 3_i32)" <=> "(i32.mul ?x 8_i32)"));
        rules.extend(rewrite!("strength-undo22";  "(i64.shl ?x 3_i64)" <=> "(i64.mul ?x 8_i64)"));

        rules.extend(rewrite!("strength-undo3";  "(i32.shl ?x 0_i32)" <=> "?x" if self.is_type("?x", PrimitiveTypeInfo::I32) ));
        rules.extend(rewrite!("strength-undo31";  "(i64.shl ?x 0_i64)" <=> "?x" if self.is_type("?x", PrimitiveTypeInfo::I64)  ));

        rules.extend(rewrite!("add-1";  "(i32.add ?x ?x)" <=> "(i32.mul ?x 2_i32)"));
        rules.extend(rewrite!("add-12";  "(i64.add ?x ?x)" <=> "(i64.mul ?x 2_i64)"));

        rules.extend(rewrite!("idempotent-1";  "?x" <=> "(i32.or ?x ?x)" if self.is_type("?x", PrimitiveTypeInfo::I32)));
        rules.extend(rewrite!("idempotent-12";  "?x" <=> "(i64.or ?x ?x)" if self.is_type("?x", PrimitiveTypeInfo::I64)));

        rules.extend(rewrite!("idempotent-2";  "?x" <=> "(i32.and ?x ?x)" if self.is_type("?x", PrimitiveTypeInfo::I32)));
        rules.extend(rewrite!("idempotent-21";  "?x" <=> "(i64.and ?x ?x)" if self.is_type("?x", PrimitiveTypeInfo::I64)));

        rules.extend(rewrite!("commutative-1";  "(i32.add ?x ?y)" <=> "(i32.add ?y ?x)"));
        rules.extend(rewrite!("commutative-12";  "(i64.add ?x ?y)" <=> "(i64.add ?y ?x)"));
        rules.extend(rewrite!("commutative-13";  "(f32.add ?x ?y)" <=> "(f32.add ?y ?x)"));
        rules.extend(rewrite!("commutative-14";  "(f64.add ?x ?y)" <=> "(f64.add ?y ?x)"));

        rules.extend(rewrite!("commutative-2";  "(i32.mul ?x ?y)" <=> "(i32.mul ?y ?x)" ));
        rules.extend(rewrite!("commutative-22";  "(i64.mul ?x ?y)" <=> "(i64.mul ?y ?x)" ));
        rules.extend(rewrite!("commutative-23";  "(f32.mul ?x ?y)" <=> "(f32.mul ?y ?x)" ));
        rules.extend(rewrite!("commutative-224";  "(f64.mul ?x ?y)" <=> "(f64.mul ?y ?x)" ));

        rules
            .extend(rewrite!("associative-2";  "(i32.mul ?x (i32.mul ?y ?z))" <=> "(i32.mul (i32.mul ?x ?y) ?z)" ));
        rules
            .extend(rewrite!("associative-22";  "(i64.mul ?x (i64.mul ?y ?z))" <=> "(i64.mul (i64.mul ?x ?y) ?z)" ));

        rules
            .extend(rewrite!("associative-1";  "(i32.add ?x (i32.add ?y ?z))" <=> "(i32.add (i32.add ?x ?y) ?z)" ));
        rules
            .extend(rewrite!("associative-12";  "(i64.add ?x (i64.add ?y ?z))" <=> "(i64.add (i64.add ?x ?y) ?z)" ));

        rules.extend(rewrite!("idempotent-3";  "?x" <=> "(i32.mul ?x 1_i32)" if self.is_type("?x", PrimitiveTypeInfo::I32)));
        rules.extend(rewrite!("idempotent-31";  "?x" <=> "(i64.mul ?x 1_i64)" if self.is_type("?x", PrimitiveTypeInfo::I64)));

        rules.extend(rewrite!("idempotent-311";  "?x" <=> "(f32.mul ?x 1_f32)" if self.is_type("?x", PrimitiveTypeInfo::F32)));
        rules.extend(rewrite!("idempotent-312";  "?x" <=> "(f64.mul ?x 1_f64)" if self.is_type("?x", PrimitiveTypeInfo::F64)));

        rules.extend(rewrite!("idempotent-4";  "?x" <=> "(i32.add ?x 0_i32)" if self.is_type("?x", PrimitiveTypeInfo::I32)));
        rules.extend(rewrite!("idempotent-41";  "?x" <=> "(i64.add ?x 0_i64)" if self.is_type("?x", PrimitiveTypeInfo::I64)));
        rules.extend(rewrite!("idempotent-42";  "?x" <=> "(f32.add ?x 0_f32)" if self.is_type("?x", PrimitiveTypeInfo::F32)));
        rules.extend(rewrite!("idempotent-43";  "?x" <=> "(f64.add ?x 0_f64)" if self.is_type("?x", PrimitiveTypeInfo::F64)));

        rules.extend(rewrite!("idempotent-5";  "?x" <=> "(i32.xor ?x 0_i32)" if self.is_type("?x", PrimitiveTypeInfo::I32)));
        rules.extend(rewrite!("idempotent-51";  "?x" <=> "(i64.xor ?x 0_i64)" if self.is_type("?x", PrimitiveTypeInfo::I64)));

        rules.extend(rewrite!("idempotent-6"; "(i32.eqz ?x)" <=> "(i32.eq ?x 0_i32)" if self.is_type("?x", PrimitiveTypeInfo::I32)));
        rules.extend(rewrite!("idempotent-61"; "(i64.eqz ?x)" <=> "(i64.eq ?x 0_i64)" if self.is_type("?x", PrimitiveTypeInfo::I64)));

        rules.extend(rewrite!("commutative-3"; "(i32.eq ?x ?y)" <=> "(i32.eq ?y ?x)" if self.is_type("?x", PrimitiveTypeInfo::I32)));
        rules.extend(rewrite!("commutative-31"; "(i64.eq ?x ?y)" <=> "(i64.eq ?y ?x)" if self.is_type("?x", PrimitiveTypeInfo::I64)));

        // Overflow rules
        if !config.preserve_semantics {
            // Correctness attraction
            rules.push(rewrite!("correctness-1";  "?x" => "(i32.add ?x 1_i32)" if self.is_const("?x") if self.is_type("?x", PrimitiveTypeInfo::I32)));
            rules.push(rewrite!("correctness-12";  "?x" => "(i64.add ?x 1_i64)" if self.is_const("?x") if self.is_type("?x", PrimitiveTypeInfo::I64)));
        }

        rules
    }
}
