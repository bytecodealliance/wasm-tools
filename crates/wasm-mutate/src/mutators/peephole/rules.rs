//! Rewriting rules for the peephole mutator.
//!
//! New rewriting rules should be declared inside the
//! [`get_rules`](/src/wasm_mutate/mutators/peephole/rules.rs.html#17) function.

use egg::{rewrite, Id, Rewrite, Subst};

use crate::{module::PrimitiveTypeInfo, WasmMutate};

use super::{
    eggsy::{analysis::PeepholeMutationAnalysis, lang::Lang},
    PeepholeMutator, EG,
};

impl PeepholeMutator {
    /// Returns the rewriting rules.
    ///
    /// Define new fules here for the peephole mutator.
    pub fn get_rules(&self, config: &WasmMutate) -> Vec<Rewrite<Lang, PeepholeMutationAnalysis>> {
        let mut rules = vec![];

        // Various identities.
        if config.reduce {
            // NB: these only go one way when we are reducing.
            rules.extend(vec![
                rewrite!("i32.or--1"; "(i32.or ?x i32.const.-1)" => "i32.const.-1"),
                rewrite!("i64.or--1"; "(i64.or ?x i64.const.-1)" => "i64.const.-1"),
                rewrite!("i32.or-x-x"; "(i32.or ?x ?x)" => "?x"),
                rewrite!("i64.or-x-x"; "(i64.or ?x ?x)" => "?x"),
                rewrite!("i32.and-x-x"; "(i32.and ?x ?x)" => "?x"),
                rewrite!("i64.and-x-x"; "(i64.and ?x ?x)" => "?x"),
                rewrite!("select-same-branches"; "(select ?y ?y ?x)" => "?y"),
                rewrite!("i32.sub-0"; "(i32.sub ?x i32.const.0)" => "?x"),
                rewrite!("i64.sub-0"; "(i64.sub ?x i64.const.0)" => "?x"),
                rewrite!("i32.mul-x-1"; "(i32.mul ?x i32.const.1)" => "?x"),
                rewrite!("i64.mul-x-1"; "(i64.mul ?x i64.const.1)" => "?x"),
                rewrite!("f32.mul-x-1"; "(f32.mul ?x f32.const.1,0)" => "?x"),
                rewrite!("f64.mul-x-1"; "(f64.mul ?x f64.const.1,0)" => "?x"),
                rewrite!("i32.add-x-0"; "(i32.add ?x i32.const.0)" => "?x"),
                rewrite!("i64.add-x-0"; "(i64.add ?x i64.const.0)" => "?x"),
                rewrite!("f32-add-x-0"; "(f32.add ?x f32.const.0,0)" => "?x"),
                rewrite!("f64.add-x-0"; "(f64.add ?x f64.const.0,0)" => "?x"),
                rewrite!("i32.xor-x-0"; "(i32.xor ?x i32.const.0)" => "?x"),
                rewrite!("i64.xor-x-0"; "(i64.xor ?x i64.const.0)" => "?x"),
                rewrite!("i32.eq-x-0"; "(i32.eq ?x i32.const.0)" => "(i32.eqz ?x)"),
                rewrite!("i64.eq-x-0"; "(i64.eq ?x i64.const.0)" => "(i64.eqz ?x)"),
                rewrite!("i32.shl-by-0"; "(i32.shl ?x i32.const.0)" => "?x"),
                rewrite!("i64.shl-by-0"; "(i64.shl ?x i64.const.0)" => "?x"),
                rewrite!("i32.shr_u-by-0"; "(i32.shr_u ?x i32.const.0)" => "?x"),
                rewrite!("i64.shr_u-by-0"; "(i64.shr_u ?x i64.const.0)" => "?x"),
                rewrite!("i32.shr_s-by-0"; "(i32.shr_s ?x i32.const.0)" => "?x"),
                rewrite!("i64.shr_s-by-0"; "(i64.shr_s ?x i64.const.0)" => "?x"),
            ]);
        } else {
            rules.extend(vec![
                rewrite!("i32.or--1"; "(i32.or ?x i32.const.-1)" => "i32.const.-1"),
                rewrite!("i64.or--1"; "(i64.or ?x i64.const.-1)" => "i64.const.-1"),
            ]);

            rules.extend(rewrite!(
                "i32.or-x-x";
                "(i32.or ?x ?x)" <=> "?x"
                    if self.is_type("?x", PrimitiveTypeInfo::I32)
            ));
            rules.extend(rewrite!(
                "i64.or-x-x";
                "(i64.or ?x ?x)" <=> "?x"
                    if self.is_type("?x", PrimitiveTypeInfo::I64)
            ));

            rules.extend(rewrite!(
                "i32.and-x-x";
                "(i32.and ?x ?x)" <=> "?x"
                    if self.is_type("?x", PrimitiveTypeInfo::I32)
            ));
            rules.extend(rewrite!(
                "i64.and-x-x";
                "(i64.and ?x ?x)" <=> "?x"
                    if self.is_type("?x", PrimitiveTypeInfo::I64)
            ));

            rules.push(rewrite!("select-same-branches"; "(select ?y ?y ?x)" => "?y"));

            rules.extend(rewrite!(
                "i32.sub-0";
                "(i32.sub ?x i32.const.0)" <=> "?x"
                    if self.is_type("?x", PrimitiveTypeInfo::I32)
            ));
            rules.extend(rewrite!(
                "i64.sub-0";
                "(i64.sub ?x i64.const.0)" <=> "?x"
                    if self.is_type("?x", PrimitiveTypeInfo::I64)
            ));

            rules.extend(rewrite!(
                "i32.mul-x-1";
                "?x" <=> "(i32.mul ?x i32.const.1)"
                    if self.is_type("?x", PrimitiveTypeInfo::I32)
            ));
            rules.extend(rewrite!(
                "i64.mul-x-1";
                "?x" <=> "(i64.mul ?x i64.const.1)"
                    if self.is_type("?x", PrimitiveTypeInfo::I64)
            ));

            rules.extend(rewrite!(
                "f32.mul-x-1";
                "?x" <=> "(f32.mul ?x f32.const.1,0)"
                    if self.is_type("?x", PrimitiveTypeInfo::F32)
            ));
            rules.extend(rewrite!(
                "f64.mul-x-1";
                "?x" <=> "(f64.mul ?x f64.const.1,0)"
                    if self.is_type("?x", PrimitiveTypeInfo::F64)
            ));

            rules.extend(rewrite!(
                "i32.add-x-0";
                "?x" <=> "(i32.add ?x i32.const.0)"
                    if self.is_type("?x", PrimitiveTypeInfo::I32)
            ));
            rules.extend(rewrite!(
                "i64.add-x-0";
                "?x" <=> "(i64.add ?x i64.const.0)"
                    if self.is_type("?x", PrimitiveTypeInfo::I64)
            ));
            rules.extend(rewrite!(
                "f32-add-x-0";
                "?x" <=> "(f32.add ?x f32.const.0,0)"
                    if self.is_type("?x", PrimitiveTypeInfo::F32)
            ));
            rules.extend(rewrite!(
                "f64.add-x-0";
                "?x" <=> "(f64.add ?x f64.const.0,0)"
                    if self.is_type("?x", PrimitiveTypeInfo::F64)
            ));

            rules.extend(rewrite!(
                "i32.xor-x-0";
                "?x" <=> "(i32.xor ?x i32.const.0)"
                    if self.is_type("?x", PrimitiveTypeInfo::I32)
            ));
            rules.extend(rewrite!(
                "i64.xor-x-0";
                "?x" <=> "(i64.xor ?x i64.const.0)"
                    if self.is_type("?x", PrimitiveTypeInfo::I64)
            ));

            rules.extend(rewrite!(
                "i32.eq-x-0";
                "(i32.eq ?x i32.const.0)" <=> "(i32.eqz ?x)"
                    if self.is_type("?x", PrimitiveTypeInfo::I32)
            ));
            rules.extend(rewrite!(
                "i64.eq-x-0";
                "(i64.eq ?x i64.const.0)" <=> "(i64.eqz ?x)"
                    if self.is_type("?x", PrimitiveTypeInfo::I64)
            ));

            rules.extend(rewrite!(
                "i32.shl-by-0";
                "(i32.shl ?x i32.const.0)" <=> "?x"
                    if self.is_type("?x", PrimitiveTypeInfo::I32)
            ));
            rules.extend(rewrite!(
                "i64.shl-by-0";
                "(i64.shl ?x i64.const.0)" <=> "?x"
                    if self.is_type("?x", PrimitiveTypeInfo::I64)
            ));

            rules.extend(rewrite!(
                "i32.shr_u-by-0";
                "(i32.shr_u ?x i32.const.0)" <=> "?x"
                    if self.is_type("?x", PrimitiveTypeInfo::I32)
            ));
            rules.extend(rewrite!(
                "i64.shr_u-by-0";
                "(i64.shr_u ?x i64.const.0)" <=> "?x"
                    if self.is_type("?x", PrimitiveTypeInfo::I64)
            ));

            rules.extend(rewrite!(
                "i32.shr_s-by-0";
                "(i32.shr_s ?x i32.const.0)" <=> "?x"
                    if self.is_type("?x", PrimitiveTypeInfo::I32)
            ));
            rules.extend(rewrite!(
                "i64.shr_s-by-0";
                "(i64.shr_s ?x i64.const.0)" <=> "?x"
                    if self.is_type("?x", PrimitiveTypeInfo::I64)
            ));
        }

        // A bunch of commutativity rules.
        //
        // Even though these don't reduce code size themselves, they can help
        // other rules apply, so we add them even when we aren't just reducing.

        rules.extend(rewrite!("i32.add-commutes"; "(i32.add ?x ?y)" <=> "(i32.add ?y ?x)"));
        rules.extend(rewrite!("i64.add-commutes"; "(i64.add ?x ?y)" <=> "(i64.add ?y ?x)"));
        rules.extend(rewrite!("f32.add-commutes"; "(f32.add ?x ?y)" <=> "(f32.add ?y ?x)"));
        rules.extend(rewrite!("f64.add-commutes"; "(f64.add ?x ?y)" <=> "(f64.add ?y ?x)"));

        rules.extend(rewrite!("i32.mul-commutes"; "(i32.mul ?x ?y)" <=> "(i32.mul ?y ?x)" ));
        rules.extend(rewrite!("i64.mul-commutes"; "(i64.mul ?x ?y)" <=> "(i64.mul ?y ?x)" ));
        rules.extend(rewrite!("f32.mul-commutes"; "(f32.mul ?x ?y)" <=> "(f32.mul ?y ?x)" ));
        rules.extend(rewrite!("f64.mul-commutes"; "(f64.mul ?x ?y)" <=> "(f64.mul ?y ?x)" ));

        rules.extend(rewrite!("i32.and-commutes"; "(i32.and ?x ?y)" <=> "(i32.and ?y ?x)"));
        rules.extend(rewrite!("i64.and-commutes"; "(i64.and ?x ?y)" <=> "(i64.and ?y ?x)"));

        rules.extend(rewrite!("i32.or-commutes"; "(i32.or ?x ?y)" <=> "(i32.or ?y ?x)"));
        rules.extend(rewrite!("i64.or-commutes"; "(i64.or ?x ?y)" <=> "(i64.or ?y ?x)"));

        rules.extend(rewrite!("i32.xor-commutes"; "(i32.xor ?x ?y)" <=> "(i32.xor ?y ?x)"));
        rules.extend(rewrite!("i64.xor-commutes"; "(i64.xor ?x ?y)" <=> "(i64.xor ?y ?x)"));

        rules.extend(rewrite!("i32.eq-commutes"; "(i32.eq ?x ?y)" <=> "(i32.eq ?y ?x)"));
        rules.extend(rewrite!("i64.eq-commutes"; "(i64.eq ?x ?y)" <=> "(i64.eq ?y ?x)"));

        // A bunch of associativity rules.
        //
        // Even though these don't reduce code size themselves, they can help
        // other rules apply, so we add them even when we aren't just reducing.

        rules.extend(rewrite!(
            "i32.mul-associates";
            "(i32.mul ?x (i32.mul ?y ?z))" <=> "(i32.mul (i32.mul ?x ?y) ?z)"
        ));
        rules.extend(rewrite!(
            "i64.mul-associates";
            "(i64.mul ?x (i64.mul ?y ?z))" <=> "(i64.mul (i64.mul ?x ?y) ?z)"
        ));

        rules.extend(rewrite!(
            "i32.add-associates";
            "(i32.add ?x (i32.add ?y ?z))" <=> "(i32.add (i32.add ?x ?y) ?z)"
        ));
        rules.extend(rewrite!(
            "i64.add-associates";
            "(i64.add ?x (i64.add ?y ?z))" <=> "(i64.add (i64.add ?x ?y) ?z)"
        ));

        rules.extend(rewrite!(
            "i32.and-associates";
            "(i32.and ?x (i32.and ?y ?z))" <=> "(i32.and (i32.and ?x ?y) ?z)"
        ));
        rules.extend(rewrite!(
            "i64.and-associates";
            "(i64.and ?x (i64.and ?y ?z))" <=> "(i64.and (i64.and ?x ?y) ?z)"
        ));

        rules.extend(rewrite!(
            "i32.or-associates";
            "(i32.or ?x (i32.or ?y ?z))" <=> "(i32.or (i32.or ?x ?y) ?z)"
        ));
        rules.extend(rewrite!(
            "i64.or-associates";
            "(i64.or ?x (i64.or ?y ?z))" <=> "(i64.or (i64.or ?x ?y) ?z)"
        ));

        rules.extend(rewrite!(
            "i32.xor-associates";
            "(i32.xor ?x (i32.xor ?y ?z))" <=> "(i32.xor (i32.xor ?x ?y) ?z)"
        ));
        rules.extend(rewrite!(
            "i64.xor-associates";
            "(i64.xor ?x (i64.xor ?y ?z))" <=> "(i64.xor (i64.xor ?x ?y) ?z)"
        ));

        rules.extend(rewrite!(
            "i32.eq-associates";
            "(i32.eq ?x (i32.eq ?y ?z))" <=> "(i32.eq (i32.eq ?x ?y) ?z)"
        ));
        rules.extend(rewrite!(
            "i64.eq-associates";
            "(i64.eq ?x (i64.eq ?y ?z))" <=> "(i64.eq (i64.eq ?x ?y) ?z)"
        ));

        // Undoing `x * 2 ==> x << 1` strength reduction, etc...
        if !config.reduce {
            rules.extend(
                rewrite!("i32.mul-by-2"; "(i32.shl ?x i32.const.1)" <=> "(i32.mul ?x i32.const.2)"),
            );
            rules.extend(
                rewrite!("i64.mul-by-2"; "(i64.shl ?x i64.const.1)" <=> "(i64.mul ?x i64.const.2)"),
            );
            rules.extend(
                rewrite!("i32.mul-by-4"; "(i32.shl ?x i32.const.2)" <=> "(i32.mul ?x i32.const.4)"),
            );
            rules.extend(
                rewrite!("i64.mul-by-4"; "(i64.shl ?x i64.const.2)" <=> "(i64.mul ?x i64.const.4)"),
            );
            rules.extend(
                rewrite!("i32.mul-by-8"; "(i32.shl ?x i32.const.3)" <=> "(i32.mul ?x i32.const.8)"),
            );
            rules.extend(
                rewrite!("i64.mul-by-8"; "(i64.shl ?x i64.const.3)" <=> "(i64.mul ?x i64.const.8)"),
            );
        }

        // Invert a `select` condition and swap its consequent and alternative.
        if !config.reduce {
            rules.extend(
                rewrite!("select-invert"; "(select ?x ?y ?z)" <=> "(select ?y ?x (i32.eqz ?z))"),
            );
        }

        // Convert `x + x` into `x * 2`.
        if !config.reduce {
            rules.extend(rewrite!("i32.add-x-x"; "(i32.add ?x ?x)" <=> "(i32.mul ?x i32.const.2)"));
            rules.extend(rewrite!("i64.add-x-x"; "(i64.add ?x ?x)" <=> "(i64.mul ?x i64.const.2)"));
        }

        // Mess with dropped subexpressions.
        if !config.reduce {
            rules.extend(vec![
                rewrite!(
                    "i32.drop-x";
                    "(drop ?x)" => "(drop i32.rand)"
                        if self.is_type("?x", PrimitiveTypeInfo::I32)
                ),
                rewrite!(
                    "i64.drop-x";
                    "(drop ?x)" => "(drop i64.rand)"
                        if self.is_type("?x", PrimitiveTypeInfo::I64)
                ),
                rewrite!(
                    "f32.drop-x";
                    "(drop ?x)" => "(drop f32.const.0,0)"
                        if self.is_type("?x", PrimitiveTypeInfo::F32)
                ),
                rewrite!(
                    "f64.drop-x";
                    "(drop ?x)" => "(drop f32.const.0,0)"
                        if self.is_type("?x", PrimitiveTypeInfo::F64)
                ),
            ]);
        }

        // Insert some stack-neutral sub-expressions.
        if !config.reduce {
            rules.extend(vec![
                rewrite!("container-nop-x"; "?x" => "(container nop ?x)"),
                rewrite!("container-x-nop"; "?x" => "(container ?x nop)"),
                rewrite!("container-drop-i32.rand-x"; "?x" => "(container (drop i32.rand) ?x)"),
                rewrite!("container-drop-i64.rand-x"; "?x" => "(container (drop i64.rand) ?x)"),
                rewrite!("container-x-drop-i32.rand"; "?x" => "(container ?x (drop i32.rand))"),
                rewrite!("container-x-drop-i64.rand"; "?x" => "(container ?x (drop i64.rand))"),
            ]);
        }

        // Spill expressions to a new global and then use the global's value.
        if !config.reduce {
            let max_globals = 100_000;
            rules.extend(vec![
                rewrite!(
                    "i32.use_of_global";
                    "?x" => "(i32.use_of_global ?x)"
                        if self.is_type("?x", PrimitiveTypeInfo::I32)
                        if self.global_count_less_than(config, max_globals)
                ),
                rewrite!(
                    "i64.use_of_global";
                    "?x" => "(i64.use_of_global ?x)"
                        if self.is_type("?x", PrimitiveTypeInfo::I64)
                        if self.global_count_less_than(config, max_globals)
                ),
                rewrite!(
                    "f32.use_of_global";
                    "?x" => "(f32.use_of_global ?x)"
                        if self.is_type("?x", PrimitiveTypeInfo::F32)
                        if self.global_count_less_than(config, max_globals)
                ),
                rewrite!(
                    "f64.use_of_global";
                    "?x" => "(f64.use_of_global ?x)"
                        if self.is_type("?x", PrimitiveTypeInfo::F64)
                        if self.global_count_less_than(config, max_globals)
                ),
            ]);
        }

        // Unfolding constants.
        if !config.reduce {
            rules.extend(vec![
                rewrite!(
                    "i32.unfold";
                    "?x" => "(i32.unfold ?x)"
                        if self.is_const("?x")
                        if self.is_type("?x", PrimitiveTypeInfo::I32)
                ),
                rewrite!(
                    "i64.unfold";
                    "?x" => "(i64.unfold ?x)"
                        if self.is_const("?x")
                        if self.is_type("?x", PrimitiveTypeInfo::I64)
                ),
            ]);
        }

        // If we aren't preserving semantics, then go wild with mutations. Only
        // thing we need to preserve is that we are emitting valid, well-typed
        // Wasm.
        if !config.preserve_semantics {
            // Replace an expression with either zero, one, or a random constant.
            rules.extend(vec![
                rewrite!(
                    "replace-with-i32-1";
                    "?x" => "i32.const.1"
                        if self.is_type("?x", PrimitiveTypeInfo::I32)
                ),
                rewrite!(
                    "replace-with-i64-1";
                    "?x" => "i64.const.1"
                        if self.is_type("?x", PrimitiveTypeInfo::I64)
                ),
                rewrite!(
                    "replace-with-f32-1.0";
                    "?x" => "f32.const.1,0"
                        if self.is_type("?x", PrimitiveTypeInfo::F32)
                ),
                rewrite!(
                    "replace-with-f64-1.0";
                    "?x" => "f64.const.1,0"
                        if self.is_type("?x", PrimitiveTypeInfo::F64)
                ),
                rewrite!(
                    "replace-with-i32-0";
                    "?x" => "i32.const.0"
                        if self.is_type("?x", PrimitiveTypeInfo::I32)
                ),
                rewrite!(
                    "replace-with-i64-0";
                    "?x" => "i64.const.0"
                        if self.is_type("?x", PrimitiveTypeInfo::I64)
                ),
                rewrite!(
                    "replace-with-f32-0";
                    "?x" => "f32.const.0,0"
                        if self.is_type("?x", PrimitiveTypeInfo::F32)
                ),
                rewrite!(
                    "replace-with-f64-0";
                    "?x" => "f64.const.0,0"
                        if self.is_type("?x", PrimitiveTypeInfo::F64)
                ),
                rewrite!(
                    "replace-with-v128-0";
                    "?x" => "v128.const.0"
                        if self.is_type("?x", PrimitiveTypeInfo::V128)
                ),
                rewrite!(
                    "replace-with-i32.rand";
                    "?x" => "i32.rand"
                        if self.is_type("?x", PrimitiveTypeInfo::I32)
                ),
                rewrite!(
                    "replace-with-i64.rand";
                    "?x" => "i64.rand"
                        if self.is_type("?x", PrimitiveTypeInfo::I64)
                ),
                rewrite!(
                    "replace-with-f32.rand";
                    "?x" => "f32.rand"
                        if self.is_type("?x", PrimitiveTypeInfo::F32)
                ),
                rewrite!(
                    "replace-with-f64.rand";
                    "?x" => "f64.rand"
                        if self.is_type("?x", PrimitiveTypeInfo::F64)
                ),
                rewrite!(
                    "replace-with-ref-null-func";
                    "?x" => "ref.null.func"
                        if self.is_type("?x", PrimitiveTypeInfo::FuncRef)
                ),
                rewrite!(
                    "replace-with-ref-null-extern";
                    "?x" => "ref.null.extern"
                        if self.is_type("?x", PrimitiveTypeInfo::ExternRef)
                ),
                // Try to outright delete some instructions and containers to
                // help make input even smaller in wasm-shrink cases.
                rewrite!(
                    "remove-drop";
                    "(drop ?x)" => "(container)"
                ),
                rewrite!(
                    "remove-nop";
                    "nop" => "(container)"
                ),
                rewrite!(
                    "remove-global.set.0";
                    "(global.set.0 ?x)" => "(container)"
                ),
                rewrite!(
                    "remove-global.set.1";
                    "(global.set.1 ?x)" => "(container)"
                ),
                rewrite!(
                    "remove-elem.drop.0";
                    "(elem.drop.0)" => "(container)"
                ),
                rewrite!(
                    "remove-elem.drop.1";
                    "(elem.drop.1)" => "(container)"
                ),
                rewrite!(
                    "remove-data.drop.0";
                    "(data.drop.0)" => "(container)"
                ),
                rewrite!(
                    "remove-data.drop.1";
                    "(data.drop.1)" => "(container)"
                ),
            ]);

            if !config.reduce {
                // `x <=> x + 1`
                rules.extend(rewrite!(
                    "i32.add-1";
                    "?x" <=> "(i32.add ?x i32.const.1)"
                        if self.is_type("?x", PrimitiveTypeInfo::I32)
                ));
                rules.extend(rewrite!(
                    "i64.add-1";
                    "?x" <=> "(i64.add ?x i64.const.1)"
                        if self.is_type("?x", PrimitiveTypeInfo::I64)
                ));

                // `x <=> x - 1`
                rules.extend(rewrite!(
                    "i32.sub-1";
                    "?x" <=> "(i32.sub ?x i32.const.1)"
                        if self.is_type("?x", PrimitiveTypeInfo::I32)
                ));
                rules.extend(rewrite!(
                    "i64.sub-1";
                    "?x" <=> "(i64.sub ?x i64.const.1)"
                        if self.is_type("?x", PrimitiveTypeInfo::I64)
                ));

                // `x <=> x & 1`
                rules.extend(rewrite!(
                    "i32.and-1";
                    "?x" <=> "(i32.and ?x i32.const.1)"
                        if self.is_type("?x", PrimitiveTypeInfo::I32)
                ));
                rules.extend(rewrite!(
                    "i64.and-1";
                    "?x" <=> "(i64.and ?x i64.const.1)"
                        if self.is_type("?x", PrimitiveTypeInfo::I64)
                ));

                // `x <=> x | 1`
                rules.extend(rewrite!(
                    "i32.or-1";
                    "?x" <=> "(i32.or ?x i32.const.1)"
                        if self.is_type("?x", PrimitiveTypeInfo::I32)
                ));
                rules.extend(rewrite!(
                    "i64.or-1";
                    "?x" <=> "(i64.or ?x i64.const.1)"
                        if self.is_type("?x", PrimitiveTypeInfo::I64)
                ));

                // `x <=> x ^ 1`
                rules.extend(rewrite!(
                    "i32.xor-1";
                    "?x" <=> "(i32.xor ?x i32.const.1)"
                        if self.is_type("?x", PrimitiveTypeInfo::I32)
                ));
                rules.extend(rewrite!(
                    "i64.xor-1";
                    "?x" <=> "(i64.xor ?x i64.const.1)"
                        if self.is_type("?x", PrimitiveTypeInfo::I64)
                ));

                // `x <=> x << 1`
                rules.extend(rewrite!(
                    "i32.shl-1";
                    "?x" <=> "(i32.shl ?x i32.const.1)"
                        if self.is_type("?x", PrimitiveTypeInfo::I32)
                ));
                rules.extend(rewrite!(
                    "i64.shl-1";
                    "?x" <=> "(i64.shl ?x i64.const.1)"
                        if self.is_type("?x", PrimitiveTypeInfo::I64)
                ));

                // `x <=> x >> 1`
                rules.extend(rewrite!(
                    "i32.shr_u-1";
                    "?x" <=> "(i32.shr_u ?x i32.const.1)"
                        if self.is_type("?x", PrimitiveTypeInfo::I32)
                ));
                rules.extend(rewrite!(
                    "i64.shr_u-1";
                    "?x" <=> "(i64.shr_u ?x i64.const.1)"
                        if self.is_type("?x", PrimitiveTypeInfo::I64)
                ));
                rules.extend(rewrite!(
                    "i32.shr_s-1";
                    "?x" <=> "(i32.shr_s ?x i32.const.1)"
                        if self.is_type("?x", PrimitiveTypeInfo::I32)
                ));
                rules.extend(rewrite!(
                    "i64.shr_s-1";
                    "?x" <=> "(i64.shr_s ?x i64.const.1)"
                        if self.is_type("?x", PrimitiveTypeInfo::I64)
                ));
            }
        }

        rules
    }

    /// Checks if a variable is of a specific type.
    fn is_type(
        &self,
        vari: &'static str,
        t: PrimitiveTypeInfo,
    ) -> impl Fn(&mut EG, Id, &Subst) -> bool {
        move |egraph: &mut EG, _, subst| {
            let var = vari.parse();
            match var {
                Ok(var) => {
                    let eclass = &egraph[subst[var]];
                    match &eclass.data {
                        Some(d) => d.tpe == t,
                        None => false,
                    }
                }
                Err(_) => false,
            }
        }
    }

    /// Check whether the given variable is a constant.
    fn is_const(&self, vari: &'static str) -> impl Fn(&mut EG, Id, &Subst) -> bool {
        move |egraph: &mut EG, _, subst| {
            let var = vari.parse();
            match var {
                Ok(var) => {
                    let eclass = &egraph[subst[var]];
                    if eclass.nodes.len() == 1 {
                        let node = &eclass.nodes[0];
                        match node {
                            Lang::I32(_) => true,
                            Lang::I64(_) => true,
                            Lang::F32(_) => true,
                            Lang::F64(_) => true,
                            _ => false,
                        }
                    } else {
                        false
                    }
                }
                Err(_) => false,
            }
        }
    }

    /// Condition that check for number of module globals
    fn global_count_less_than(
        &self,
        config: &WasmMutate,
        allowed: usize,
    ) -> impl Fn(&mut EG, Id, &Subst) -> bool {
        let count = config.info().get_global_count();
        move |_egraph: &mut EG, _, _subst| count < allowed
    }
}
