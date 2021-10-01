use crate::mutators::peephole::eggsy::lang::Lang;
use crate::mutators::peephole::eggsy::{analysis::PeepholeMutationAnalysis, Encoder};
use egg::{
    define_language, rewrite, Analysis, AstSize, CostFunction, EClass, EGraph, Extractor, Id,
    Language, Pattern, RecExpr, Rewrite, Runner, Searcher, Subst, Symbol,
};
use rand::{
    prelude::{IteratorRandom, SliceRandom, SmallRng},
    Rng,
};
use std::{cmp::Ordering, collections::HashMap, hash::Hash, num::Wrapping};
use wasm_encoder::{CodeSection, Function, Instruction, MemArg, Module, ValType};
use wasmparser::{BinaryReaderError, CodeSectionReader, FunctionBody, Operator, Range};

use crate::{
    module::{map_operator, map_type},
    ModuleInfo, Result, WasmMutate,
};

use self::{dfg::DFGIcator, eggsy::RandomExtractor};

use super::Mutator;

pub mod dfg;
pub mod eggsy;

pub struct PeepholeMutator;
type EG = egg::EGraph<Lang, PeepholeMutationAnalysis>;

// Code mutator, function id, operator id
type MutationContext = (Function, u32);

// Helper type to return operator and ofsset inside the byte stream
type OperatorAndByteOffset<'a> = (Operator<'a>, usize);
impl PeepholeMutator {
    fn copy_locals(&self, reader: FunctionBody) -> Result<Function> {
        // Create the new function
        let mut localreader = reader.get_locals_reader().unwrap();
        // Get current locals and map to encoder types
        let mut local_count = 0;
        let mut current_locals = (0..localreader.get_count())
            .map(|f| {
                let (count, ty) = localreader.read().unwrap();
                local_count += count;
                (count, map_type(ty).unwrap())
            })
            .collect::<Vec<(u32, ValType)>>();

        Ok(Function::new(current_locals /*copy locals here*/))
    }

    fn random_mutate(
        &self,
        config: &crate::WasmMutate,
        rnd: &mut rand::prelude::SmallRng,
        info: &mut crate::ModuleInfo,
        rules: &[Rewrite<Lang, PeepholeMutationAnalysis>],
    ) -> Result<MutationContext> {
        let code_section = info.get_code_section();
        let mut sectionreader = CodeSectionReader::new(code_section.data, 0)?;
        let function_count = sectionreader.get_count();

        // Split where to start looking for mutable function
        // In theory random split will provide a mutable location faster
        let function_to_mutate = rnd.gen_range(0, function_count);
        let all_readers = (0..function_count)
            .map(|_| sectionreader.read().unwrap())
            .collect::<Vec<FunctionBody>>();
        for fidx in (function_to_mutate..function_count).chain(0..function_to_mutate) {
            let reader = all_readers[fidx as usize];
            let operatorreader = reader.get_operators_reader()?;
            let operatorsrange = operatorreader.reader.range();
            let operators = operatorreader
                .into_iter_with_offsets()
                .collect::<wasmparser::Result<Vec<OperatorAndByteOffset>>>()?;
            let operatorscount = operators.len();
            let opcode_to_mutate = rnd.gen_range(0, operatorscount);

            for oidx in (opcode_to_mutate..operatorscount).chain(0..opcode_to_mutate) {
                let mut dfg = DFGIcator::new();
                let basicblock = dfg.get_bb_from_operator(oidx, &operators);

                match basicblock {
                    Some(basicblock) => {
                        let minidfg = dfg.get_dfg(&operators, &basicblock)?;

                        if !minidfg.map.contains_key(&oidx) {
                            continue;
                        }
                        // This selection is also random, set this to a maximum number of tries
                        // For example, the code selected could be `i32.shl 54 1` which can be eterminized as `i32.shl ?x 1` or `i32.shl ?x ?y`
                        // But in practice we could have no rewriting rule for all of them
                        let eterm = Encoder::wasm2eterm(&minidfg, oidx, &operators, rnd);

                        match eterm {
                            Ok((eterm, symbolsmap)) => {
                                let start = eterm.parse().unwrap();

                                let runner = Runner::default().with_expr(&start).run(rules);
                                let mut egraph = runner.egraph;

                                // In theory this will return the Id of the operator eterm
                                let root = egraph.add_expr(&start);

                                // This cost function could be replaced by a custom weighted probability, for example
                                // we could modify the cost function based on the previous mutation/rotation outcome
                                let cf = AstSize;
                                let extractor = RandomExtractor::new(&egraph, cf);

                                let (id_to_node, operands) = extractor
                                    .extract_random(rnd, root, 0 /* only 1 for now */)?;

                                // There is no point in generating the same symbol
                                if operands.len() == 1 && operands[0].len() == 0 {
                                    continue;
                                }

                                // Create a new function using the previous locals
                                let mut newfunc = self.copy_locals(reader)?;

                                // Translate lang expr to wasm
                                Encoder::build_function(
                                    info,
                                    rnd,
                                    0, // The root of the tree
                                    oidx,
                                    &id_to_node,
                                    &operands,
                                    &operators,
                                    &basicblock,
                                    &mut newfunc,
                                    &symbolsmap,
                                    &minidfg,
                                )?;

                                let (_, offset) = operators[oidx + 1];
                                let ending = &code_section.data[offset..operatorsrange.end];
                                newfunc.raw(ending.iter().copied());
                                return Ok((newfunc, fidx));
                            }
                            Err(_) => {
                                continue;
                            }
                        }
                    }
                    None => {
                        continue;
                    }
                }
            }
        }

        Err(crate::Error::NotMatchingPeepholes)
    }

    /// To separate the methods will allow us to test rule by rule
    fn mutate_with_rules(
        &self,
        config: &crate::WasmMutate,
        rnd: &mut rand::prelude::SmallRng,
        info: &mut crate::ModuleInfo,
        rules: &[Rewrite<Lang, PeepholeMutationAnalysis>],
    ) -> Result<Module> {
        let (new_function, function_to_mutate) = self.random_mutate(config, rnd, info, rules)?;

        let mut codes = CodeSection::new();
        let code_section = info.get_code_section();
        let mut sectionreader = CodeSectionReader::new(code_section.data, 0)?;

        for fidx in 0..info.function_count {
            let reader = sectionreader.read()?;
            if fidx == function_to_mutate {
                codes.function(&new_function);
            } else {
                codes.raw(&code_section.data[reader.range().start..reader.range().end]);
            }
        }

        let module = info.replace_section(info.code.unwrap(), &codes);
        Ok(module)
    }

    /// Condition to apply the unfold operator
    /// check that the var is a constant
    /// Condition to apply the unfold operator
    /// check that the var is a constant
    fn is_const(&self, vari: &'static str) -> impl Fn(&mut EG, Id, &Subst) -> bool {
        move |egraph: &mut EG, _, subst| {
            let var = vari.parse();
            match var {
                Ok(var) => {
                    let eclass = &egraph[subst[var]];
                    if eclass.nodes.len() == 1 {
                        let node = &eclass.nodes[0];
                        match node {
                            Lang::I32Const(_) => true,
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
}

/// Meta mutator for peephole
impl Mutator for PeepholeMutator {
    fn mutate(
        &self,
        config: &crate::WasmMutate,
        rnd: &mut rand::prelude::SmallRng,
        info: &mut crate::ModuleInfo,
    ) -> Result<Module> {
        let mut rules = vec![
            rewrite!("unfold-2";  "?x" => "(unfold ?x)" if self.is_const("?x") ), // Use a custom instruction-mutator for this
            // This specific rewriting rule has a condition, it should be appplied if the operand is a constant
            // To do so we can write all symbols representing constants as ?c when we translate wasm to eterm
            rewrite!("strength-undo";  "(i32.shl ?x 1)" => "(i32.mul ?x ?x)"),
            rewrite!("strength-undo1";  "(i32.shl ?x 2)" => "(i32.mul ?x 2)"),
            rewrite!("strength-undo2";  "(i32.shl ?x 3)" => "(i32.mul ?x 8)"),
            rewrite!("add-1";  "(i32.add ?x ?x)" => "(i32.mul ?x 2)"),
            rewrite!("idempotent-1";  "?x" => "(i32.or ?x ?x)"),
            rewrite!("idempotent-2";  "?x" => "(i32.and ?x ?x)"),
        ];

        if !config.preserve_semantics {
            rules.push(rewrite!("mem-load-shift";  "(i.load ?x)" => "(i.load (i32.add ?x rand))"))
            // Check why this is generating a lot of the same replacements
        }

        self.mutate_with_rules(config, rnd, info, &rules)
    }

    fn can_mutate<'a>(
        &self,
        config: &'a crate::WasmMutate,
        info: &crate::ModuleInfo,
    ) -> Result<bool> {
        Ok(info.has_code() && info.function_count > 0)
    }
}

use std::fmt::Debug;
impl Debug for Box<dyn CodeMutator> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_tuple("Code mutator").finish()
    }
}
pub(crate) trait CodeMutator {
    fn mutate(
        &self,
        config: &WasmMutate,
        rnd: &mut SmallRng,
        operator_index: usize,
        operators: Vec<OperatorAndByteOffset>,
        funcreader: FunctionBody,
        body_range: wasmparser::Range,
        function_data: &[u8],
    ) -> Result<Function>;

    /// Returns if this mutator can be applied to the opcode at index i
    fn can_mutate<'a>(
        &self,
        config: &'a WasmMutate,
        operators: &Vec<OperatorAndByteOffset<'a>>,
        at: usize,
    ) -> Result<bool>;

    /// Provides the name of the mutator, mostly used for debugging purposes
    fn name(&self) -> String {
        return format!("{:?}", std::any::type_name::<Self>());
    }
}

// This macro is meant to be used for testing deep mutators
// It receives the original wat text variable, the expression returning the mutated function and the expected wat
// For an example, look at SwapCommutativeOperator
#[cfg(test)]
#[macro_export]
macro_rules! match_code_mutation {
    ($wat: ident, $mutation:expr, $expected:ident) => {{
        let original = &wat::parse_str($wat).unwrap();

        let mut parser = Parser::new(0);
        let config = WasmMutate::default();

        let mut offset = 0;

        let mut modu = Module::new();
        let mut codesection = CodeSection::new();

        loop {
            let (payload, chunksize) = match parser.parse(&original[offset..], true).unwrap() {
                Chunk::NeedMoreData(_) => {
                    panic!("This should not be reached");
                }
                Chunk::Parsed { consumed, payload } => (payload, consumed),
            };
            offset += chunksize;

            match payload {
                Payload::TypeSection(reader) => {
                    modu.section(&RawSection {
                        id: SectionId::Type.into(),
                        data: &original[reader.range().start..reader.range().end],
                    });
                }
                Payload::FunctionSection(reader) => {
                    modu.section(&RawSection {
                        id: SectionId::Function.into(),
                        data: &original[reader.range().start..reader.range().end],
                    });
                }
                Payload::ExportSection(reader) => {
                    modu.section(&RawSection {
                        id: SectionId::Export.into(),
                        data: &original[reader.range().start..reader.range().end],
                    });
                }
                Payload::CodeSectionEntry(reader) => {
                    let operatorsreader = reader.get_operators_reader().unwrap();
                    let range = operatorsreader.reader.range();
                    let operators = operatorsreader
                        .into_iter_with_offsets()
                        .collect::<wasmparser::Result<Vec<OperatorAndByteOffset>>>()
                        .unwrap();
                    let mutated = $mutation(&config, operators, reader, range, original);
                    codesection.function(&mutated);
                }
                wasmparser::Payload::End => break,
                _ => {
                    // do nothing
                }
            }
        }
        modu.section(&codesection);
        let mutated = modu.finish();
        let mut validator = wasmparser::Validator::new();
        crate::validate(&mut validator, &mutated);

        let text = wasmprinter::print_bytes(mutated).unwrap();

        // parse expected to use the same formatter
        let expected_bytes = &wat::parse_str($expected).unwrap();
        let expectedtext = wasmprinter::print_bytes(expected_bytes).unwrap();
        assert_eq!(text, expectedtext);
    }};
}

#[cfg(test)]
mod tests {
    use crate::{
        mutators::{peephole::PeepholeMutator, Mutator},
        WasmMutate,
    };
    use egg::{rewrite, Id, Rewrite, Subst};
    use rand::{rngs::SmallRng, SeedableRng};

    use super::{PeepholeMutationAnalysis, EG};
    use crate::mutators::peephole::Lang;

    /// Condition to apply the unfold operator
    /// check that the var is a constant
    fn is_const(vari: &'static str) -> impl Fn(&mut EG, Id, &Subst) -> bool {
        move |egraph: &mut EG, _, subst| {
            let var = vari.parse();

            match var {
                Ok(var) => {
                    let eclass = &egraph[subst[var]];
                    if eclass.nodes.len() == 1 {
                        let node = &eclass.nodes[0];
                        match node {
                            Lang::I32Const(_) => true,
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
    #[test]
    fn test_peep_unfold2() {
        let rules: &[Rewrite<super::Lang, PeepholeMutationAnalysis>] =
            &[rewrite!("unfold-2";  "?x" => "(unfold ?x)" if is_const("?x"))];

        test_peephole_mutator(
            r#"
            (module
                (func (export "exported_func") (result i32) (local i32 i32)
                    i32.const 56
                )
            )
            "#,
            rules,
            r#"
            (module
                (type (;0;) (func (result i32)))
                (func (;0;) (type 0) (result i32)
                  (local i32 i32)
                  i32.const -2078218253
                  i32.const -2078218309
                  i32.add)
                (export "exported_func" (func 0)))
            "#,
            3,
        );
    }

    #[test]
    fn test_peep_stack_neutral() {
        let rules: &[Rewrite<super::Lang, PeepholeMutationAnalysis>] =
            &[rewrite!("strength-undo";  "(i32.shl ?x 1)" => "(i32.mul ?x 2)")];

        test_peephole_mutator(
            r#"
        (module
            (func (export "exported_func") (result i32) (local i32 i32)
                local.get 0
                i32.const 42
                drop
                i32.const 1
                i32.shl
            )
        )
        "#,
            rules,
            r#"
            (module
                (type (;0;) (func (result i32)))
                (func (;0;) (type 0) (result i32)
                    (local i32 i32)
                    i32.const 42
                    drop
                    i32.const 2
                    local.get 0
                    i32.mul)
                (export "exported_func" (func 0)))
            "#,
            0,
        );
    }

    #[test]
    fn test_peep_strength() {
        let rules: &[Rewrite<super::Lang, PeepholeMutationAnalysis>] =
            &[rewrite!("strength-undo";  "(i32.shl ?x 1)" => "(i32.mul ?x 2)")];

        test_peephole_mutator(
            r#"
        (module
            (func (export "exported_func") (result i32) (local i32 i32)
                local.get 0
                i32.const 1
                i32.shl
            )
        )
        "#,
            rules,
            r#"
            (module
                (type (;0;) (func (result i32)))
                (func (;0;) (type 0) (result i32)
                  (local i32 i32)
                  i32.const 2
                  local.get 0
                  i32.mul)
                (export "exported_func" (func 0)))
            "#,
            27,
        );
    }

    #[test]
    fn test_peep_idem1() {
        let rules: &[Rewrite<super::Lang, PeepholeMutationAnalysis>] =
            &[rewrite!("idempotent-1";  "?x" => "(i32.or ?x ?x)")];

        test_peephole_mutator(
            r#"
        (module
            (func (export "exported_func") (result i32) (local i32 i32)
                i32.const 56
            )
        )
        "#,
            rules,
            r#"
        (module
            (type (;0;) (func (result i32)))
            (func (;0;) (type 0) (result i32)
                (local i32 i32)
                i32.const 56
                i32.const 56
                i32.or)
            (export "exported_func" (func 0)))
        "#,
            3,
        );
    }

    #[test]
    fn test_peep_mem_shift() {
        let rules: &[Rewrite<super::Lang, PeepholeMutationAnalysis>] =
            &[rewrite!("mem-load-shift";  "(i.load ?x)" => "(i.load (i32.add ?x rand))")];

        test_peephole_mutator(
            r#"
        (module
            (memory 1)
            (func (export "exported_func") (param i32) (result i32)
                local.get 0
                i32.load
            )
        )
        "#,
            rules,
            r#"
            (module
                (type (;0;) (func (param i32) (result i32)))
                (func (;0;) (type 0) (param i32) (result i32)
                  i32.const 2143720002
                  i32.const -1322847442
                  i32.const 480862779
                  i32.const -226618899
                  i32.const -186590935
                  i32.const 1446998146
                  i32.const 662639854
                  i32.const 1453188773
                  i32.const 718353904
                  i32.const 982412755
                  i32.const -1311920857
                  i32.const 1900885693
                  i32.const -815127447
                  i32.const -99060203
                  i32.const -447427166
                  i32.const 201654363
                  i32.const -1699431774
                  i32.const 1727026012
                  i32.const 1481586467
                  i32.const -1685060834
                  i32.const 136956081
                  i32.const -1155874118
                  local.get 0
                  i32.add
                  i32.add
                  i32.add
                  i32.add
                  i32.add
                  i32.add
                  i32.add
                  i32.add
                  i32.add
                  i32.add
                  i32.add
                  i32.add
                  i32.add
                  i32.add
                  i32.add
                  i32.add
                  i32.add
                  i32.add
                  i32.add
                  i32.add
                  i32.add
                  i32.add
                  i32.load)
                (memory (;0;) 1)
                (export "exported_func" (func 0)))
        "#,
            1230,
        );
    }

    fn test_peephole_mutator(
        original: &str,
        rules: &[Rewrite<super::Lang, PeepholeMutationAnalysis>],
        expected: &str,
        seed: u64,
    ) {
        let wasmmutate = WasmMutate::default();
        let original = &wat::parse_str(original).unwrap();

        let mutator = PeepholeMutator; // the string is empty

        let mut info = wasmmutate.get_module_info(original).unwrap();
        let can_mutate = mutator.can_mutate(&wasmmutate, &info).unwrap();

        let mut rnd = SmallRng::seed_from_u64(seed);

        assert_eq!(can_mutate, true);

        let mutated = mutator
            .mutate_with_rules(&wasmmutate, &mut rnd, &mut info, rules)
            .unwrap();

        let mut validator = wasmparser::Validator::new();
        let mutated_bytes = &mutated.finish();
        crate::validate(&mut validator, mutated_bytes);
        let text = wasmprinter::print_bytes(mutated_bytes).unwrap();

        let expected_bytes = &wat::parse_str(expected).unwrap();
        let expectedtext = wasmprinter::print_bytes(expected_bytes).unwrap();

        assert_eq!(text, expectedtext);
    }
}
