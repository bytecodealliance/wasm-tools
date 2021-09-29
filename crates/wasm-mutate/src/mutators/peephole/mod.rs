use crate::mutators::peephole::eggsy::analysis::PeepholeMutationAnalysis;
use crate::mutators::peephole::eggsy::lang::Lang;
use egg::{
    define_language, rewrite, Analysis, CostFunction, EClass, EGraph, Extractor, Id, Language,
    Pattern, RecExpr, Rewrite, Runner, Searcher, Symbol,
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

use self::eggsy::{NoPopcnt, RandomExtractor};

use super::Mutator;

pub mod cfg;
pub mod eggsy;
pub struct PeepholeMutator;

// Code mutator, function id, operator id
type MutationContext = (Function, u32);
// Helper type to return operator and ofsset inside the byte stream
type TupleType<'a> = (Operator<'a>, usize);
impl PeepholeMutator {
    // Map operator to Lang expr and the corresponding instruction
    // This method should return the expression (egg language) expected from the operator
    // TODO, add small CFG information

    fn operator2term(
        &self,
        operators: &Vec<TupleType>,
        at: usize,
    ) -> Option<(&str, HashMap<&str, Range>, usize)> {
        let (op, _) = &operators[at];
        match op {
            Operator::I32Load { .. }
            | Operator::I64Load { .. }
            | Operator::F32Load { .. }
            | Operator::F64Load { .. }
            | Operator::I32Load16S { .. }
            | Operator::I32Load16U { .. }
            | Operator::I32Load8S { .. }
            | Operator::I32Load8U { .. }
            | Operator::I64Load32S { .. }
            | Operator::I64Load32U { .. }
            | Operator::I64Load8S { .. }
            | Operator::I64Load8U { .. } => {
                Some(("(i.load ?x)", [].iter().cloned().collect(), at + 1))
            }
            Operator::I32Const { .. } => Some((
                "?x",
                [(
                    "?x",
                    Range {
                        start: at,
                        end: at + 1,
                    },
                )]
                .iter()
                .cloned()
                .collect(),
                at + 1,
            )),
            Operator::I32Shl => {
                let (previous2, _) = &operators[at - 2];
                let (previous, _) = &operators[at - 1];
                if let Operator::I32Const { .. } = previous2 {
                    if let Operator::I32Const { value } = previous {
                        match value {
                            1 => {
                                return Some((
                                    "(i32.shl ?x 1)",
                                    [(
                                        "?x",
                                        Range {
                                            start: at - 2,
                                            end: at - 1,
                                        },
                                    )]
                                    .iter()
                                    .cloned()
                                    .collect(),
                                    at - 2,
                                ))
                            }
                            _ => return None,
                        }
                    }
                }

                None
            }
            _ => None,
        }
    }

    // lang to wasm adaptor
    fn write2wasm(
        &self,
        info: &ModuleInfo,
        rnd: &mut rand::prelude::SmallRng,
        current: usize,
        insertion_point: usize,
        id_to_node: &Vec<&Lang>,
        operands: &Vec<Vec<Id>>,
        operators: &Vec<TupleType>,
        newfunc: &mut Function,
        symbolmap: &HashMap<&str, Range>,
        random_pool: Option<i32>,
    ) -> Result<Option<Range>> {
        let root = id_to_node[current];

        let new_random = if let Lang::Rand = root {
            Some(rnd.gen())
        } else {
            random_pool
        };

        // Write operands first, stack way
        let operand_cfg = operands[current]
            .iter()
            .map(|&idx| {
                self.write2wasm(
                    info,
                    rnd,
                    usize::from(idx),
                    insertion_point,
                    id_to_node,
                    operands,
                    operators,
                    newfunc,
                    symbolmap,
                    new_random,
                )
            })
            .collect::<Result<Vec<Option<Range>>>>()?;

        match root {
            Lang::I32Add(_) => {
                newfunc.instruction(Instruction::I32Add);
            }
            Lang::I32Sub(_) => {
                newfunc.instruction(Instruction::I32Sub);
            }
            Lang::I32Mul(_) => {
                newfunc.instruction(Instruction::I32Mul);
            }
            Lang::I32And(_) => {
                newfunc.instruction(Instruction::I32And);
            }
            Lang::I32Or(_) => {
                newfunc.instruction(Instruction::I32Or);
            }
            Lang::I32Xor(_) => {
                newfunc.instruction(Instruction::I32Xor);
            }
            Lang::I32Shl(_) => {
                newfunc.instruction(Instruction::I32Shl);
            }
            Lang::I32ShrU(_) => {
                newfunc.instruction(Instruction::I32ShrU);
            }
            Lang::I32Popcnt(_) => {
                newfunc.instruction(Instruction::I32Popcnt);
            }
            Lang::Rand => {
                // Check if the random was set...otherwise use the rnd
                println!("{:?}", random_pool);
                match random_pool {
                    Some(r) => {
                        newfunc.instruction(Instruction::I32Const(r));
                    }
                    None => {
                        newfunc.instruction(Instruction::I32Const(160268115));
                    }
                }
            }
            Lang::Unfold(_) => {
                // Get CFG value and check, if its a constant, do the unfolding
                // This will fail if the operand was not a Symbol
                assert_eq!(operand_cfg.len(), 1);

                let operators_range = operand_cfg[0].unwrap();
                let operators = &operators[operators_range.start..operators_range.end];
                assert_eq!(operators.len(), 1);
                let (i32const, _) = &operators[0];

                if let Operator::I32Const { value } = i32const {
                    let r: i32 = rnd.gen();

                    // Drop previous
                    newfunc.instruction(Instruction::Drop);

                    newfunc.instruction(Instruction::I32Const(r));
                    newfunc.instruction(Instruction::I32Const((Wrapping(*value) - Wrapping(r)).0));
                    newfunc.instruction(Instruction::I32Add);
                }

                println!("{:?}", newfunc);
            }
            Lang::ILoad(_) => {
                // Do nothing
                let operators = &operators[insertion_point..=insertion_point + 1/* take to the next operator to save the offset */];
                let range = (operators[0].1, operators[1].1);

                // Copy the mem operation
                let raw_data = &info.get_code_section().data[range.0..range.1];
                newfunc.raw(raw_data.iter().copied());
            }
            Lang::Prev => {
                // Do nothing, bypass
            }
            Lang::I32Const(v) => {
                newfunc.instruction(Instruction::I32Const(*v));
            }
            Lang::Symbol(s1) => {
                // Copy from the CFG
                // The data below can be taken from the input wasm directly
                let operators_range = symbolmap[&s1.as_str()].clone();
                let operators = &operators[operators_range.start..operators_range.end + 1/* take to the next operator to save the offset */];

                // Copy exactly the same bnytes from the original wasm
                let raw_range = (
                    operators[0].1, // offset
                    operators[operators.len() - 1].1,
                );

                println!("raw range {:?}", raw_range);

                let raw_data = &info.get_code_section().data[raw_range.0..raw_range.1];
                println!("raw data {:?}", raw_data);

                newfunc.raw(raw_data.iter().copied());

                return Ok(Some(operators_range));
            }
        }

        Ok(None)
    }

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
                .collect::<wasmparser::Result<Vec<TupleType>>>()?;
            let operatorscount = operators.len();
            let opcode_to_mutate = rnd.gen_range(0, operatorscount);

            for oidx in (opcode_to_mutate..operatorscount).chain(0..opcode_to_mutate) {
                //let mut applicable = Vec::new();
                //let (operator, offset) = &operators[oidx];
                let eterm = self.operator2term(&operators, oidx);
                println!("{:?} {:?}", eterm, operators[oidx]);
                if let Some((eterm, symbolmap, start_at)) = eterm {
                    // Create an e-graph from this operator mapping
                    let start = eterm.parse().unwrap();
                    println!("{:?}", start);
                    let runner = Runner::default().with_expr(&start).run(rules);
                    let mut egraph = runner.egraph;

                    // In theory this will return the Id of the operator eterm
                    let root = egraph.add_expr(&start);

                    // This cost function could be replaced by a custom weighted probability, for example
                    // we could modify the cost function based on the previous mutation/rotation outcome
                    let cf = NoPopcnt;
                    let extractor = RandomExtractor::new(&egraph, cf);

                    // The mutation is doable if the equivalence class for this eterm is not empty
                    println!("{:?} {:?}", egraph[root].nodes, egraph[root]);
                    //if egraph[root].nodes.len() > 1 /* the current root plus another one */ {

                    // The magic happens here :)
                    let (id_to_node, operands) =
                        extractor.generate_random_tree(rnd, root, 0 /* only 1 for now */)?;
                    println!("{:?} {:?}", id_to_node, operands);

                    // This is a hack, TODO check is we can generate random from this root
                    if operands.len() == 1 && operands[0].len() == 0 {
                        continue;
                    }

                    // Create a new function using the previous locals
                    let mut newfunc = self.copy_locals(reader)?;
                    // Copying code previous to the code match
                    let previous =
                        &code_section.data[operatorsrange.start..operatorsrange.start + start_at];
                    newfunc.raw(previous.iter().copied());

                    // Pool for rand instruction
                    let i: i32 = rnd.gen();
                    // Translate lang expr to wasm
                    self.write2wasm(
                        info,
                        rnd,
                        0, // The root of the tree
                        oidx,
                        &id_to_node,
                        &operands,
                        &operators,
                        &mut newfunc,
                        &symbolmap,
                        Some(i),
                    )?;

                    let (_, offset) = operators[oidx + 1];
                    let ending = &code_section.data[offset..operatorsrange.end];
                    newfunc.raw(ending.iter().copied());

                    return Ok((newfunc, fidx));
                    //}
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
            let mut reader = sectionreader.read()?;
            if fidx == function_to_mutate {
                codes.function(&new_function);
            } else {
                codes.raw(&code_section.data[reader.range().start..reader.range().end]);
            }
        }

        let module = info.replace_section(info.code.unwrap(), &codes);
        Ok(module)
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
        let rules: &[Rewrite<Lang, PeepholeMutationAnalysis>] = &[
            rewrite!("unfold-1";  "?x" => "(i32.add rand (i32.sub rand ?x))"),
            rewrite!("unfold-2";  "?x" => "(unfold ?x)"), // Use a custom instruction-mutator for this
            rewrite!("strength-undo";  "(i32.shl ?x 1)" => "(i32.mul ?x ?x)"),
            rewrite!("strength-undo1";  "(i32.shl ?x 2)" => "(i32.mul ?x (i32.mul ?x ?x))"),
            rewrite!("strength-undo2";  "(i32.shl ?x 3)" => "(i32.mul ?x (i32.mul ?x (i32.mul ?x ?x)))"),
            rewrite!("idempotent-1";  "?x" => "(i32.or ?x ?x)"),
            rewrite!("idempotent-2";  "?x" => "(i32.and ?x ?x)"),
            rewrite!("mem-load-shift";  "(i.load ?x)" => "(i.load (i32.add skip rand))"),
        ];

        self.mutate_with_rules(config, rnd, info, rules)
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
        operators: Vec<TupleType>,
        funcreader: FunctionBody,
        body_range: wasmparser::Range,
        function_data: &[u8],
    ) -> Result<Function>;

    /// Returns if this mutator can be applied to the opcode at index i
    fn can_mutate<'a>(
        &self,
        config: &'a WasmMutate,
        operators: &Vec<TupleType<'a>>,
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
                        .collect::<wasmparser::Result<Vec<TupleType>>>()
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
    use egg::{rewrite, Rewrite};
    use rand::{rngs::SmallRng, SeedableRng};

    use super::PeepholeMutationAnalysis;

    #[test]
    fn test_peep_unfold1() {
        let rules: &[Rewrite<super::Lang, PeepholeMutationAnalysis>] =
            &[rewrite!("unfold-1";  "?x" => "(i32.add rand (i32.sub rand ?x))")];

        test_peephole_mutator(
            r#"
            (module
                (func (export "exported_func") (result i32) (local i32 i32)
                    i32.const 56
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
                  i32.const 56
                  i32.const 1
                  i32.const 160268115
                  i32.sub
                  i32.const 160268115
                  i32.add
                  i32.shl)
                (export "exported_func" (func 0)))
            "#,
        );
    }

    #[test]
    fn test_peep_unfold2() {
        let rules: &[Rewrite<super::Lang, PeepholeMutationAnalysis>] =
            &[rewrite!("unfold-2";  "?x" => "(unfold ?x)")];

        test_peephole_mutator(
            r#"
            (module
                (func (export "exported_func") (result i32) (local i32 i32)
                    i32.const 56
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
                  i32.const 56
                  i32.const 1
                  drop
                  i32.const 991484282
                  i32.const -991484281
                  i32.add
                  i32.shl)
                (export "exported_func" (func 0)))
            "#,
        );
    }

    #[test]
    fn test_peep_strength() {
        let rules: &[Rewrite<super::Lang, PeepholeMutationAnalysis>] =
            &[rewrite!("strength-undo";  "(i32.shl ?x 1)" => "(i32.mul ?x ?x)")];

        test_peephole_mutator(
            r#"
        (module
            (func (export "exported_func") (result i32) (local i32 i32)
                i32.const 56
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
              i32.const 56
              i32.const 56
              i32.mul)
            (export "exported_func" (func 0)))"#,
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
              i32.const 56
              i32.const 1
              i32.const 1
              i32.or
              i32.shl)
            (export "exported_func" (func 0)))
        "#,
        );
    }

    #[test]
    fn test_peep_mem_shift() {
        let rules: &[Rewrite<super::Lang, PeepholeMutationAnalysis>] =
            &[rewrite!("mem-load-shift";  "(i.load ?x)" => "(i.load (i32.add skip rand))")];

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
                  local.get 0
                  i32.const -683260416
                  i32.add
                  i32.load)
                (memory (;0;) 1)
                (export "exported_func" (func 0)))
        "#,
        );
    }

    fn test_peephole_mutator(
        original: &str,
        rules: &[Rewrite<super::Lang, PeepholeMutationAnalysis>],
        expected: &str,
    ) {
        let wasmmutate = WasmMutate::default();
        let original = &wat::parse_str(original).unwrap();

        let mutator = PeepholeMutator; // the string is empty

        let mut info = wasmmutate.get_module_info(original).unwrap();
        let can_mutate = mutator.can_mutate(&wasmmutate, &info).unwrap();

        let mut rnd = SmallRng::seed_from_u64(0);

        assert_eq!(can_mutate, true);

        let mutated = mutator
            .mutate_with_rules(&wasmmutate, &mut rnd, &mut info, rules)
            .unwrap();

        let mut validator = wasmparser::Validator::new();
        let mutated_bytes = &mutated.finish();
        crate::validate(&mut validator, mutated_bytes);
        let text = wasmprinter::print_bytes(mutated_bytes).unwrap();

        println!("{}", text);

        let expected_bytes = &wat::parse_str(expected).unwrap();
        let expectedtext = wasmprinter::print_bytes(expected_bytes).unwrap();

        assert_eq!(text, expectedtext);
    }
}
