//! This mutator applies a random peephole transformation to the input Wasm module.
//!
//! It builds a minimal DFG (Data Flow Graph) from a random operator selected
//! from a random function inside the input Wasm. If this DFG is consistent and
//! has no side-effects, and egraph is constructed with
//! several hand-made rewriting rules. Random rewriting rules are selected and
//! the DFG is replaced by a new one. The final step assembles all together with the
//! new DFG, constructing a new equivalent Wasm binary.
//!
//!
//! To contribute with this specific mutator you can augment the defined
//! [rules][rules]. Those rewriting rules should be designed to
//! preserve the semantic of the original DFG or, in other case, should follow the filter
//! of the top config `preserve_semantics`.
//!
//! # Example
//!
//! ```ignore
//! rules.extend(rewrite!("strength-reduction";  "(i32.shl ?x 1_i32)" <=> "(i32.mul ?x 2_i32)"));
//! ```
//!

pub mod dfg;
pub mod eggsy;
pub mod rules;

use self::{
    dfg::DFGBuilder,
    eggsy::{
        analysis::PeepholeMutationAnalysis,
        encoder::{expr2wasm::ResourceRequest, Encoder},
        expr_enumerator::lazy_expand_aux,
        lang::Lang,
    },
};
use super::{Mutator, OperatorAndByteOffset};
use crate::{
    module::{map_type, PrimitiveTypeInfo},
    Error, ModuleInfo, Result, WasmMutate,
};
use egg::{Rewrite, Runner};
use rand::{prelude::SmallRng, Rng};
use std::{borrow::Cow, fmt::Debug};
use wasm_encoder::{CodeSection, Function, GlobalSection, Instruction, Module, ValType};
use wasmparser::{CodeSectionReader, FunctionBody, GlobalSectionReader, LocalsReader};

/// This mutator applies a random peephole transformation to the input Wasm module
#[derive(Clone, Copy)]
pub struct PeepholeMutator {
    max_tree_depth: u32,
}
type EG = egg::EGraph<Lang, PeepholeMutationAnalysis>;

impl PeepholeMutator {
    /// Initializes a new PeepholeMutator with fuel
    pub fn new(max_depth: u32) -> Self {
        PeepholeMutator {
            max_tree_depth: max_depth,
        }
    }

    // Collect and unfold params and locals, [x, ty, y, ty2] -> [ty....ty, ty2...ty2]
    fn get_func_locals(
        &self,
        info: &ModuleInfo,
        funcidx: u32,
        localsreader: &mut LocalsReader,
    ) -> Result<Vec<PrimitiveTypeInfo>> {
        let ftype = info.get_functype_idx(funcidx);
        match ftype {
            crate::module::TypeInfo::Func(tpe) => {
                let mut all_locals = Vec::new();

                for primitive in &tpe.params {
                    all_locals.push(primitive.clone())
                }
                for _ in 0..localsreader.get_count() {
                    let (count, ty) = localsreader.read()?;
                    let tymapped = PrimitiveTypeInfo::from(ty);
                    for _ in 0..count {
                        all_locals.push(tymapped.clone());
                    }
                }

                Ok(all_locals)
            }
        }
    }

    fn copy_locals(&self, reader: FunctionBody) -> Result<Function> {
        // Create the new function
        let mut localreader = reader.get_locals_reader()?;
        // Get current locals and map to encoder types
        let mut local_count = 0;
        let current_locals = (0..localreader.get_count())
            .map(|_| {
                let (count, ty) = localreader.read().unwrap();
                local_count += count;
                (count, map_type(ty).unwrap())
            })
            .collect::<Vec<(u32, ValType)>>();

        Ok(Function::new(current_locals /*copy locals here*/))
    }

    fn random_mutate<'a>(
        self,
        config: &'a mut WasmMutate,
        rules: Vec<Rewrite<Lang, PeepholeMutationAnalysis>>,
    ) -> Result<Box<dyn Iterator<Item = Result<Module>> + 'a>> {
        let code_section = config.info().get_code_section();
        let mut sectionreader = CodeSectionReader::new(code_section.data, 0)?;
        let function_count = sectionreader.get_count();
        let mut function_to_mutate = config.rng().gen_range(0, function_count);

        let mut visited_functions = 0;

        let readers = (0..function_count)
            .map(|_| sectionreader.read().unwrap())
            .collect::<Vec<_>>();

        loop {
            if visited_functions == function_count {
                return Err(Error::no_mutations_applicable());
            }

            let reader = readers[function_to_mutate as usize];
            let operatorreader = reader.get_operators_reader()?;
            let mut localsreader = reader.get_locals_reader()?;
            let operators = operatorreader
                .into_iter_with_offsets()
                .collect::<wasmparser::Result<Vec<OperatorAndByteOffset>>>()?;
            let operatorscount = operators.len();

            let mut opcode_to_mutate = config.rng().gen_range(0, operatorscount);
            log::trace!(
                "Selecting operator {}/{} from function {}",
                opcode_to_mutate,
                operatorscount,
                function_to_mutate,
            );
            let locals = self.get_func_locals(
                config.info(),
                function_to_mutate + config.info().num_imported_functions(), /* the function type is shifted
                                                                            by the imported functions*/
                &mut localsreader,
            )?;
            let mut count = 0;
            loop {
                config.consume_fuel(1)?;
                if count == operatorscount {
                    break;
                }
                let mut dfg = DFGBuilder::new();
                let basicblock = dfg.get_bb_from_operator(opcode_to_mutate, &operators);

                let basicblock = match basicblock {
                    None => {
                        log::trace!(
                            "Basic block cannot be constructed for opcode {:?}",
                            &operators[opcode_to_mutate]
                        );
                        opcode_to_mutate = (opcode_to_mutate + 1) % operatorscount;
                        count += 1;
                        continue;
                    }
                    Some(basicblock) => basicblock,
                };
                let minidfg = dfg.get_dfg(config.info(), &operators, &basicblock);

                let minidfg = match minidfg {
                    None => {
                        log::trace!("DFG cannot be constructed for opcode {}", opcode_to_mutate);

                        opcode_to_mutate = (opcode_to_mutate + 1) % operatorscount;
                        count += 1;
                        continue;
                    }
                    Some(minidfg) => minidfg,
                };

                if !minidfg.map.contains_key(&opcode_to_mutate) {
                    opcode_to_mutate = (opcode_to_mutate + 1) % operatorscount;
                    count += 1;
                    continue;
                }

                // Create an eterm expression from the basic block starting at oidx
                let start = minidfg.get_expr(opcode_to_mutate);

                if !minidfg.is_subtree_consistent_from_root() {
                    log::trace!("{} is not consistent", start);
                    opcode_to_mutate = (opcode_to_mutate + 1) % operatorscount;
                    count += 1;
                    continue;
                };

                log::trace!(
                    "Trying to mutate\n\
                     {}\n\
                     at opcode {} in function {}",
                    start.pretty(30).trim(),
                    opcode_to_mutate,
                    function_to_mutate,
                );

                let analysis = PeepholeMutationAnalysis::new(
                    config.info().global_types.clone(),
                    locals.clone(),
                    config.info().types_map.clone(),
                    config.info().function_map.clone(),
                );
                let runner = Runner::<Lang, PeepholeMutationAnalysis, ()>::new(analysis)
                    .with_iter_limit(1) // FIXME, the iterations should consume fuel from the actual mutator. Be careful with inner set time limits that can lead us to non-deterministic behavior
                    .with_expr(&start)
                    .run(&rules);
                let mut egraph = runner.egraph;
                // In theory this will return the Id of the operator eterm
                let root = egraph.add_expr(&start);
                let startcmp = start.clone();
                // Since this construction is expensive then more fuel is consumed
                let config4fuel = config.clone();

                // If the number of nodes in the egraph is not large, then
                // continue the search
                if egraph.total_number_of_nodes() <= 1 {
                    opcode_to_mutate = (opcode_to_mutate + 1) % operatorscount;
                    count += 1;
                    continue;
                };

                log::trace!(
                    "Egraph built, nodes count = {}",
                    egraph.total_number_of_nodes()
                );

                // At this point we spent some resource calculating basic block,
                // and constructing the egraph
                config.consume_fuel(1)?;
                let iterator = if config.reduce {
                    let mut extractor = egg::Extractor::new(&egraph, egg::AstSize);
                    let (_best_cost, best_expr) = extractor.find_best(root);
                    Box::new(std::iter::once(best_expr))
                } else {
                    lazy_expand_aux(
                        root,
                        egraph.clone(),
                        self.max_tree_depth,
                        config.rng().gen(),
                    )
                };

                // Filter expression equal to the original one
                let iterator = iterator
                    .filter(move |expr| !expr.to_string().eq(&startcmp.to_string()))
                    .map(move |expr| {
                        log::trace!("Yielding expression:\n{}", expr.pretty(60));

                        let mut newfunc = self.copy_locals(reader)?;
                        let needed_resources = Encoder::build_function(
                            config,
                            opcode_to_mutate,
                            &expr,
                            &operators,
                            &basicblock,
                            &mut newfunc,
                            &minidfg,
                            &egraph,
                        )?;

                        let mut codes = CodeSection::new();
                        let code_section = config.info().get_code_section();
                        let mut sectionreader = CodeSectionReader::new(code_section.data, 0)?;

                        // this mutator is applicable to internal functions, so
                        // it starts by randomly selecting an index between
                        // the imported functions and the total count, total=imported + internal
                        for fidx in 0..config.info().num_local_functions() {
                            let reader = sectionreader.read()?;
                            if fidx == function_to_mutate {
                                codes.function(&newfunc);
                            } else {
                                codes.raw(
                                    &code_section.data[reader.range().start..reader.range().end],
                                );
                            }
                        }

                        // Process the outside function needed resources
                        // Needed globals
                        let mut new_global_section = GlobalSection::new();
                        // Reparse and reencode global section
                        if let Some(_) = config.info().globals {
                            // If the global section was already there, try to copy it to the
                            // new raw section
                            let global_section = config.info().get_global_section();
                            let mut globalreader =
                                GlobalSectionReader::new(global_section.data, 0)?;
                            let count = globalreader.get_count();
                            let mut start = globalreader.original_position();

                            for _ in 0..count {
                                let _ = globalreader.read()?;
                                let current_pos = globalreader.original_position();
                                let global = &global_section.data[start..current_pos];
                                new_global_section.raw(global);
                                start = current_pos;
                            }
                        }

                        if needed_resources.len() > 0 {
                            log::trace!("Adding {} additional resources", needed_resources.len());
                        }

                        for resource in &needed_resources {
                            match resource {
                                ResourceRequest::Global {
                                    index: _,
                                    tpe,
                                    mutable,
                                } => {
                                    // Add to globals
                                    new_global_section.global(
                                        wasm_encoder::GlobalType {
                                            mutable: *mutable,
                                            val_type: match tpe {
                                                PrimitiveTypeInfo::I32 => ValType::I32,
                                                PrimitiveTypeInfo::I64 => ValType::I64,
                                                PrimitiveTypeInfo::F32 => ValType::F32,
                                                PrimitiveTypeInfo::F64 => ValType::F64,
                                                PrimitiveTypeInfo::V128 => ValType::V128,
                                                _ => {
                                                    unreachable!("Not valid for globals")
                                                }
                                            },
                                        },
                                        match tpe {
                                            PrimitiveTypeInfo::I32 => &Instruction::I32Const(0),
                                            PrimitiveTypeInfo::I64 => &Instruction::I64Const(0),
                                            PrimitiveTypeInfo::F32 => &Instruction::F32Const(0.0),
                                            PrimitiveTypeInfo::F64 => &Instruction::F64Const(0.0),
                                            PrimitiveTypeInfo::V128 => &Instruction::V128Const(0),
                                            _ => {
                                                unreachable!("Not valid for globals")
                                            }
                                        },
                                    );
                                }
                            }
                        }

                        let code_index = config.info().code;
                        let global_index = config.info().globals;

                        // This conditional placing enforces to write the global
                        // section by respecting its relative order in the Wasm module
                        let insert_globals_before = config
                            .info()
                            .globals
                            .or(config.info().exports)
                            .or(config.info().start)
                            .or(config.info().elements)
                            .or(config.info().data_count)
                            .or(code_index);

                        // If the mutator is in this staeg, then it passes the can_mutate flter,
                        // which checks for code section existance
                        let insert_globals_before = insert_globals_before.unwrap();
                        let module = config.info().replace_multiple_sections(
                            move |index, _sectionid, module: &mut wasm_encoder::Module| {
                                if insert_globals_before == index
                            // Write if needed or if it wasm in the init Wasm
                            && (new_global_section.len() > 0 || global_index.is_some() )
                                {
                                    // Insert the new globals here
                                    module.section(&new_global_section);
                                }
                                if index == code_index.unwrap() {
                                    // Replace code section
                                    module.section(&codes);

                                    return true;
                                }
                                if let Some(gidx) = global_index {
                                    // return true since the global section is written by the
                                    // conditional position writer
                                    return gidx == index;
                                }
                                // False to say the underlying encoder to write the prexisting
                                // section
                                false
                            },
                        );
                        Ok(module)
                    })
                    // Consume fuel for each returned expression and it is expensive
                    .take_while(move |_| config4fuel.consume_fuel(1).is_ok());

                return Ok(Box::new(iterator));
            }
            function_to_mutate = (function_to_mutate + 1) % function_count;
            visited_functions += 1;
        }
    }

    /// To separate the methods will allow us to test rule by rule
    fn mutate_with_rules<'a>(
        self,
        config: &'a mut WasmMutate,
        rules: Vec<Rewrite<Lang, PeepholeMutationAnalysis>>,
    ) -> Result<Box<dyn Iterator<Item = Result<Module>> + 'a>> {
        self.random_mutate(config, rules)
    }
}

/// Meta mutator for peephole
impl Mutator for PeepholeMutator {
    fn mutate<'a>(
        self,
        config: &'a mut crate::WasmMutate,
    ) -> Result<Box<dyn Iterator<Item = Result<Module>> + 'a>> {
        // Calculate here type related information for parameters, locals and returns
        // This information could be passed to the conditions to check for type correctness rewriting
        // Write the new rules in the rules.rs file
        let rules = self.get_rules(config);

        let modules = self.mutate_with_rules(config, rules)?;

        Ok(modules)
    }

    fn can_mutate<'a>(&self, config: &'a WasmMutate) -> bool {
        config.info().has_code() && config.info().num_local_functions() > 0
    }
}

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
        operators: &[OperatorAndByteOffset<'a>],
        at: usize,
    ) -> Result<bool>;

    /// Provides the name of the mutator, mostly used for debugging purposes
    fn name(&self) -> Cow<'static, str> {
        std::any::type_name::<Self>().into()
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
                    let range = operatorsreader.get_binary_reader().range();
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
        info::ModuleInfo,
        module::PrimitiveTypeInfo,
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
                            Lang::I32(_) => true,
                            Lang::I64(_) => true,
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

    fn is_type(vari: &'static str, t: PrimitiveTypeInfo) -> impl Fn(&mut EG, Id, &Subst) -> bool {
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

    #[test]
    fn test_peep_unfold2() {
        let rules: &[Rewrite<super::Lang, PeepholeMutationAnalysis>] = &[
            rewrite!("unfold-2";  "?x" => "(i32.unfold ?x)" if is_const("?x") if is_type("?x", PrimitiveTypeInfo::I32)),
        ];

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
                  i32.const 160268115
                  i32.const -160268059
                  i32.add)
                (export "exported_func" (func 0)))
            "#,
            0,
        );
    }

    #[test]
    fn test_peep_stack_neutral2() {
        let rules: &[Rewrite<super::Lang, PeepholeMutationAnalysis>] = &[
            rewrite!("strength-undo";  "?x" => "(i32.or ?x ?x)" if is_type("?x", PrimitiveTypeInfo::I32)),
        ];

        test_peephole_mutator(
            r#"
        (module
            (func (export "exported_func")  (local i32 i32)
                i32.const 10
                drop
            )
        )
        "#,
            rules,
            r#"
            (module
                (type (;0;) (func ))
                (func (;0;) (type 0)
                    (local i32 i32)
                    i32.const 10
                    i32.const 10
                    i32.or
                    drop
                )
                (export "exported_func" (func 0)))
            "#,
            4,
        );
    }

    #[test]
    fn test_peep_select() {
        let rules: &[Rewrite<super::Lang, PeepholeMutationAnalysis>] =
            &[rewrite!("rule";  "(select ?x ?y ?z)" => "(select ?y ?x (i32.eqz ?z))")];

        test_peephole_mutator(
            r#"
        (module
            (func (export "exported_func") (result f32)  (local i32 i32)
                f32.const 10
                f32.const 20
                f32.add
                f32.const 200
                local.get 0
                select
            )
        )
        "#,
            rules,
            r#"
            (module
                (type (;0;) (func (result f32)))
                (func (;0;) (type 0) (result f32)
                  (local i32 i32)
                  f32.const 0x1.9p+7 (;=200;)
                  f32.const 0x1.4p+3 (;=10;)
                  f32.const 0x1.4p+4 (;=20;)
                  f32.add
                  local.get 0
                  i32.eqz
                  select)
                (export "exported_func" (func 0)))
            "#,
            4,
        );
    }

    #[test]
    fn test_peep_wrap() {
        let rules: &[Rewrite<super::Lang, PeepholeMutationAnalysis>] = &[
            rewrite!("strength-undo";  "?x" => "(i32.add ?x 0_i32)" if is_type("?x", PrimitiveTypeInfo::I32)),
        ];

        test_peephole_mutator(
            r#"
        (module
            (func (export "exported_func") (result i32) (local i64)
                local.get 0
                i64.const 0
                i64.shl
                i32.wrap_i64
                i32.const -441701230
                i32.const 441701230
                i32.add
                i32.add
            )
        )
        "#,
            rules,
            r#"
            (module
                (type (;0;) (func (result i32) ))
                (func (;0;) (type 0) (result i32)
                    (local i64)
                    local.get 0
                    i64.const 0
                    i64.shl
                    i32.wrap_i64
                    i32.const -441701230
                    i32.const 441701230
                    i32.add
                    i32.add
                    i32.const 0
                    i32.add)
              (export "exported_func" (func 0)))
            "#,
            0,
        );
    }

    #[test]
    fn test_peep_irelop1() {
        let rules: &[Rewrite<super::Lang, PeepholeMutationAnalysis>] =
            &[rewrite!("strength-undo";  "(i64.eqz ?x)" => "(i64.eq ?x 0_i64)")];

        test_peephole_mutator(
            r#"
        (module
            (func (export "exported_func") (result i32) (local i32 i32)
                i64.const 10
                i64.eqz
            )
        )
        "#,
            rules,
            r#"
            (module
                (type (;0;) (func (result i32) ))
                (func (;0;) (type 0)
                    (local i32 i32)
                    i64.const 10
                    i64.const 0
                    i64.eq
                )
                (export "exported_func" (func 0)))
            "#,
            2,
        );
    }

    #[test]
    fn test_peep_bug1() {
        let rules: &[Rewrite<super::Lang, PeepholeMutationAnalysis>] = &[
            rewrite!("strength-undo";  "?x" => "(i32.shl ?x 0_i32)" if is_type("?x", PrimitiveTypeInfo::I32)),
        ];

        test_peephole_mutator(
            r#"
            (module
                (type (;0;) (func (result i32)))
                (func (;0;) (type 0) (result i32)
                  i32.const -14671840
                  i64.extend_i32_u
                  i32.const -1
                  i64.extend_i32_u
                  i64.rem_s
                  i64.const -1
                  i64.le_u)
                (data (;0;) ""))
        "#,
            rules,
            r#"
            (module
                (type (;0;) (func (result i32)))
                (func (;0;) (type 0) (result i32)
                  i32.const -14671840
                  i64.extend_i32_u
                  i32.const -1
                  i64.extend_i32_u
                  i64.rem_s
                  i64.const -1
                  i64.le_u
                  i32.const 0
                  i32.shl)
                (data (;0;) ""))
            "#,
            11494877297919394048,
        );
    }

    #[test]
    fn test_peep_commutative() {
        let rules: &[Rewrite<super::Lang, PeepholeMutationAnalysis>] =
            &[rewrite!("commutative-1";  "(i32.add ?x ?y)" => "(i32.add ?y ?x)")];

        test_peephole_mutator(
            r#"
        (module
            (func (export "exported_func") (result i32) (local i32 i32)
                i32.const 42
                i32.const 1
                i32.add
            )
        )
        "#,
            rules,
            r#"
            (module
                (type (;0;) (func (result i32)))
                (func (;0;) (type 0) (result i32)
                  (local i32 i32)
                  i32.const 1
                  i32.const 42
                  i32.add
                )
                (export "exported_func" (func 0)))
            "#,
            6,
        );
    }

    #[test]
    fn test_peep_inversion() {
        let rules: &[Rewrite<super::Lang, PeepholeMutationAnalysis>] =
            &[rewrite!("inversion-1";  "(i32.gt_s ?x ?y)" => "(i32.le_s ?y ?x)")];

        test_peephole_mutator(
            r#"
        (module
            (func (export "exported_func") (result i32) (local i32 i32)
                i32.const 42
                i32.const 1
                i32.gt_s
            )
        )
        "#,
            rules,
            r#"
            (module
                (type (;0;) (func (result i32)))
                (func (;0;) (type 0) (result i32)
                  (local i32 i32)
                  i32.const 1
                  i32.const 42
                  i32.le_s)
                (export "exported_func" (func 0)))
            "#,
            0,
        );
    }

    #[test]
    fn test_peep_integrtion() {
        let rules: &[Rewrite<super::Lang, PeepholeMutationAnalysis>] =
            &[rewrite!("inversion-1";  "(i32.gt_s ?x ?y)" => "(i32.le_s ?y ?x)")];

        test_peephole_mutator(
            r#"
        (module
            (func (export "exported_func") (result i32) (local i32 i32)
                i32.const 42
                i32.const 1
                i32.gt_s
            )
        )
        "#,
            rules,
            r#"
            (module
                (type (;0;) (func (result i32)))
                (func (;0;) (type 0) (result i32)
                  (local i32 i32)
                  i32.const 1
                  i32.const 42
                  i32.le_s)
                (export "exported_func" (func 0)))
            "#,
            0,
        );
    }

    #[test]
    fn test_peep_inversion2() {
        let original = r#"
            (module
                (type (;0;) (func (param i64 i32 f32)))
                (func (;0;) (type 0) (param i64 i32 f32)
                i32.const 100
                i32.const 200
                i32.store offset=600 align=1
                )
                (memory (;0;) 0)
                (export "\00" (memory 0)))
        "#;
        let original = &wat::parse_str(original).unwrap();
        let info = ModuleInfo::new(original).unwrap();

        let mut wasmmutate = WasmMutate::default();
        wasmmutate.fuel(3);
        wasmmutate.info = Some(info);
        let rnd = SmallRng::seed_from_u64(0);
        wasmmutate.rng = Some(rnd);

        let mutator = PeepholeMutator::new(2);

        let can_mutate = mutator.can_mutate(&wasmmutate);

        assert_eq!(can_mutate, true);
        let rules = mutator.get_rules(&wasmmutate);

        for mutated in mutator.mutate_with_rules(&mut wasmmutate, rules).unwrap() {
            let module = mutated.unwrap();

            let mut validator = wasmparser::Validator::new();
            let mutated_bytes = &module.finish();
            let _text = wasmprinter::print_bytes(mutated_bytes).unwrap();
            crate::validate(&mut validator, mutated_bytes);
        }
    }

    #[test]
    fn test_mem_store1() {
        let rules: &[Rewrite<super::Lang, PeepholeMutationAnalysis>] = &[
            rewrite!("rule";  "(i32.store.600.0.0 ?value ?offset)" => "(i32.store.0.0.0 ?value (i32.add ?offset 600_i32))" ),
        ];

        test_peephole_mutator(
            r#"
            (module
                (type (;0;) (func (param i64 i32 f32)))
                (func (;0;) (type 0) (param i64 i32 f32)
                  i32.const 100
                  i32.const 200
                  i32.store offset=600 align=1
                )
                (memory (;0;) 0)
                (export "\00" (memory 0)))
        "#,
            rules,
            r#"
            (module
                (type (;0;) (func (param i64 i32 f32)))
                (func (;0;) (type 0) (param i64 i32 f32)
                  i32.const 100
                  i32.const 600
                  i32.add
                  i32.const 200
                  i32.store align=1)
                (memory (;0;) 0)
                (export "\00" (memory 0)))
            "#,
            0,
        );
    }

    #[test]
    fn test_peep_shl0() {
        let rules: &[Rewrite<super::Lang, PeepholeMutationAnalysis>] = &[
            rewrite!("strength-undo3";  "(i64.shr_u ?x ?y)" => "(i64.shl (i64.shr_u ?x ?y) 0_i64)" ),
        ];

        test_peephole_mutator(
            r#"
            (module
                (type (;0;) (func (param i64 i32 f32)))
                (func (;0;) (type 0) (param i64 i32 f32)
                  i64.const 89
                  local.get 1
                  i64.load align=2
                  local.get 1
                  i64.load align=1
                  i64.shr_u
                  drop
                  drop
                )
                (memory (;0;) 0)
                (export "\00" (memory 0)))
        "#,
            rules,
            r#"
            (module
                (type (;0;) (func (param i64 i32 f32)))
                (func (;0;) (type 0) (param i64 i32 f32)
                  i64.const 89
                  local.get 1
                  i64.load align=2
                  local.get 1
                  i64.load align=1
                  i64.shr_u
                  i64.const 0
                  i64.shl
                  drop
                  drop)
                (memory (;0;) 0)
                (export "\00" (memory 0)))
            "#,
            5,
        );
    }

    #[test]
    fn test_peep_idem1() {
        let rules: &[Rewrite<super::Lang, PeepholeMutationAnalysis>] = &[
            rewrite!("idempotent-1";  "?x" => "(i32.or ?x ?x)" if is_type("?x", PrimitiveTypeInfo::I32)),
            rewrite!("idempotent-12";  "?x" => "(i64.or ?x ?x)" if is_type("?x", PrimitiveTypeInfo::I64)),
        ];

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
            0,
        );
    }

    #[test]
    fn test_peep_cv() {
        let rules: &[Rewrite<super::Lang, PeepholeMutationAnalysis>] = &[
            rewrite!("idempotent-1";  "?x" => "(i32.or ?x ?x)" if is_type("?x", PrimitiveTypeInfo::I32)),
        ];

        test_peephole_mutator(
            r#"
        (module
            (func (export "exported_func") (result i32) (local i32 i32)
                i64.const 56
                i64.const 2
                i64.mul
                i32.wrap_i64
            )
        )
        "#,
            rules,
            r#"
        (module
            (type (;0;) (func (result i32)))
            (func (;0;) (type 0) (result i32)
                (local i32 i32)
                i64.const 56
                i64.const 2
                i64.mul
                i32.wrap_i64
                i64.const 56
                i64.const 2
                i64.mul
                i32.wrap_i64
                i32.or)
            (export "exported_func" (func 0)))
        "#,
            4,
        );
    }

    #[test]
    fn test_peep_cv4() {
        let rules: &[Rewrite<super::Lang, PeepholeMutationAnalysis>] = &[
            rewrite!("idempotent-1";  "?x" => "(i32.or ?x ?x)" if is_type("?x", PrimitiveTypeInfo::I32)),
        ];

        test_peephole_mutator(
            r#"
        (module
            (func (export "exported_func") (result i32) (local i32 i32)
                i32.const 56
                i32.extend8_s
            )
        )
        "#,
            rules,
            r#"
        (module
            (func (;0;) (result i32)
                (local i32 i32)
                i32.const 56
                i32.extend8_s
                i32.const 56
                i32.extend8_s
                i32.or)
            (export "exported_func" (func 0)))
        "#,
            8,
        );
    }

    #[test]
    fn test_peep_cv5() {
        let rules: &[Rewrite<super::Lang, PeepholeMutationAnalysis>] =
            &[rewrite!("cv4";  "?x" => "(i32.and ?x ?x)" if is_type("?x", PrimitiveTypeInfo::I32))];

        test_peephole_mutator(
            r#"
                (module
                    (type (;0;) (func (result i32)))
                    (func (;0;) (type 0) (result i32)
                    i32.const -1
                    i64.extend_i32_u
                    i64.const -1
                    i64.ge_s)
                    (data (;0;) ""))
            "#,
            rules,
            r#"
                        (module
                            (type (;0;) (func (result i32)))
                            (func (;0;) (type 0) (result i32)
                            i32.const -1
                            i64.extend_i32_u
                            i64.const -1
                            i64.ge_s
                            i32.const -1
                            i64.extend_i32_u
                            i64.const -1
                            i64.ge_s
                            i32.and)
                            (data (;0;) ""))
                    "#,
            10,
        );
    }

    #[test]
    fn test_use_global() {
        let rules: &[Rewrite<super::Lang, PeepholeMutationAnalysis>] =
            &[rewrite!("rule";  "?x" => "(i32.use_of_global ?x)")];

        test_peephole_mutator(
            r#"
        (module
            (func (export "exported_func") (result i32) (local i32 i32)
                i32.const 10
            )
        )
        "#,
            rules,
            r#"
            (module
                (type (;0;) (func (result i32)))
                (func (;0;) (type 0) (result i32)
                  (local i32 i32)
                  i32.const 10
                  global.set 0
                  global.get 0)
                (global (;0;) (mut i32) i32.const 0)
                (export "exported_func" (func 0)))
            "#,
            4,
        );
    }

    #[test]
    fn test_peep_idem3() {
        let rules: &[Rewrite<super::Lang, PeepholeMutationAnalysis>] = &[
            rewrite!("idempotent-3";  "?x" => "(i32.add ?x 0_i32)" if is_type("?x", PrimitiveTypeInfo::I32)),
        ];

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
                i32.const 0
                i32.add)
            (export "exported_func" (func 0)))
        "#,
            0,
        );
    }

    #[test]
    fn test_peep_idem4() {
        let rules: &[Rewrite<super::Lang, PeepholeMutationAnalysis>] = &[
            rewrite!("idempotent-4";  "?x" => "(i32.mul ?x 1_i32)" if is_type("?x", PrimitiveTypeInfo::I32)),
            rewrite!("idempotent-4";  "?x" => "(i64.mul ?x 1_i32)" if is_type("?x", PrimitiveTypeInfo::I64)),
        ];

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
                i32.const 1
                i32.mul)
            (export "exported_func" (func 0)))
        "#,
            0,
        );
    }

    #[test]
    fn test_peep_typeinfo() {
        let rules: &[Rewrite<super::Lang, PeepholeMutationAnalysis>] = &[
            rewrite!("type1-1";  "?x" => "(i32.shr_u ?x ?x)" if is_type("?x", PrimitiveTypeInfo::I32) ),
        ];

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
                i32.shr_u)
            (export "exported_func" (func 0)))
        "#,
            0,
        );
    }

    #[test]
    fn test_peep_locals1() {
        let rules: &[Rewrite<super::Lang, PeepholeMutationAnalysis>] =
            &[rewrite!("type1-1";  "(i32.add ?x ?y)" => "(i32.add ?y ?x)" )];

        test_peephole_mutator(
            r#"
        (module
            (func (export "exported_func") (result i32) (local i32 i32)
                local.get 0
                local.get 1
                i32.add
            )
        )
        "#,
            rules,
            r#"
        (module
            (type (;0;) (func (result i32)))
            (func (;0;) (type 0) (result i32)
                (local i32 i32)
                local.get 1
                local.get 0
                i32.add)
            (export "exported_func" (func 0)))
        "#,
            5,
        );
    }

    #[test]
    fn test_peep_locals3() {
        let rules: &[Rewrite<super::Lang, PeepholeMutationAnalysis>] =
            &[rewrite!("type1-1";  "(local.set.1 100_i32)" => "(local.set.1 0_i32)" )];

        test_peephole_mutator(
            r#"
        (module
            (func (export "exported_func") (local i32 i32)
                i32.const 100
                local.set 1

            )
        )
        "#,
            rules,
            r#"
        (module
            (type (;0;) (func ))
            (func (;0;) (type 0)
                (local i32 i32)
                i32.const 0
                local.set 1
            )
            (export "exported_func" (func 0)))
        "#,
            1,
        );
    }

    #[test]
    fn test_peep_functions() {
        let rules: &[Rewrite<super::Lang, PeepholeMutationAnalysis>] =
            &[rewrite!("type1-1";  "(call.0 ?x ?y)" => "(call.0 1_i64 11_i32) " )];

        test_peephole_mutator(
            r#"
        (module
            (func (export "exported_func")(param i64 i32 )  (result i64)  (local i64 i32)
                local.get 0
                i32.const 10
                call 0
            )
        )
        "#,
            rules,
            r#"
            (module
                (func (export "exported_func") (param i64 i32)  (result i64)  (local i64 i32)
                    i64.const 1
                    i32.const 11
                    call 0
                )
            )
        "#,
            0,
        );
    }

    #[test]
    fn test_peep_functions2() {
        let rules: &[Rewrite<super::Lang, PeepholeMutationAnalysis>] = &[
            rewrite!("type1-1";  "?x" => "(i32.or ?x ?x)" if is_type("?x", PrimitiveTypeInfo::I32)),
        ];

        test_peephole_mutator(
            r#"
            (module
                (type (;0;) (func (param i64 i64 i64 i64 i64 i64 i64 i64 i64 i64) (result i32)))
                (type (;1;) (func (param i64) (result i32)))
                (import "ttttttttttttuttttttttttut\09" "" (func (;0;) (type 1)))
                (func (;1;) (type 0) (param i64 i64 i64 i64 i64 i64 i64 i64 i64 i64) (result i32)
                  (local i32)
                  local.get 6
                  local.get 6
                  i64.div_s
                  local.get 6
                  i64.div_s
                  local.get 6
                  i64.div_s
                  local.get 6
                  i64.div_s
                  local.get 6
                  i64.div_s
                  local.get 6
                  i64.div_s
                  call 0)
            )
            "#,
            rules,
            r#"
            (module
                (type (;0;) (func (param i64 i64 i64 i64 i64 i64 i64 i64 i64 i64) (result i32)))
                (type (;1;) (func (param i64) (result i32)))
                (import "ttttttttttttuttttttttttut\09" "" (func (;0;) (type 1)))
                (func (;1;) (type 0) (param i64 i64 i64 i64 i64 i64 i64 i64 i64 i64) (result i32)
                  (local i32)
                  local.get 6
                  local.get 6
                  i64.div_s
                  local.get 6
                  i64.div_s
                  local.get 6
                  i64.div_s
                  local.get 6
                  i64.div_s
                  local.get 6
                  i64.div_s
                  local.get 6
                  i64.div_s
                  call 0
                  local.get 6
                  local.get 6
                  i64.div_s
                  local.get 6
                  i64.div_s
                  local.get 6
                  i64.div_s
                  local.get 6
                  i64.div_s
                  local.get 6
                  i64.div_s
                  local.get 6
                  i64.div_s
                  call 0
                  i32.or
                )
            )
        "#,
            9,
        );
    }

    #[test]
    fn test_peep_locals2() {
        let rules: &[Rewrite<super::Lang, PeepholeMutationAnalysis>] =
            &[rewrite!("type1-1";  "(i64.add ?x ?y)" => "(i64.add ?y ?x)" )];

        test_peephole_mutator(
            r#"
        (module
            (func (export "exported_func") (result i64) (local i64 i64)
                local.get 0
                local.get 1
                i64.add
            )
        )
        "#,
            rules,
            r#"
        (module
            (type (;0;) (func (result i64)))
            (func (;0;) (type 0) (result i64)
                (local i64 i64)
                local.get 1
                local.get 0
                i64.add)
            (export "exported_func" (func 0)))
        "#,
            0,
        );
    }

    #[test]
    fn test_peep_floats1() {
        let rules: &[Rewrite<super::Lang, PeepholeMutationAnalysis>] =
            &[rewrite!("rule";  "1065353216_f32" => "0_f32" )];

        test_peephole_mutator(
            r#"
        (module
            (func (export "exported_func") (result f32) (local i64 i64)
                f32.const 1
            )
        )
        "#,
            rules,
            r#"
            (module
                (type (;0;) (func (result f32)))
                (func (;0;) (type 0) (result f32)
                  (local i64 i64)
                  f32.const 0x0p+0 (;=0;))
                (export "exported_func" (func 0)))
        "#,
            0,
        );
    }

    #[test]
    fn test_peep_globals1() {
        let rules: &[Rewrite<super::Lang, PeepholeMutationAnalysis>] = &[
            rewrite!("mem-load-shift";  "?x" => "(i32.add ?x 0_i32)" if is_type("?x", PrimitiveTypeInfo::I32)),
        ];

        test_peephole_mutator(
            r#"
        (module
            (memory 1)
            (global $0 i32 i32.const 0)
            (func (export "exported_func") (param i32) (result i32)
                global.get 0
            )
        )
        "#,
            rules,
            r#"
            (module
                (type (;0;) (func (param i32) (result i32)))
                (global $0 i32 i32.const 0)
                (func (;0;) (type 0) (param i32) (result i32)
                  global.get $0
                  i32.const 0
                  i32.add)
                (memory (;0;) 1)
                (export "exported_func" (func 0)))
        "#,
            0,
        );
    }

    #[test]
    fn test_peep_globals2() {
        let rules: &[Rewrite<super::Lang, PeepholeMutationAnalysis>] = &[
            rewrite!("rule";  "?x" => "(i32.add ?x 0_i32)" if is_type("?x", PrimitiveTypeInfo::I32)),
        ];

        test_peephole_mutator(
            r#"
        (module
            (memory 1)
            (global $0 (mut i32) i32.const 0)
            (func (export "exported_func") (param i32) (result i32)
                i32.const 10
                global.set 0
                i32.const 20
            )
        )
        "#,
            rules,
            r#"
            (module
                (type (;0;) (func (param i32) (result i32)))
                (func (;0;) (type 0) (param i32) (result i32)
                  i32.const 10
                  global.set $0
                  i32.const 20
                  i32.const 0
                  i32.add)
                (memory (;0;) 1)
                (global $0 (mut i32) i32.const 0)
                (export "exported_func" (func 0)))
        "#,
            4,
        );
    }

    fn test_peephole_mutator(
        original: &str,
        rules: &[Rewrite<super::Lang, PeepholeMutationAnalysis>],
        expected: &str,
        seed: u64,
    ) {
        let original = &wat::parse_str(original).unwrap();
        let info = ModuleInfo::new(original).unwrap();

        let mut wasmmutate = WasmMutate::default();
        wasmmutate.fuel(300);
        wasmmutate.info = Some(info);
        let rnd = SmallRng::seed_from_u64(seed);
        wasmmutate.rng = Some(rnd);

        let mutator = PeepholeMutator::new(3);

        let can_mutate = mutator.can_mutate(&wasmmutate);

        assert_eq!(can_mutate, true);
        let mut found = false;
        for mutated in mutator
            .mutate_with_rules(&mut wasmmutate, rules.to_vec())
            .unwrap()
        {
            let mut validator = wasmparser::Validator::new();
            let mutated_bytes = &mutated.unwrap().finish();
            let text = wasmprinter::print_bytes(mutated_bytes).unwrap();
            crate::validate(&mut validator, mutated_bytes);

            let expected_bytes = &wat::parse_str(expected).unwrap();
            let expectedtext = wasmprinter::print_bytes(expected_bytes).unwrap();

            if expectedtext == text {
                found = true;
            }
        }

        // Assert that the passed mutation was found
        assert!(found)
    }
}
