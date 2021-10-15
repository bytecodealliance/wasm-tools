//! This mutator applies a random peephole transformation to the input Wasm module
use crate::error::EitherType;
use crate::module::PrimitiveTypeInfo;
use crate::mutators::peephole::eggsy::analysis::PeepholeMutationAnalysis;
use crate::mutators::peephole::eggsy::encoder::Encoder;
use crate::mutators::peephole::eggsy::lang::Lang;
use egg::{rewrite, AstSize, Id, RecExpr, Rewrite, Runner, Subst};
use rand::{prelude::SmallRng, Rng};
use std::convert::TryFrom;
use std::sync::atomic::AtomicU64;
use wasm_encoder::{CodeSection, Function, Module, ValType};
use wasmparser::{CodeSectionReader, FunctionBody, LocalsReader, Operator};

// Hack to show debug messages in tests
#[cfg(not(test))]
use log::debug;
#[cfg(test)]
use std::println as debug;

use crate::{module::map_type, ModuleInfo, Result, WasmMutate};

// This is a performance counter for the number of operators that can be mutated
static NUM_RUNS: AtomicU64 = AtomicU64::new(0);
static NUM_SUCCESSFUL_MUTATIONS: AtomicU64 = AtomicU64::new(0);

use self::{dfg::DFGIcator, eggsy::RandomExtractor};

use super::Mutator;

pub mod dfg;
pub mod eggsy;

/// This mutator applies a random peephole transformation to the input Wasm module
pub struct PeepholeMutator;
type EG = egg::EGraph<Lang, PeepholeMutationAnalysis>;

// Code mutator, function id, operator id
type MutationContext = (Function, u32);

// Helper type to return operator and ofsset inside the byte stream
type OperatorAndByteOffset<'a> = (Operator<'a>, usize);
impl PeepholeMutator {
    // Collect and unfold params and locals, [x, ty, y, ty2] -> [ty....ty, ty2...ty2]
    fn get_func_locals(
        &self,
        info: &ModuleInfo,
        funcidx: u32,
        localsreader: &mut LocalsReader,
    ) -> Result<Vec<PrimitiveTypeInfo>> {
        let ftype = info.get_functype_idx(funcidx as usize);
        match ftype {
            crate::module::TypeInfo::Func(tpe) => {
                let mut all_locals = Vec::new();

                for primitive in &tpe.params {
                    all_locals.push(primitive.clone())
                }
                for _ in 0..localsreader.get_count() {
                    let (count, ty) = localsreader.read()?;
                    let tymapped = PrimitiveTypeInfo::try_from(ty)?;
                    for _ in 0..count {
                        all_locals.push(tymapped.clone());
                    }
                }

                Ok(all_locals)
            }
            _ => Err(crate::Error::UnsupportedType(EitherType::TypeDef(format!(
                "The type for this function is not a function tyupe definition"
            )))),
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

    fn random_mutate(
        &self,
        _: &crate::WasmMutate,
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
            let mut localsreader = reader.get_locals_reader()?;
            let operators = operatorreader
                .into_iter_with_offsets()
                .collect::<wasmparser::Result<Vec<OperatorAndByteOffset>>>()?;
            let operatorscount = operators.len();
            let opcode_to_mutate = rnd.gen_range(0, operatorscount);

            let locals = self.get_func_locals(&info, fidx + info.imported_functions_count /* the function type is shifted by the imported functions*/, &mut localsreader)?;

            for oidx in (opcode_to_mutate..operatorscount).chain(0..opcode_to_mutate) {
                let mut dfg = DFGIcator::new();
                let basicblock = dfg.get_bb_from_operator(oidx, &operators);

                let old_num_runs = NUM_RUNS.fetch_add(1, core::sync::atomic::Ordering::Relaxed);
                if old_num_runs % 4096 == 0 && log::log_enabled!(log::Level::Info) {
                    let successful =
                        NUM_SUCCESSFUL_MUTATIONS.load(core::sync::atomic::Ordering::Relaxed);
                    let percent = successful as f64 / old_num_runs as f64 * 100.0;
                    log::info!(
                        "{} / {} ({:.2}%) mutated operators.",
                        successful,
                        old_num_runs,
                        percent
                    );
                }

                match basicblock {
                    Some(basicblock) => {
                        let minidfg = dfg.get_dfg(&info, &operators, &basicblock, &locals);

                        match minidfg {
                            None => {
                                continue;
                            }
                            Some(minidfg) => {
                                if !minidfg.map.contains_key(&oidx) {
                                    continue;
                                }

                                // Create an eterm expression from the basic block starting at oidx
                                let mut start = RecExpr::<Lang>::default();
                                let lang_to_stack_entries =
                                    Encoder::wasm2expr(&minidfg, oidx, &operators, &mut start);

                                if let Err(crate::Error::NoMutationsAplicable) =
                                    lang_to_stack_entries
                                {
                                    // Definition entries that dont return values cannot be rewritten ?
                                    debug!("{} does not return a value", start);
                                    continue;
                                }
                                let lang_to_stack_entries = lang_to_stack_entries?;
                                // Continue if the subtree coloring is inconsistent
                                debug!("{}", minidfg.pretty_print(&operators));

                                if !minidfg.is_subtree_consistent_from_root() {
                                    debug!("{} is not consistent", start);
                                    continue;
                                }

                                debug!("Trying to mutate {} at {}", start, oidx);

                                let analysis = PeepholeMutationAnalysis::new(
                                    lang_to_stack_entries.clone(),
                                    minidfg.clone(),
                                );
                                let runner =
                                    Runner::<Lang, PeepholeMutationAnalysis, ()>::new(analysis)
                                        .with_iter_limit(1) // only one iterations, do not wait for eq saturation, increasing only by one it affects the execution time of the mutator by a lot
                                        .with_expr(&start)
                                        .run(rules);
                                let mut egraph = runner.egraph;

                                // In theory this will return the Id of the operator eterm
                                let root = egraph.add_expr(&start);

                                // This cost function could be replaced by a custom weighted probability, for example
                                // we could modify the cost function based on the previous mutation/rotation outcome
                                let cf = AstSize;
                                let extractor = RandomExtractor::new(&egraph, cf);

                                let (expr, node_to_eclass) = extractor.extract_random(
                                    rnd,
                                    root,
                                    /* only 1 for now */ 0,
                                    Encoder::build_expr,
                                    /* max tries */ 1,
                                    |expr| !expr.to_string().eq(&start.to_string()),
                                )?;

                                if expr.to_string().eq(&start.to_string()) {
                                    continue;
                                }

                                debug!("Applied mutation {} for {}", expr, start);

                                let mut newfunc = self.copy_locals(reader)?;

                                Encoder::build_function(
                                    info,
                                    rnd,
                                    oidx,
                                    &expr,
                                    &node_to_eclass,
                                    &operators,
                                    &basicblock,
                                    &mut newfunc,
                                    &egraph,
                                )?;

                                if log::log_enabled!(log::Level::Info) {
                                    NUM_SUCCESSFUL_MUTATIONS
                                        .fetch_add(1, core::sync::atomic::Ordering::Relaxed);
                                }

                                return Ok((newfunc, fidx));
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

        // this mutator is applicable to internal functions, so
        // it starts by randomly selecting an index between
        // the imported functions and the total count, total=imported + internal
        for fidx in 0..info.function_count {
            let reader = sectionreader.read()?;
            if fidx == function_to_mutate {
                debug!("Mutating function  idx {:?}", fidx);
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
    fn is_const(&self, vari: &'static str) -> impl Fn(&mut EG, Id, &Subst) -> bool {
        move |egraph: &mut EG, _, subst| {
            let var = vari.parse();
            match var {
                Ok(var) => {
                    let eclass = &egraph[subst[var]];
                    if eclass.nodes.len() == 1 {
                        let node = &eclass.nodes[0];
                        match node {
                            Lang::Num(_) => true,
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
        // Calculate here type related information for parameters, locals and returns
        // This information could be passed to the conditions to check for type correctness rewriting

        let mut rules = vec![
            rewrite!("unfold-2";  "?x" => "(unfold ?x)" if self.is_const("?x") ),
            rewrite!("xor-x-x";  "(xor ?x ?x)" => "0" ),
            rewrite!("mem-load-shift";  "(load ?x ?y ?z ?w)" => "(load (add ?x ?y) 0 ?z ?w)"),
        ];
        // Use a custom instruction-mutator for this
        // This specific rewriting rule has a condition, it should be appplied if the operand is a constant
        rules.extend(rewrite!("strength-undo";  "(shl ?x 1)" <=> "(mul ?x 2)"));
        rules.extend(rewrite!("strength-undo1";  "(shl ?x 2)" <=> "(mul ?x 4)"));
        rules.extend(rewrite!("strength-undo2";  "(shl ?x 3)" <=> "(mul ?x 8)"));
        rules.extend(rewrite!("strength-undo3";  "(shl ?x 0)" <=> "?x"));

        rules.extend(rewrite!("add-1";  "(add ?x ?x)" <=> "(mul ?x 2)"));
        rules.extend(rewrite!("idempotent-1";  "?x" <=> "(or ?x ?x)" ));
        rules.extend(rewrite!("idempotent-2";  "?x" <=> "(and ?x ?x)"));
        rules.extend(rewrite!("commutative-1";  "(add ?x ?y)" <=> "(add ?y ?x)"));
        rules.extend(rewrite!("commutative-2";  "(mul ?x ?y)" <=> "(mul ?y ?x)" ));
        rules
            .extend(rewrite!("associative-2";  "(mul ?x (mul ?y ?z))" <=> "(mul (mul ?x ?y) ?z)" ));
        rules
            .extend(rewrite!("associative-1";  "(add ?x (add ?y ?z))" <=> "(add (add ?x ?y) ?z)" ));

        rules.extend(rewrite!("idempotent-3";  "?x" <=> "(mul ?x 1)" ));
        rules.extend(rewrite!("idempotent-4";  "?x" <=> "(add ?x 0)" ));
        rules.extend(rewrite!("idempotent-5";  "?x" <=> "(xor ?x 0)" ));
        rules.extend(rewrite!("idempotent-6"; "(eqz ?x)" <=> "(eq ?x 0)"));
        rules.extend(rewrite!("commutative-3"; "(eq ?x ?y)" <=> "(eq ?y ?x)"));

        if !config.preserve_semantics {
            rules.push(rewrite!("mem-load-shift";  "(load ?x ?y ?z ?w)" => "(load (add ?x rand) ?y ?z ?w)"))
            // Check why this is generating a lot of the same replacements
            // Add corretness attraction ones
            // x  = x + 1
            //
        }

        self.mutate_with_rules(config, rnd, info, &rules)
    }

    fn can_mutate<'a>(&self, _: &'a crate::WasmMutate, info: &crate::ModuleInfo) -> Result<bool> {
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
                            Lang::Num(_) => true,
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
                  i32.const 160268115
                  i32.const -160268059
                  i32.add)
                (export "exported_func" (func 0)))
            "#,
            0,
        );
    }

    #[test]
    fn test_peep_stack_neutral1() {
        let rules: &[Rewrite<super::Lang, PeepholeMutationAnalysis>] =
            &[rewrite!("strength-undo";  "(shl ?x 1)" => "(mul ?x 2)")];

        test_peephole_mutator(
            r#"
        (module
            (func (export "exported_func") (result i32) (local i32 i32)
                i32.const 42
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
                    i32.const 42
                    i32.const 2
                    i32.mul)
                (export "exported_func" (func 0)))
            "#,
            2,
        );
    }

    #[test]
    fn test_peep_stack_neutral2() {
        let rules: &[Rewrite<super::Lang, PeepholeMutationAnalysis>] =
            &[rewrite!("strength-undo";  "?x" => "(or ?x ?x)")];

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
            2,
        );
    }

    #[test]
    fn test_peep_irelop1() {
        let rules: &[Rewrite<super::Lang, PeepholeMutationAnalysis>] =
            &[rewrite!("strength-undo";  "(eqz ?x)" => "(eq ?x 0)")];

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
    fn test_peep_strength() {
        let rules: &[Rewrite<super::Lang, PeepholeMutationAnalysis>] =
            &[rewrite!("strength-undo";  "(shl ?x 1)" => "(mul ?x 2)")];

        test_peephole_mutator(
            r#"
        (module
            (func (export "exported_func") (result i32) (local i32 i32)
                i32.const 42
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
                  i32.const 2
                  i32.mul)
                (export "exported_func" (func 0)))
            "#,
            0,
        );
    }

    #[test]
    fn test_peep_commutative() {
        let rules: &[Rewrite<super::Lang, PeepholeMutationAnalysis>] =
            &[rewrite!("commutative-1";  "(add ?x ?y)" => "(add ?y ?x)")];

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
                  i32.add)
                (export "exported_func" (func 0)))
            "#,
            0,
        );
    }

    #[test]
    fn test_peep_inversion() {
        let rules: &[Rewrite<super::Lang, PeepholeMutationAnalysis>] =
            &[rewrite!("inversion-1";  "(gt_s ?x ?y)" => "(le_s ?y ?x)")];

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
        let rules: &[Rewrite<super::Lang, PeepholeMutationAnalysis>] =
            &[rewrite!("inversion-1";  "(gt_u ?x ?y)" => "(le_u ?y ?x)")];

        test_peephole_mutator(
            r#"
        (module
            (func (export "exported_func") (result i32) (local i32 i32)
                i32.const 42
                i32.const 1
                i32.gt_u
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
                  i32.le_u)
                (export "exported_func" (func 0)))
            "#,
            0,
        );
    }

    #[test]
    fn test_peep_shl0() {
        let rules: &[Rewrite<super::Lang, PeepholeMutationAnalysis>] =
            &[rewrite!("strength-undo3";  "(shr_u ?x ?y)" => "(shl (shr_u ?x ?y) 0)" )];

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
            1,
        );
    }

    #[test]
    fn test_peep_idem1() {
        let rules: &[Rewrite<super::Lang, PeepholeMutationAnalysis>] =
            &[rewrite!("idempotent-1";  "?x" => "(or ?x ?x)")];

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
    fn test_peep_rots() {
        let rules: &[Rewrite<super::Lang, PeepholeMutationAnalysis>] =
            &[rewrite!("idempotent-1";  "?x" => "(or ?x ?x)")];

        test_peephole_mutator(
            r#"
        (module
            (func (export "exported_func") (result i32) (local i32 i32)
                i32.const 56
                i32.const 2
                i32.rotr
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
                i32.const 2
                i32.rotr
                i32.const 56
                i32.const 2
                i32.rotr
                i32.or)
            (export "exported_func" (func 0)))
        "#,
            1,
        );
    }

    #[test]
    fn test_peep_rems() {
        let rules: &[Rewrite<super::Lang, PeepholeMutationAnalysis>] =
            &[rewrite!("idempotent-1";  "?x" => "(or ?x ?x)")];

        test_peephole_mutator(
            r#"
        (module
            (func (export "exported_func") (result i32) (local i32 i32)
                i32.const 56
                i32.const 2
                i32.rem_s
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
                i32.const 2
                i32.rem_s
                i32.const 56
                i32.const 2
                i32.rem_s
                i32.or)
            (export "exported_func" (func 0)))
        "#,
            1,
        );
    }

    #[test]
    fn test_peep_idem3() {
        let rules: &[Rewrite<super::Lang, PeepholeMutationAnalysis>] =
            &[rewrite!("idempotent-3";  "?x" => "(add ?x 0)")];

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
        let rules: &[Rewrite<super::Lang, PeepholeMutationAnalysis>] =
            &[rewrite!("idempotent-4";  "?x" => "(mul ?x 1)")];

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
        let rules: &[Rewrite<super::Lang, PeepholeMutationAnalysis>] =
            &[rewrite!("type1-1";  "?x" => "(shr_u ?x ?x)" )];

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
            &[rewrite!("type1-1";  "(add ?x ?y)" => "(add ?y ?x)" )];

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
            0,
        );
    }

    #[test]
    fn test_peep_functions() {
        let rules: &[Rewrite<super::Lang, PeepholeMutationAnalysis>] =
            &[rewrite!("type1-1";  "(call ?fidx ?x)" => "(call ?fidx 1) " )];

        test_peephole_mutator(
            r#"
        (module
            (func (export "exported_func")(param i32 )  (result i32)  (local i32 i32)
                local.get 0
                call 0
            )
        )
        "#,
            rules,
            r#"
            (module
                (func (export "exported_func") (param i32 )  (result i32)  (local i32 i32)
                    i32.const 1
                    call 0
                )
            )
        "#,
            5,
        );
    }

    #[test]
    fn test_peep_locals2() {
        let rules: &[Rewrite<super::Lang, PeepholeMutationAnalysis>] =
            &[rewrite!("type1-1";  "(add ?x ?y)" => "(add ?y ?x)" )];

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
    fn test_peep_mem_shift() {
        let rules: &[Rewrite<super::Lang, PeepholeMutationAnalysis>] =
            &[rewrite!("mem-load-shift";  "(load ?x ?y ?z ?w)" => "(load (add ?x rand) ?y ?z ?w)")];

        test_peephole_mutator(
            r#"
        (module
            (memory 1)
            (func (export "exported_func") (param i32) (result i32)
                i32.const 42
                i32.load
            )
        )
        "#,
            rules,
            r#"
            (module
                (type (;0;) (func (param i32) (result i32)))
                (func (;0;) (type 0) (param i32) (result i32)
                  i32.const 42
                  i32.const -1267761968
                  i32.add
                  i32.load)
                (memory (;0;) 1)
                (export "exported_func" (func 0)))
        "#,
            1,
        );
    }

    #[test]
    fn test_peep_mem_popout() {
        let rules: &[Rewrite<super::Lang, PeepholeMutationAnalysis>] =
            &[rewrite!("mem-load-shift";  "(load ?x ?y ?z ?w)" => "(load (add ?x ?y) 0 ?z ?w)")];

        test_peephole_mutator(
            r#"
        (module
            (memory 1)
            (func (export "exported_func") (param i32) (result i32)
                i32.const 42
                i32.load offset=100
            )
        )
        "#,
            rules,
            r#"
            (module
                (type (;0;) (func (param i32) (result i32)))
                (func (;0;) (type 0) (param i32) (result i32)
                  i32.const 42
                  i32.const 100
                  i32.add
                  i32.load)
                (memory (;0;) 1)
                (export "exported_func" (func 0)))
        "#,
            1,
        );
    }

    #[test]
    fn test_peep_globals1() {
        let rules: &[Rewrite<super::Lang, PeepholeMutationAnalysis>] =
            &[rewrite!("mem-load-shift";  "?x" => "(add ?x 0)")];

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
        let rules: &[Rewrite<super::Lang, PeepholeMutationAnalysis>] =
            &[rewrite!("mem-load-shift";  "?x" => "(add ?x 0)")];

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

                (global $0 (mut i32) i32.const 0)
                (func (;0;) (type 0) (param i32) (result i32)
                  i32.const 10
                  i32.const 0  
                  i32.add
                    global.set $0
                    i32.const 20
                )
                (memory (;0;) 1)
                (export "exported_func" (func 0)))
        "#,
            1,
        );
    }

    #[test]
    fn test_peep_locals3() {
        let rules: &[Rewrite<super::Lang, PeepholeMutationAnalysis>] =
            &[rewrite!("mem-load-shift";  "?x" => "(add ?x 0)")];

        test_peephole_mutator(
            r#"
        (module
            (memory 1)
            (global $0 (mut i32) i32.const 0)
            (func (export "exported_func") (param i32) (result i32)
                i32.const 120
                local.tee 0
            )
        )
        "#,
            rules,
            r#"
            (module
                (type (;0;) (func (param i32) (result i32)))

                (global $0 (mut i32) i32.const 0)
                (func (;0;) (type 0) (param i32) (result i32)
                    i32.const 120
                    local.tee 0
                    i32.const 0
                    i32.add
                )
                (memory (;0;) 1)
                (export "exported_func" (func 0)))
        "#,
            3,
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
        assert_eq!(expectedtext, text);
    }
}
