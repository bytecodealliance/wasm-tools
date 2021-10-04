//! This mutator applies a random peephole transformation to the input Wasm module
use crate::error::EitherType;
use crate::module::PrimitiveTypeInfo;
use crate::mutators::peephole::eggsy::analysis::PeepholeMutationAnalysis;
use crate::mutators::peephole::eggsy::encoder::Encoder;
use crate::mutators::peephole::eggsy::lang::Lang;
use egg::{rewrite, AstSize, Id, RecExpr, Rewrite, Runner, Subst};
use rand::{prelude::SmallRng, Rng};
use std::convert::TryFrom;
use std::{cmp::Ordering, collections::HashMap, hash::Hash, num::Wrapping};
use wasm_encoder::{CodeSection, Function, Instruction, MemArg, Module, ValType};
use wasmparser::{
    BinaryReaderError, CodeSectionReader, FunctionBody, LocalsReader, Operator, Range, Type,
};

// Hack to show debug messages in tests
#[cfg(not(test))]
use log::debug;
#[cfg(test)]
use std::println as debug;

use crate::{
    module::{map_operator, map_type},
    ModuleInfo, Result, WasmMutate,
};

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

        println!("Locals {:?}", current_locals);
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

            let locals = self.get_func_locals(&info, fidx, &mut localsreader)?;

            for oidx in (opcode_to_mutate..operatorscount).chain(0..opcode_to_mutate) {
                debug!("Trying with oidx {:?} operator {:?}", oidx, operators[oidx]);
                let mut dfg = DFGIcator::new();
                let basicblock = dfg.get_bb_from_operator(oidx, &operators);
                debug!("Basic block range {:?} for idx {:?}", basicblock, oidx);

                match basicblock {
                    Some(basicblock) => {
                        let minidfg = dfg.get_dfg(&operators, &basicblock, &locals);
                        match minidfg {
                            None => {
                                continue;
                            }
                            Some(minidfg) => {
                                debug!("DFG {:?}", minidfg);
                                if !minidfg.map.contains_key(&oidx) {
                                    continue;
                                }
                                // Create an eterm expression from the basic block starting at oidx
                                let mut start = RecExpr::<Lang>::default();
                                debug!("Translating Wasm basic block to recexpr");
                                let (_, symbolsmap) =
                                    Encoder::wasm2expr(&minidfg, oidx, &operators, &mut start)?;
                                debug!("Eterm `{}`", start);

                                // TODO have this as a condition
                                let undef = Lang::Undef;
                                if start.as_ref().contains(&undef) {
                                    debug!("The dfg is not deterministic");
                                    continue;
                                }

                                let runner = Runner::default().with_expr(&start).run(rules);
                                let mut egraph = runner.egraph;

                                // In theory this will return the Id of the operator eterm
                                let root = egraph.add_expr(&start);

                                // This cost function could be replaced by a custom weighted probability, for example
                                // we could modify the cost function based on the previous mutation/rotation outcome
                                let cf = AstSize;
                                let extractor = RandomExtractor::new(&egraph, cf);

                                let newexpr = extractor.extract_random(
                                    rnd,
                                    root,
                                    0,
                                    /* only 1 for now */ Encoder::build_expr,
                                )?;

                                // There is no point in generating the same symbol
                                // TODO, Move this to try all possible before continuing
                                if newexpr.to_string().eq(&start.to_string()) {
                                    debug!("The generated random `{}` is the same intial expression, going to next operator", newexpr);
                                    continue;
                                }
                                debug!(
                                    "Mutating function {:?} at {:?} with {}",
                                    fidx, oidx, newexpr
                                );
                                // Create a new function using the previous locals
                                let mut newfunc = self.copy_locals(reader)?;

                                // Translate lang expr to wasm
                                Encoder::build_function(
                                    info,
                                    rnd,
                                    oidx,
                                    &newexpr,
                                    &operators,
                                    &basicblock,
                                    &mut newfunc,
                                    &symbolsmap,
                                    &minidfg,
                                )?;

                                debug!("Built function {:?}", newfunc);
                                return Ok((newfunc, fidx));
                            }
                        }
                    }
                    None => {
                        debug!("No valid basic block {:?}  for {}", basicblock, oidx);
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
        // Calculate here type related information for parameters, locals and returns
        // This information could be passed to the conditions to check for type correctness rewriting

        let mut rules = vec![
            rewrite!("unfold-2";  "?x" => "(unfold ?x)" if self.is_const("?x") ), // Use a custom instruction-mutator for this
            // This specific rewriting rule has a condition, it should be appplied if the operand is a constant
            rewrite!("strength-undo";  "(i32.shl ?x 1)" => "(i32.mul ?x ?x)"),
            rewrite!("strength-undo1";  "(i32.shl ?x 2)" => "(i32.mul ?x 2)"),
            rewrite!("strength-undo2";  "(i32.shl ?x 3)" => "(i32.mul ?x 8)"),
            rewrite!("add-1";  "(i32.add ?x ?x)" => "(i32.mul ?x 2)"),
            rewrite!("idempotent-1";  "?x" => "(i32.or ?x ?x)" ),
            rewrite!("idempotent-2";  "?x" => "(i32.and ?x ?x)"),
            rewrite!("commutative-1";  "(i32.add ?x ?y)" => "(i32.add ?y ?x)" ),
            rewrite!("commutative-2";  "(i32.mul ?x ?y)" => "(i32.mul ?y ?x)" ),
            rewrite!("associative-1";  "(i32.add ?x (i32.add ?y ?z))" => "(i32.add (i32.add ?x ?y) ?z)" ),
            rewrite!("associative-2";  "(i32.mul ?x (i32.mul ?y ?z))" => "(i32.mul (i32.mul ?x ?y) ?z)" ),
        ];

        if !config.preserve_semantics {
            rules.push(rewrite!("mem-load-shift";  "(i.load ?x)" => "(i.load (i32.add ?x rand))"))
            // Check why this is generating a lot of the same replacements
            // Add corretness attraction ones
            // x  = x + 1
            //
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
                  i32.const 160268115
                  i32.const 160268059
                  i32.add)
                (export "exported_func" (func 0)))
            "#,
            0,
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
                  i32.add)
                (export "exported_func" (func 0)))
            "#,
            1,
        );
    }

    #[test]
    fn test_peep_commutative2() {
        let rules: &[Rewrite<super::Lang, PeepholeMutationAnalysis>] =
            &[rewrite!("commutative-1";  "(i32.add ?x ?y)" => "(i32.add ?y ?x)")];

        test_peephole_mutator(
            r#"
        (module
            (func (export "exported_func") (result i32) (local i32 i32)
                i32.const 1
                if
                    i32.const 67
                    drop
                else
                    i32.const 100
                    drop
                end
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
                    if
                        i32.const 67
                        drop
                    else
                        i32.const 100
                        drop
                    end
                  i32.const 42
                  i32.add)
                (export "exported_func" (func 0)))
            "#,
            0,
        );
    }

    /// Condition to apply when the tree needs to be consistent
    /// Checks if no undef oeprators are in the tree
    fn is_complete(var1: &'static str) -> impl Fn(&mut EG, Id, &Subst) -> bool {
        let var1 = var1.parse().unwrap();
        let undef = Lang::Undef;
        move |egraph, _, subst| !egraph[subst[var1]].nodes.contains(&undef)
    }

    #[test]
    fn test_peep_commutative_with_undef() {
        let rules: &[Rewrite<super::Lang, PeepholeMutationAnalysis>] = &[
            rewrite!("commutative-1";  "(i32.add ?x ?y)" => "(i32.add ?y ?x)" if is_complete("?x") if is_complete("?y") ),
        ];

        // With the condition this should fail
        test_peephole_mutator(
            r#"
        (module
            (func (export "exported_func") (result i32) (local i32 i32)
                i32.const 1
                if (result i32)
                    i32.const 67
                else
                    i32.const 100
                end
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
                    if
                        i32.const 67
                        drop
                    else
                        i32.const 100
                        drop
                    end
                  i32.const 1
                  i32.const 42
                  i32.add)
                (export "exported_func" (func 0)))
            "#,
            0,
        );
    }

    #[test]
    fn test_peep_idem1() {
        let rules: &[Rewrite<super::Lang, PeepholeMutationAnalysis>] =
            &[rewrite!("idempotent-1";  "?x" => "(i32.or ?x ?x)" if is_const("?x"))];

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
    fn test_peep_mem_shift() {
        let rules: &[Rewrite<super::Lang, PeepholeMutationAnalysis>] =
            &[rewrite!("mem-load-shift";  "(i.load ?x)" => "(i.load (i32.add ?x rand))")];

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
                  i32.const -683260416
                  i32.add
                  i32.const 160268115
                  i32.add
                  i32.const 991484282
                  i32.add
                  i32.const 2018993756
                  i32.add
                  i32.const -1908705872
                  i32.add
                  i32.const -1399093629
                  i32.add
                  i32.const 1735359708
                  i32.add
                  i32.const -1016670648
                  i32.add
                  i32.const 1897657819
                  i32.add
                  i32.const 1922808570
                  i32.add
                  i32.const -1502375410
                  i32.add
                  i32.const 2005067762
                  i32.add
                  i32.const -1030639517
                  i32.add
                  i32.const 1748478738
                  i32.add
                  i32.const 500342713
                  i32.add
                  i32.const -396939652
                  i32.add
                  i32.const 154479202
                  i32.add
                  i32.const 686992112
                  i32.add
                  i32.const -1751313776
                  i32.add
                  i32.const -1875541192
                  i32.add
                  i32.const 1750141307
                  i32.add
                  i32.load)
                (memory (;0;) 1)
                (export "exported_func" (func 0)))
        "#,
            0,
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
    }
}
