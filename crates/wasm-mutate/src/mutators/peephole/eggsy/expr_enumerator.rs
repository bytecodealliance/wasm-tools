use std::{
    cell::{RefCell},
};

use crate::{
    mutators::peephole::{
        eggsy::{encoder::rebuild::build_expr, RandomExtractor},
        EG,
    },
};
use egg::{AstSize, Id, Language};
use rand::{prelude::SmallRng, Rng};

use super::lang::Lang;

macro_rules! product(
    ($first:ident, $($next:ident),*) => (
        $first.iter() $(
            .flat_map(|e| std::iter::Repeat::new(e)
                .zip($next.iter()))
        )*
    );
);

/// Returns a lazy iterator over random expressions in the eggraph
///
pub fn lazy_expand<'a>(
    id: Id,
    egraph: EG,
    depth: u32,
    rnd: &'a RefCell<&'a mut SmallRng>,
) -> impl Iterator<Item = String> + 'a {
    println!("Calling for {}", id);
    // Notice that the result is a String, it is easier to construct the expression like this
    // future implementation can take a mut RecExpr and just return the Id of the expression on
    // each `lazy_expand` iteration
    let t: Box<dyn Iterator<Item = String>> = if depth == 0 {
        let cf = AstSize;
        let extractor = RandomExtractor::new(&egraph, cf);
        let shorter = extractor.extract_shorter(id, build_expr).unwrap();
        Box::new(vec![format!("{}", shorter)].into_iter())
    } else {
        let nodes = egraph[id].nodes.clone();
        let count = nodes.len();
        // For each eclass, at least one node exists
        let split_at = rnd.borrow_mut().gen_range(0, count);
        let indices = (0..split_at).into_iter().chain(split_at..count);
        let t = indices
            .map(move |i| nodes[i].clone())
            .map(move |l| {
                let n = depth - 1;
                let eg = egraph.clone();
                let lc = l;

                let iter: Box<dyn Iterator<Item = String>> = match lc {
                    Lang::I32Store {
                        value_and_offset: [right, left],
                        ..
                    }
                    | Lang::I64Store {
                        value_and_offset: [right, left],
                        ..
                    }
                    | Lang::F32Store {
                        value_and_offset: [right, left],
                        ..
                    }
                    | Lang::F64Store {
                        value_and_offset: [right, left],
                        ..
                    }
                    | Lang::I32Store8 {
                        value_and_offset: [right, left],
                        ..
                    }
                    | Lang::I32Store16 {
                        value_and_offset: [right, left],
                        ..
                    }
                    | Lang::I64Store8 {
                        value_and_offset: [right, left],
                        ..
                    }
                    | Lang::I64Store16 {
                        value_and_offset: [right, left],
                        ..
                    }
                    | Lang::I64Store32 {
                        value_and_offset: [right, left],
                        ..
                    }
                    | Lang::I32Add([left, right])
                    | Lang::I64Add([left, right])
                    | Lang::I32Sub([left, right])
                    | Lang::I64Sub([left, right])
                    | Lang::I32Mul([left, right])
                    | Lang::I64Mul([left, right])
                    | Lang::I32And([left, right])
                    | Lang::I64And([left, right])
                    | Lang::I32Or([left, right])
                    | Lang::I64Or([left, right])
                    | Lang::I32Xor([left, right])
                    | Lang::I64Xor([left, right])
                    | Lang::I32Shl([left, right])
                    | Lang::I64Shl([left, right])
                    | Lang::I32ShrU([left, right])
                    | Lang::I64ShrU([left, right])
                    | Lang::I32DivU([left, right])
                    | Lang::I64DivU([left, right])
                    | Lang::I32DivS([left, right])
                    | Lang::I64DivS([left, right])
                    | Lang::I32ShrS([left, right])
                    | Lang::I64ShrS([left, right])
                    | Lang::I32RotR([left, right])
                    | Lang::I64RotR([left, right])
                    | Lang::I32RotL([left, right])
                    | Lang::I64RotL([left, right])
                    | Lang::I32RemS([left, right])
                    | Lang::I64RemS([left, right])
                    | Lang::I32RemU([left, right])
                    | Lang::I64RemU([left, right])
                    | Lang::I32Eq([left, right])
                    | Lang::I64Eq([left, right])
                    | Lang::I32Ne([left, right])
                    | Lang::I64Ne([left, right])
                    | Lang::I32LtS([left, right])
                    | Lang::I64LtS([left, right])
                    | Lang::I32LtU([left, right])
                    | Lang::I64LtU([left, right])
                    | Lang::I32GtS([left, right])
                    | Lang::I64GtS([left, right])
                    | Lang::I32GtU([left, right])
                    | Lang::I64GtU([left, right])
                    | Lang::I32LeS([left, right])
                    | Lang::I64LeS([left, right])
                    | Lang::I32LeU([left, right])
                    | Lang::I64LeU([left, right])
                    | Lang::I32GeS([left, right])
                    | Lang::I64GeS([left, right])
                    | Lang::I32GeU([left, right])
                    | Lang::I64GeU([left, right])
                    | Lang::F32Add([left, right])
                    | Lang::F64Add([left, right])
                    | Lang::F32Sub([left, right])
                    | Lang::F64Sub([left, right])
                    | Lang::F32Mul([left, right])
                    | Lang::F64Mul([left, right])
                    | Lang::F32Div([left, right])
                    | Lang::F64Div([left, right])
                    | Lang::F32Min([left, right])
                    | Lang::F64Min([left, right])
                    | Lang::F32Max([left, right])
                    | Lang::F64Max([left, right])
                    | Lang::F32Copysign([left, right])
                    | Lang::F64Copysign([left, right])
                    | Lang::F32Eq([left, right])
                    | Lang::F64Eq([left, right])
                    | Lang::F32Ne([left, right])
                    | Lang::F64Ne([left, right])
                    | Lang::F32Lt([left, right])
                    | Lang::F64Lt([left, right])
                    | Lang::F32Gt([left, right])
                    | Lang::F64Gt([left, right])
                    | Lang::F32Le([left, right])
                    | Lang::F64Le([left, right])
                    | Lang::F32Ge([left, right])
                    | Lang::F64Ge([left, right]) => {
                        let lcope = left;
                        let rcopy = right;
                        let lc2 = lc.clone();
                        // zip the expansion of the operands
                        let t = lazy_expand(lcope, eg.clone(), n, rnd)
                            .flat_map(move |e| {
                                std::iter::repeat(e).zip(lazy_expand(rcopy, eg.clone(), n, rnd))
                            })
                            .map(move |(l, r)| {
                                format!("({} {} {})", lc2.display_op(), l, r)
                            });

                        Box::new(t)
                    }
                    Lang::Call(idx, arguments) => {
                        // FIXME
                        // I could not find a way to have a cartesian product of dynamic size collection of Iterators
                        // This can also be solved if we turn Call and Container enodes to be binary trees where
                        // the left operand is a real Id and the second is a container, and so on until all
                        // arguments are expressed in the binary tree
                        // For example
                        // (call.$1 a b c d) can be turned into (call.$1 a (container b (container c (container d)))))
                        let mut expr = String::from("");
                        for a in &arguments {
                            expr.push_str(&format!(
                                " {}",
                                lazy_expand(*a, eg.clone(), n, rnd).next().unwrap()
                            ));
                        }

                        let t = std::iter::once(format!("(call.{}{})", idx, expr));
                        Box::new(t)
                    }

                    Lang::Container(arguments) => {
                        // FIXME
                        // Same as Call
                        let mut expr = String::from("");
                        for a in &arguments {
                            expr.push_str(&format!(
                                " {}",
                                lazy_expand(*a, eg.clone(), n, rnd).next().unwrap()
                            ));
                        }

                        let t = std::iter::once(format!("(container {})", expr));
                        Box::new(t)
                    }
                    Lang::F32Load { offset: arg, .. }
                    | Lang::F64Load { offset: arg, .. }
                    | Lang::I32Load8S { offset: arg, .. }
                    | Lang::I32Load8U { offset: arg, .. }
                    | Lang::I32Load16S { offset: arg, .. }
                    | Lang::I32Load16U { offset: arg, .. }
                    | Lang::I64Load8S { offset: arg, .. }
                    | Lang::I64Load8U { offset: arg, .. }
                    | Lang::I64Load16S { offset: arg, .. }
                    | Lang::I64Load16U { offset: arg, .. }
                    | Lang::I64Load32S { offset: arg, .. }
                    | Lang::I64Load32U { offset: arg, .. }
                    | Lang::I64Load { offset: arg, .. }
                    | Lang::I32Load { offset: arg, .. }
                    | Lang::LocalSet(_, arg)
                    | Lang::LocalTee(_, arg)
                    | Lang::GlobalSet(_, arg)
                    | Lang::F32Abs([arg])
                    | Lang::F64Abs([arg])
                    | Lang::F32Neg([arg])
                    | Lang::F64Neg([arg])
                    | Lang::F32Sqrt([arg])
                    | Lang::F64Sqrt([arg])
                    | Lang::F32Ceil([arg])
                    | Lang::F64Ceil([arg])
                    | Lang::F32Floor([arg])
                    | Lang::F64Floor([arg])
                    | Lang::F32Trunc([arg])
                    | Lang::F64trunc([arg])
                    | Lang::F32Nearest([arg])
                    | Lang::F64Nearest([arg])
                    | Lang::Wrap([arg])
                    | Lang::I32Extend8S([arg])
                    | Lang::I64Extend8S([arg])
                    | Lang::I32Extend16S([arg])
                    | Lang::I64Extend16S([arg])
                    | Lang::I64Extend32S([arg])
                    | Lang::I64ExtendI32S([arg])
                    | Lang::I64ExtendI32U([arg])
                    | Lang::I32TruncF32S([arg])
                    | Lang::I32TruncF32U([arg])
                    | Lang::I32TruncF64S([arg])
                    | Lang::I32TruncF64U([arg])
                    | Lang::I64TruncF32S([arg])
                    | Lang::I64TruncF32U([arg])
                    | Lang::I64TruncF64S([arg])
                    | Lang::I64TruncF64U([arg])
                    | Lang::F32ConvertI32S([arg])
                    | Lang::F32ConvertI32U([arg])
                    | Lang::F32ConvertI64S([arg])
                    | Lang::F32ConvertI64U([arg])
                    | Lang::F32DemoteF64([arg])
                    | Lang::F64ConvertI32S([arg])
                    | Lang::F64ConvertI32U([arg])
                    | Lang::F64ConvertI64S([arg])
                    | Lang::F64ConvertI64U([arg])
                    | Lang::F64PromoteF32([arg])
                    | Lang::I32ReinterpretF32([arg])
                    | Lang::I64ReinterpretF64([arg])
                    | Lang::F32ReinterpretI32([arg])
                    | Lang::F64ReinterpretI64([arg])
                    | Lang::I32TruncSatF32S([arg])
                    | Lang::I32TruncSatF32U([arg])
                    | Lang::I32TruncSatF64S([arg])
                    | Lang::I32TruncSatF64U([arg])
                    | Lang::I64TruncSatF32S([arg])
                    | Lang::I64TruncSatF32U([arg])
                    | Lang::I64TruncSatF64S([arg])
                    | Lang::I64TruncSatF64U([arg])
                    | Lang::Drop([arg])
                    | Lang::I32Eqz([arg])
                    | Lang::I64Eqz([arg])
                    | Lang::I32Popcnt([arg])
                    | Lang::I64Popcnt([arg])
                    | Lang::I32Clz([arg])
                    | Lang::I32Ctz([arg])
                    | Lang::I64Ctz([arg])
                    | Lang::I64Clz([arg])
                    | Lang::UnfoldI64(arg)
                    | Lang::UnfoldI32(arg) => {
                        let lcope = arg;
                        let lc2 = lc.clone();
                        // zip the expansion of the operands
                        let t = lazy_expand(lcope, eg, n, rnd)
                            .map(move |l| format!("({} {})", lc2.display_op(), l));

                        Box::new(t)
                    }
                    i @ Lang::I32(_) => Box::new(vec![i.display_op().to_string()].into_iter()),
                    i @ Lang::I64(_) => Box::new(vec![i.display_op().to_string()].into_iter()),
                    i @ Lang::F32(_) => Box::new(vec![i.display_op().to_string()].into_iter()),
                    i @ Lang::F64(_) => Box::new(vec![i.display_op().to_string()].into_iter()),
                    l @ Lang::LocalGet(_) => Box::new(vec![l.display_op().to_string()].into_iter()),
                    g @ Lang::GlobalGet(_) => {
                        Box::new(vec![g.display_op().to_string()].into_iter())
                    }
                    u @ Lang::Undef => Box::new(vec![u.display_op().to_string()].into_iter()),
                    n @ Lang::Nop => Box::new(vec![n.display_op().to_string()].into_iter()),
                    r32 @ Lang::RandI32 => Box::new(vec![r32.display_op().to_string()].into_iter()),
                    r64 @ Lang::RandI64 => Box::new(vec![r64.display_op().to_string()].into_iter()),
                    //_ => unreachable!("not implemented {:?}", lc),
                };
                iter
            })
            .flatten();
        Box::new(t)
    };
    t
}

#[cfg(test)]
mod tests {
    use std::{cell::RefCell, str::FromStr};

    use egg::{rewrite, AstSize, RecExpr, Rewrite, Runner};
    use rand::{prelude::SmallRng, SeedableRng};

    use crate::mutators::peephole::{
        eggsy::{analysis::PeepholeMutationAnalysis, expr_enumerator::lazy_expand, lang::Lang},
    };

    #[derive(Clone, Copy, Debug)]
    struct Mimi {
        id: usize,
    }
    impl Mimi {
        pub fn used(&self) {
            println!("mimi {}", self.id)
        }
    }
    #[test]
    fn test_lazy() {
        let O1 = vec![Mimi { id: 1 }, Mimi { id: 2 }].into_iter();
        let O2 = vec![Mimi { id: 2 }, Mimi { id: 3 }].into_iter();

        let mut l = O1
            .flat_map(|e| std::iter::repeat(e).zip(O2.clone()))
            .map(|(l, r)| {
                l.used();
                r.used();
                (l, r)
            });

        println!("1 {:?}", l.next());
        println!("2 {:?}", l.next());
        println!("3 {:?}", l.next());
        println!("4 {:?}", l.next());
    }

    #[test]
    fn test_rec_iterator() {
        let rules: &[Rewrite<Lang, PeepholeMutationAnalysis>] = &[
            rewrite!("rule";  "?x" => "(i32.add ?x 0_i32)"),
            //rewrite!("rule2";  "(i32.add ?y ?x)" => "(i32.add ?x ?y)"),
            //rewrite!("rule3";  "?x" => "(i32.mul ?x 1_i32)"),
            //rewrite!("rule4";  "0_i32" => "(i32.unfold 0_i32)"),
            //rewrite!("rule5";  "1_i32" => "(i32.unfold 1_i32)"),
        ];

        let expr = "(i32.add 100_i32 200_i32)";

        let analysis = PeepholeMutationAnalysis::new(vec![], vec![], vec![], vec![]);
        let runner = Runner::<Lang, PeepholeMutationAnalysis, ()>::new(analysis)
            .with_iter_limit(1) // only one iterations, do not wait for eq saturation, increasing only by one it affects the execution time of the mutator by a lot
            .with_expr(&expr.parse().unwrap())
            .run(rules);
        let mut egraph = runner.egraph;
        let _cf = AstSize;

        let _enumeration_start = std::time::Instant::now();
        let root = egraph.add_expr(&expr.parse().unwrap());
        let mut rnd = SmallRng::seed_from_u64(0);
        let rnd = RefCell::new(&mut rnd);

        let mut it = lazy_expand(root, egraph, 4, &rnd);

        for i in 0..100000 {
            if let Some(it) = it.next() {
                println!("{} {}", i, it);
                // Parsing with our Parser :)
                let _r = RecExpr::<Lang>::from_str(&it).unwrap();
            } else {
                break;
            }
        }
    }
}
