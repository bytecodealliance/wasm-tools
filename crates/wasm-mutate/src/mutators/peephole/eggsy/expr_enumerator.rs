use std::{
    cell::{Cell, RefCell},
    convert::TryFrom,
};

use egg::{AstSize, CostFunction, Id, Language, RecExpr};
use rand::{prelude::SmallRng, Rng, SeedableRng};

use crate::{
    mutators::peephole::{
        eggsy::{encoder::rebuild::build_expr, RandomExtractor},
        EG,
    },
    WasmMutate,
};

use super::lang::Lang;

struct ENodeIterator;

pub fn lazy_expand<'a>(
    id: Id,
    egraph: &'a EG,
    depth: u32,
    rnd: &'a RefCell<&'a mut SmallRng>,
) -> impl Iterator<Item = String> + 'a {
    let t: Box<dyn Iterator<Item = String>> = if depth == 0 {
        // TODO, Consume fuel
        let cf = AstSize;
        let extractor = RandomExtractor::new(&egraph, cf);
        let shorter = extractor.extract_shorter(id, build_expr).unwrap();
        Box::new(vec![format!("{}", shorter)].into_iter())
    } else {
        let nodes = egraph[id].nodes.clone();
        let count = nodes.len();
        // For each eclass, at least one node exists
        let split_at = rnd.borrow_mut().gen_range(0, count);
        let indices = (0..split_at).into_iter().chain(split_at..count).into_iter();
        let t = indices
            .map(move |i| nodes[i].clone())
            .map(move |l| {
                let n = depth - 1;
                let eg = egraph.clone();
                let lc = l.clone();

                let iter: Box<dyn Iterator<Item = String>> = match lc {
                    Lang::I32Mul([left, right]) | Lang::I32Add([left, right]) => {
                        let lcope = left.clone();
                        let rcopy = right.clone();
                        let lc2 = lc.clone();
                        // zip the expansion of the operands
                        let t = lazy_expand(lcope, eg, n, rnd)
                            .flat_map(move |e| {
                                std::iter::repeat(e).zip(lazy_expand(rcopy, eg, n, rnd))
                            })
                            .map(move |(l, r)| {
                                format!("({} {} {})", lc2.display_op().to_string(), l, r)
                            });

                        Box::new(t)
                    }
                    Lang::UnfoldI32(arg) => {
                        let lcope = arg.clone();
                        // zip the expansion of the operands
                        let t = lazy_expand(lcope, eg, n, rnd)
                            .map(move |l| format!("(i32.unfold {} )", l));

                        Box::new(t)
                    }
                    i @ Lang::I32(_) => Box::new(vec![i.display_op().to_string()].into_iter()),
                    _ => unreachable!("todo"),
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

    use egg::{rewrite, AstSize, Id, Language, RecExpr, Rewrite, Runner};
    use rand::{prelude::SmallRng, SeedableRng};

    use crate::mutators::peephole::{
        eggsy::{analysis::PeepholeMutationAnalysis, expr_enumerator::lazy_expand, lang::Lang},
        EG,
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
            rewrite!("rule2";  "(i32.add ?y ?x)" => "(i32.add ?x ?y)"),
            rewrite!("rule3";  "?x" => "(i32.mul ?x 1_i32)"),
            rewrite!("rule4";  "0_i32" => "(i32.unfold 0_i32)"),
            rewrite!("rule5";  "1_i32" => "(i32.unfold 1_i32)"),
        ];

        let expr = "(i32.add 100_i32 200_i32)";

        let analysis = PeepholeMutationAnalysis::new(vec![], vec![], vec![], vec![]);
        let runner = Runner::<Lang, PeepholeMutationAnalysis, ()>::new(analysis)
            .with_iter_limit(1) // only one iterations, do not wait for eq saturation, increasing only by one it affects the execution time of the mutator by a lot
            .with_expr(&expr.parse().unwrap())
            .run(rules);
        let mut egraph = runner.egraph;
        let cf = AstSize;

        let enumeration_start = std::time::Instant::now();
        let root = egraph.add_expr(&expr.parse().unwrap());
        let mut rnd = SmallRng::seed_from_u64(0);
        let rnd = RefCell::new(&mut rnd);
        let mut it = lazy_expand(root, &egraph, 4, &rnd);
        let elapsed = enumeration_start.elapsed();
        eprintln!(
            "Construction {}.{:03} seconds",
            elapsed.as_secs(),
            elapsed.subsec_millis()
        );

        for i in 0..100000 {
            let enumeration_start = std::time::Instant::now();
            if let Some(it) = it.next() {
                println!("{} {}", i, it);
                let elapsed = enumeration_start.elapsed();
                eprintln!(
                    "Done enumerating in {}.{:03} seconds",
                    elapsed.as_secs(),
                    elapsed.subsec_millis()
                );
                // Parsing with our Parser :)

                let r = RecExpr::<Lang>::from_str(&it).unwrap();
            } else {
                break;
            }
        }
    }
}
