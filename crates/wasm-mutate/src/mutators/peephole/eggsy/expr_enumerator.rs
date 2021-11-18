use std::convert::TryFrom;

use egg::{AstSize, CostFunction, Id, Language, RecExpr};

use crate::mutators::peephole::{
    eggsy::{encoder::rebuild::build_expr, RandomExtractor},
    EG,
};

use super::lang::Lang;

struct ENodeIterator;

fn lazy_expand(id: Id, egraph: &EG, depth: u32) -> impl Iterator<Item = String> + '_ {
    let t: Box<dyn Iterator<Item = String>> = if depth == 0 {
        let cf = AstSize;
        let extractor = RandomExtractor::new(&egraph, cf);
        let shorter = extractor.extract_shorter(id, build_expr).unwrap();
        Box::new(vec![format!("{}", shorter)].into_iter())
    } else {
        let t = egraph[id]
            .nodes
            .clone()
            .into_iter()
            .map(move |l| {
                let n = depth - 1;
                let eg = egraph.clone();
                let lc = l.clone();

                let iter: Box<dyn Iterator<Item = String>> = match lc {
                    Lang::I32Add([left, right]) => {
                        let lcope = left.clone();
                        let rcopy = right.clone();
                        // zip the expansion of the operands
                        let t = lazy_expand(lcope, eg, n)
                            .flat_map(move |e| std::iter::repeat(e).zip(lazy_expand(rcopy, eg, n)))
                            .map(|(l, r)| format!("(i32.add {} {})", l, r));

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
        let mut it = lazy_expand(root, &egraph, 2);
        let elapsed = enumeration_start.elapsed();
        eprintln!(
            "Construction {}.{:03} seconds",
            elapsed.as_secs(),
            elapsed.subsec_millis()
        );

        for i in 0..100 {
            let enumeration_start = std::time::Instant::now();
            if let Some(it) = it.next() {
                println!("{} {}", i, it);
                let elapsed = enumeration_start.elapsed();
                eprintln!(
                    "Done enumerating in {}.{:03} seconds",
                    elapsed.as_secs(),
                    elapsed.subsec_millis()
                );
            } else {
                break;
            }
        }

        todo!();
    }
}
