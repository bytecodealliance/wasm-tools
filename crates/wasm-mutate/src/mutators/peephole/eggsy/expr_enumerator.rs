//! Lazy expansion of the egraph
//!
//! This modules provides the helpers and logic to lazyly iterate through the
//! egraph, providing potentially infinite random expressions from an initial egraph

use super::lang::Lang;
use crate::mutators::peephole::{
    eggsy::{encoder::rebuild::build_expr_inner, RandomExtractor},
    EG,
};
use egg::{AstSize, Id, Language, RecExpr};
use rand::{prelude::SmallRng, Rng, SeedableRng};
use std::{cell::RefCell, rc::Rc};

/// Returns a lazy iterator over all possible expressions that can be
/// constructed starting in eclass *Id*. If the eclass *Id* is the root of the
/// expression to mutate, this function will return a random but semantically
/// equivalent (based on the rewriting rules) expression.
///
/// Starting by the *Id* eclass in the egraph it selects a random node.
/// Depending on the node, the cartesian product of the operands iterators
/// (lazy_expand calls) is built to provide a random expression.
///
/// Let's assume the language formed only by the `add` and `100` nodes, with the
/// rewriting rule `?x => add ?x 0`. And let's assume also that we want to
/// return a random expression out of the parsing of the former integer
/// expression `100`. When the egraph is constructed, the expression `100` is in
/// the same eclass than `add 100 0`.  But, since we defined the rewriting rule
/// for any node, recursivelly, all nodes are in the same equivalence class.
///
/// If we call `lazy_expand` to randomly construct an expression out of the
/// egraph starting at equivalence class `i`, a random node is first selected
/// from  the selected starting eclass.
///
/// For this case, let's assume the `add i 0` node is selected. The iterator
/// will then build the lazy iterator upon this, something like
/// `add(lazy_expand(i), lazy_expand(0)) => add(add(lazy_expand(i), 0), 0)`, and
/// so on until the max depth is reached and the smallest expression that can be
/// constructed for i is filled up. Every time the `next` method is called a new
/// expression is returned until no more are expressions are available due to
/// the *depth* argument.
///
/// Notice that if new nodes are added to the Lang enum, the corresponding random
/// construction of equivalent expressions needs to be added to this method.
pub fn lazy_expand<'a>(
    id: Id,
    egraph: Rc<EG>,
    depth: u32,
    rnd: Rc<RefCell<SmallRng>>,
    recexpr: Rc<RefCell<RecExpr<Lang>>>,
) -> Box<dyn Iterator<Item = Id> + 'a> {
    if depth == 0 {
        let cf = AstSize;
        let extractor = RandomExtractor::new(&egraph, cf);
        let shorter = extractor
            .extract_smallest(id, &recexpr, |a, b, c| build_expr_inner(a, b, c))
            .unwrap();

        return Box::new(vec![shorter].into_iter());
    }

    let nodes = egraph[id].nodes.clone();
    let count = nodes.len();
    // For each eclass, at least one node exists
    let split_at = rnd.borrow_mut().gen_range(0, count);
    let indices = (split_at..count).into_iter().chain(0..split_at);

    let t = indices
        .map(move |i| nodes[i].clone())
        .map(move |mut l| {
            let depth = match l {
                // This is a patch to avoid expansion of non-statically known
                // values.
                Lang::UnfoldI32(_) | Lang::UnfoldI64(_) => 0,
                _ => depth - 1,
            };
            let children = l.children_mut();
            let rec = recexpr.clone();
            let iter: Box<dyn Iterator<Item = Id>> = match children.len() {
                // Without any child nodes we can simply add this `Lang` value
                // and move on.
                0 => Box::new(std::iter::once(recexpr.borrow_mut().add(l))),

                // With one child node we try to update the current node with
                // all possible expansions of the child node.
                1 => Box::new(
                    lazy_expand(
                        children[0],
                        egraph.clone(),
                        depth,
                        rnd.clone(),
                        recexpr.clone(),
                    )
                    .map(move |id| {
                        let mut l = l.clone();
                        l.children_mut()[0] = id;
                        rec.clone().borrow_mut().add(l)
                    }),
                ),

                // With two child nodes a cartesian product of expansions is
                // attempted.
                2 => {
                    let rec = recexpr.clone();
                    let egraph = egraph.clone();
                    let rnd = rnd.clone();
                    let recexpr = recexpr.clone();
                    let left = children[0];
                    let right = children[1];
                    Box::new(
                        lazy_expand(left, egraph.clone(), depth, rnd.clone(), recexpr.clone())
                            .flat_map(move |e| {
                                std::iter::repeat(e).zip(lazy_expand(
                                    right,
                                    egraph.clone(),
                                    depth,
                                    rnd.clone(),
                                    recexpr.clone(),
                                ))
                            })
                            .map(move |(left, right)| {
                                let mut l = l.clone();
                                l.children_mut()[0] = left;
                                l.children_mut()[1] = right;
                                rec.borrow_mut().add(l)
                            }),
                    )
                }

                // FIXME I could not find a way to have a cartesian product of
                // dynamic size collection of Iterators This can also be solved
                // if we turn Call and Container enodes to be binary trees where
                // the left operand is a real Id and the second is a container,
                // and so on until all arguments are expressed in the binary
                // tree For example
                // (call.$1 a b c d) can be turned into (call.$1 a (container b (container c (container d)))))
                _ => {
                    for child in children {
                        *child = lazy_expand(
                            *child,
                            egraph.clone(),
                            depth,
                            rnd.clone(),
                            recexpr.clone(),
                        )
                        .next()
                        .unwrap();
                    }
                    Box::new(std::iter::once(recexpr.borrow_mut().add(l)))
                }
            };
            iter
        })
        .flatten();
    Box::new(t)
}

/// Lazy expand helper
pub fn lazy_expand_aux<'a>(
    id: Id,
    egraph: EG,
    depth: u32,
    seed: u64,
) -> Box<dyn Iterator<Item = RecExpr<Lang>> + 'a> {
    let expr_buffer = RecExpr::default();
    let recexpr = Rc::new(RefCell::new(expr_buffer));
    // FIXME,
    let r = SmallRng::seed_from_u64(seed);
    let refrnd = Rc::new(RefCell::new(r));
    let recexprcp = recexpr.clone();
    let recexprcp2 = recexpr;
    let eg = Rc::new(egraph);
    let it = lazy_expand(id, eg, depth, refrnd, recexprcp).map(move |_id| {
        let expr = RecExpr::from(recexprcp2.borrow().as_ref().to_vec());
        expr
    });

    Box::new(it)
}

#[cfg(test)]
mod tests {
    use std::{cell::RefCell, collections::HashMap, rc::Rc};

    use egg::{rewrite, AstSize, RecExpr, Rewrite, Runner};
    use rand::{prelude::SmallRng, SeedableRng};

    use crate::mutators::peephole::eggsy::{
        analysis::PeepholeMutationAnalysis, expr_enumerator::lazy_expand, lang::Lang,
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
        let o1 = vec![Mimi { id: 1 }, Mimi { id: 2 }].into_iter();
        let o2 = vec![Mimi { id: 2 }, Mimi { id: 3 }].into_iter();

        let mut l = o1
            .flat_map(|e| std::iter::repeat(e).zip(o2.clone()))
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
            rewrite!("rule2";  "0_i32" => "(i32.sub 500_i32 500_i32)"),
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
        let rnd = SmallRng::seed_from_u64(0);
        let recexpr = RecExpr::default();
        let rnd = Rc::new(RefCell::new(rnd));
        let recexpr = Rc::new(RefCell::new(recexpr));
        let r = recexpr.clone();
        let mut it = lazy_expand(root, Rc::new(egraph), 10, rnd, recexpr);
        let mut h: HashMap<String, usize> = HashMap::new();
        for _ in 0..100000 {
            if let Some(_) = it.next() {
                let t = format!("{}", r.borrow());

                assert!(!h.contains_key(&t));

                h.insert(t, 1);
            } else {
                break;
            }
        }
    }
}
