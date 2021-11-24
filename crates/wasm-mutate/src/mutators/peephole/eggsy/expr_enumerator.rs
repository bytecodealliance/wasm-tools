use std::{cell::RefCell, rc::Rc};

use crate::mutators::peephole::{
    eggsy::{encoder::rebuild::build_expr_inner, RandomExtractor},
    EG,
};
use egg::{AstSize, Id, RecExpr};
use rand::{prelude::SmallRng, Rng, SeedableRng};

use super::lang::Lang;

macro_rules! binop {
    ($lang:ident, $left:ident, $right: ident, $egraph: ident, $rnd: ident, $depth: ident, $recexpr: ident) => {{
        let lcope = $left;
        let rcopy = $right;
        let rec = $recexpr.clone();
        let rec2 = $recexpr.clone();
        let rec3 = $recexpr.clone();

        let rnd1 = $rnd.clone();
        let rnd2 = $rnd.clone();
        // Cartesian product of the operands
        let t = lazy_expand(lcope, $egraph.clone(), $depth, rnd1.clone(), rec3)
            .flat_map(move |e| {
                std::iter::repeat(e).zip(lazy_expand(
                    rcopy,
                    $egraph.clone(),
                    $depth,
                    rnd2.clone(),
                    rec2.clone(),
                ))
            })
            .map(move |(l, r)| rec.borrow_mut().add(Lang::$lang([l, r])));

        Box::new(t)
    }};
}

macro_rules! unop {
    ($lang:ident,$arg: ident, $egraph: ident, $rnd: ident, $depth: ident, $recexpr: ident) => {{
        let lcope = $arg;
        let rec = $recexpr.clone();
        let rnd1 = $rnd.clone();
        let t = lazy_expand(lcope, $egraph, $depth, rnd1, $recexpr.clone())
            .map(move |l| rec.clone().borrow_mut().add(Lang::$lang([l])));

        Box::new(t)
    }};
}

macro_rules! store {
    ($lang:ident, $left:ident, $right: ident, $egraph: ident, $rnd: ident, $depth: ident, $recexpr: ident, $align: ident, $offset: ident, $mem: ident) => {{
        let lcope = $left;
        let rcopy = $right;
        let rec = $recexpr.clone();
        let rec2 = $recexpr.clone();
        let rec3 = $recexpr.clone();

        let rnd1 = $rnd.clone();
        let rnd2 = $rnd.clone();
        let t = lazy_expand(lcope, $egraph.clone(), $depth, rnd1, rec)
            .flat_map(move |e| {
                std::iter::repeat(e).zip(lazy_expand(
                    rcopy,
                    $egraph.clone(),
                    $depth,
                    rnd2.clone(),
                    rec2.clone(),
                ))
            })
            .map(move |(l, r)| {
                rec3.clone().borrow_mut().add(Lang::$lang {
                    value_and_offset: [l, r],
                    align: $align,
                    mem: $mem,
                    static_offset: $offset,
                })
            });

        Box::new(t)
    }};
}

macro_rules! load {
    ($lang:ident, $arg: ident, $egraph: ident, $rnd: ident, $depth: ident, $recexpr: ident, $align: ident, $offset: ident, $mem: ident) => {{
        let lcope = $arg;
        let rec = $recexpr.clone();
        let rnd1 = $rnd.clone();
        let t = lazy_expand(lcope, $egraph, $depth, rnd1, $recexpr.clone()).map(move |l| {
            rec.clone().borrow_mut().add(Lang::$lang {
                offset: l,
                static_offset: $offset,
                align: $align,
                mem: $mem,
            })
        });

        Box::new(t)
    }};
}

macro_rules! local_or_global {
    ($lang:ident, $idx: ident, $arg: ident, $egraph: ident, $rnd: ident, $depth: ident, $recexpr: ident) => {{
        let lcope = $arg;
        let rec = $recexpr.clone();
        let rnd1 = $rnd.clone();
        let t = lazy_expand(lcope, $egraph, $depth, rnd1.clone(), $recexpr.clone())
            .map(move |l| rec.clone().borrow_mut().add(Lang::$lang($idx, l)));

        Box::new(t)
    }};
}

/// Returns a lazy iterator over all possible expressions that can be
/// constructed starting in eclass *Id*. If the eclass *Id* is the root of the
/// expression to mutate, this function will return a random but semantically
/// equivalent (based on the rewriting rules) expression.
///
/// Starting by the *Id* eclass in the egraph it selects a random node.
/// Depending on the node, the cartesian product of the operands iterators
/// (lazy_expand calls) is built to provide a random expression.
///
/// Lets assume the language formed only by the `add` and `100` nodes, with
/// the rewriting rule `?x => add ?x 0`. And lets assume also that we want to
/// return a random expression out of the parsing of the former integer
/// expression `100`. When the egraph is constructed, the expression `100` is in
/// the same eclass than `add 100 0`.
/// But, since we defined the rewriting rule for any node, recursivelly, all
/// nodes are in the same equivalence class.
///
/// If we call `lazy_expand` to randomly construct an expression out of the
/// egraph starting at equivalence class `i`, a random node is first selected
/// from  the selected starting eclass.
/// For this case, lets assume the `add i 0` node is selected. The iterator will then build
/// the lazy iterator upon this, something like `add(lazy_expand(i),
/// lazy_expand(0)) => add(add(lazy_expand(i), 0), 0)`, and so on until the max
/// depth  is reached and the smallest expression that can be constructed for i
/// is filled up. Every time the `next` method is called a new expression is
/// returned until no more are expressions are available due to the *depth* argument.
///
/// Notice that if new nodes are added to the Lang enum, the corresponding random
/// construction of equivalent expressions needs to be added to this method.
pub fn lazy_expand<'a>(
    id: Id,
    egraph: Rc<EG>,
    depth: u32,
    rnd: Rc<RefCell<SmallRng>>,
    recexpr: Rc<RefCell<RecExpr<Lang>>>,
) -> impl Iterator<Item = Id> + 'a {
    let t: Box<dyn Iterator<Item = Id>> = if depth == 0 {
        let cf = AstSize;
        let extractor = RandomExtractor::new(&egraph, cf);
        let shorter = extractor
            .extract_smallest(id, &recexpr, build_expr_inner)
            .unwrap();

        Box::new(vec![shorter].into_iter())
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

                let iter: Box<dyn Iterator<Item = Id>> = match lc {
                    // ($lang:ident, $left:ident, $right: ident, $egraph: ident, $rnd: ident, $depth: ident, $recexpr: ident, &align: ident, &offset: ident, &mem: ident)
                    Lang::I32Store {
                        value_and_offset: [left, right],
                        mem,
                        static_offset,
                        align,
                    } => store!(
                        I32Store,
                        left,
                        right,
                        eg,
                        rnd,
                        n,
                        recexpr,
                        align,
                        static_offset,
                        mem
                    ),
                    Lang::I64Store {
                        value_and_offset: [left, right],
                        mem,
                        static_offset,
                        align,
                    } => store!(
                        I64Store,
                        left,
                        right,
                        eg,
                        rnd,
                        n,
                        recexpr,
                        align,
                        static_offset,
                        mem
                    ),
                    Lang::F32Store {
                        value_and_offset: [left, right],
                        mem,
                        static_offset,
                        align,
                    } => store!(
                        F32Store,
                        left,
                        right,
                        eg,
                        rnd,
                        n,
                        recexpr,
                        align,
                        static_offset,
                        mem
                    ),
                    Lang::F64Store {
                        value_and_offset: [left, right],
                        mem,
                        static_offset,
                        align,
                    } => store!(
                        F64Store,
                        left,
                        right,
                        eg,
                        rnd,
                        n,
                        recexpr,
                        align,
                        static_offset,
                        mem
                    ),
                    Lang::I32Store8 {
                        value_and_offset: [left, right],
                        mem,
                        static_offset,
                        align,
                    } => store!(
                        I32Store8,
                        left,
                        right,
                        eg,
                        rnd,
                        n,
                        recexpr,
                        align,
                        static_offset,
                        mem
                    ),
                    Lang::I32Store16 {
                        value_and_offset: [left, right],
                        mem,
                        static_offset,
                        align,
                    } => store!(
                        I32Store16,
                        left,
                        right,
                        eg,
                        rnd,
                        n,
                        recexpr,
                        align,
                        static_offset,
                        mem
                    ),
                    Lang::I64Store8 {
                        value_and_offset: [left, right],
                        mem,
                        static_offset,
                        align,
                    } => store!(
                        I64Store8,
                        left,
                        right,
                        eg,
                        rnd,
                        n,
                        recexpr,
                        align,
                        static_offset,
                        mem
                    ),
                    Lang::I64Store16 {
                        value_and_offset: [left, right],
                        mem,
                        static_offset,
                        align,
                    } => store!(
                        I64Store16,
                        left,
                        right,
                        eg,
                        rnd,
                        n,
                        recexpr,
                        align,
                        static_offset,
                        mem
                    ),
                    Lang::I64Store32 {
                        value_and_offset: [left, right],
                        mem,
                        static_offset,
                        align,
                    } => store!(
                        I64Store32,
                        left,
                        right,
                        eg,
                        rnd,
                        n,
                        recexpr,
                        align,
                        static_offset,
                        mem
                    ),
                    Lang::I32Add([left, right]) => binop!(I32Add, left, right, eg, rnd, n, recexpr),
                    Lang::I64Add([left, right]) => binop!(I64Add, left, right, eg, rnd, n, recexpr),
                    Lang::I32Sub([left, right]) => binop!(I32Sub, left, right, eg, rnd, n, recexpr),
                    Lang::I64Sub([left, right]) => binop!(I64Sub, left, right, eg, rnd, n, recexpr),
                    Lang::I32Mul([left, right]) => binop!(I32Mul, left, right, eg, rnd, n, recexpr),
                    Lang::I64Mul([left, right]) => binop!(I64Mul, left, right, eg, rnd, n, recexpr),
                    Lang::I32And([left, right]) => binop!(I32And, left, right, eg, rnd, n, recexpr),
                    Lang::I64And([left, right]) => binop!(I64And, left, right, eg, rnd, n, recexpr),
                    Lang::I32Or([left, right]) => binop!(I32Or, left, right, eg, rnd, n, recexpr),
                    Lang::I64Or([left, right]) => binop!(I64Or, left, right, eg, rnd, n, recexpr),
                    Lang::I32Xor([left, right]) => binop!(I32Xor, left, right, eg, rnd, n, recexpr),
                    Lang::I64Xor([left, right]) => binop!(I64Xor, left, right, eg, rnd, n, recexpr),
                    Lang::I32Shl([left, right]) => binop!(I32Shl, left, right, eg, rnd, n, recexpr),
                    Lang::I64Shl([left, right]) => binop!(I64Shl, left, right, eg, rnd, n, recexpr),
                    Lang::I32ShrU([left, right]) => {
                        binop!(I32ShrU, left, right, eg, rnd, n, recexpr)
                    }
                    Lang::I64ShrU([left, right]) => {
                        binop!(I64ShrU, left, right, eg, rnd, n, recexpr)
                    }
                    Lang::I32DivU([left, right]) => {
                        binop!(I32DivU, left, right, eg, rnd, n, recexpr)
                    }
                    Lang::I64DivU([left, right]) => {
                        binop!(I64DivU, left, right, eg, rnd, n, recexpr)
                    }
                    Lang::I32DivS([left, right]) => {
                        binop!(I32DivS, left, right, eg, rnd, n, recexpr)
                    }
                    Lang::I64DivS([left, right]) => {
                        binop!(I64DivS, left, right, eg, rnd, n, recexpr)
                    }
                    Lang::I32ShrS([left, right]) => {
                        binop!(I32ShrS, left, right, eg, rnd, n, recexpr)
                    }
                    Lang::I64ShrS([left, right]) => {
                        binop!(I64ShrS, left, right, eg, rnd, n, recexpr)
                    }
                    Lang::I32RotR([left, right]) => {
                        binop!(I32RotR, left, right, eg, rnd, n, recexpr)
                    }
                    Lang::I64RotR([left, right]) => {
                        binop!(I64RotR, left, right, eg, rnd, n, recexpr)
                    }
                    Lang::I32RotL([left, right]) => {
                        binop!(I32RotL, left, right, eg, rnd, n, recexpr)
                    }
                    Lang::I64RotL([left, right]) => {
                        binop!(I64RotL, left, right, eg, rnd, n, recexpr)
                    }
                    Lang::I32RemS([left, right]) => {
                        binop!(I32RemS, left, right, eg, rnd, n, recexpr)
                    }
                    Lang::I64RemS([left, right]) => {
                        binop!(I64RemS, left, right, eg, rnd, n, recexpr)
                    }
                    Lang::I32RemU([left, right]) => {
                        binop!(I32RemU, left, right, eg, rnd, n, recexpr)
                    }
                    Lang::I64RemU([left, right]) => {
                        binop!(I64RemU, left, right, eg, rnd, n, recexpr)
                    }
                    Lang::I32Eq([left, right]) => binop!(I32Eq, left, right, eg, rnd, n, recexpr),
                    Lang::I64Eq([left, right]) => binop!(I64Eq, left, right, eg, rnd, n, recexpr),
                    Lang::I32Ne([left, right]) => binop!(I32Ne, left, right, eg, rnd, n, recexpr),
                    Lang::I64Ne([left, right]) => binop!(I64Ne, left, right, eg, rnd, n, recexpr),
                    Lang::I32LtS([left, right]) => binop!(I32LtS, left, right, eg, rnd, n, recexpr),
                    Lang::I64LtS([left, right]) => binop!(I64LtS, left, right, eg, rnd, n, recexpr),
                    Lang::I32LtU([left, right]) => binop!(I32LtU, left, right, eg, rnd, n, recexpr),
                    Lang::I64LtU([left, right]) => binop!(I64LtU, left, right, eg, rnd, n, recexpr),
                    Lang::I32GtS([left, right]) => binop!(I32GtS, left, right, eg, rnd, n, recexpr),
                    Lang::I64GtS([left, right]) => binop!(I64GtS, left, right, eg, rnd, n, recexpr),
                    Lang::I32GtU([left, right]) => binop!(I32GtU, left, right, eg, rnd, n, recexpr),
                    Lang::I64GtU([left, right]) => binop!(I64GtU, left, right, eg, rnd, n, recexpr),
                    Lang::I32LeS([left, right]) => binop!(I32LeS, left, right, eg, rnd, n, recexpr),
                    Lang::I64LeS([left, right]) => binop!(I64LeS, left, right, eg, rnd, n, recexpr),
                    Lang::I32LeU([left, right]) => binop!(I32LeU, left, right, eg, rnd, n, recexpr),
                    Lang::I64LeU([left, right]) => binop!(I64LeU, left, right, eg, rnd, n, recexpr),
                    Lang::I32GeS([left, right]) => binop!(I32GeS, left, right, eg, rnd, n, recexpr),
                    Lang::I64GeS([left, right]) => binop!(I64GeS, left, right, eg, rnd, n, recexpr),
                    Lang::I32GeU([left, right]) => binop!(I32GeU, left, right, eg, rnd, n, recexpr),
                    Lang::I64GeU([left, right]) => binop!(I64GeU, left, right, eg, rnd, n, recexpr),
                    Lang::F32Add([left, right]) => binop!(F32Add, left, right, eg, rnd, n, recexpr),
                    Lang::F64Add([left, right]) => binop!(F64Add, left, right, eg, rnd, n, recexpr),
                    Lang::F32Sub([left, right]) => binop!(F32Sub, left, right, eg, rnd, n, recexpr),
                    Lang::F64Sub([left, right]) => binop!(F64Sub, left, right, eg, rnd, n, recexpr),
                    Lang::F32Mul([left, right]) => binop!(F32Mul, left, right, eg, rnd, n, recexpr),
                    Lang::F64Mul([left, right]) => binop!(F64Mul, left, right, eg, rnd, n, recexpr),
                    Lang::F32Div([left, right]) => binop!(F32Div, left, right, eg, rnd, n, recexpr),
                    Lang::F64Div([left, right]) => binop!(F64Div, left, right, eg, rnd, n, recexpr),
                    Lang::F32Min([left, right]) => binop!(F32Min, left, right, eg, rnd, n, recexpr),
                    Lang::F64Min([left, right]) => binop!(F64Min, left, right, eg, rnd, n, recexpr),
                    Lang::F32Max([left, right]) => binop!(F32Max, left, right, eg, rnd, n, recexpr),
                    Lang::F64Max([left, right]) => binop!(F64Max, left, right, eg, rnd, n, recexpr),
                    Lang::F32Copysign([left, right]) => {
                        binop!(F32Copysign, left, right, eg, rnd, n, recexpr)
                    }
                    Lang::F64Copysign([left, right]) => {
                        binop!(F64Copysign, left, right, eg, rnd, n, recexpr)
                    }
                    Lang::F32Eq([left, right]) => binop!(F32Eq, left, right, eg, rnd, n, recexpr),
                    Lang::F64Eq([left, right]) => binop!(F64Eq, left, right, eg, rnd, n, recexpr),
                    Lang::F32Ne([left, right]) => binop!(F32Ne, left, right, eg, rnd, n, recexpr),
                    Lang::F64Ne([left, right]) => binop!(F64Ne, left, right, eg, rnd, n, recexpr),
                    Lang::F32Lt([left, right]) => binop!(F32Lt, left, right, eg, rnd, n, recexpr),
                    Lang::F64Lt([left, right]) => binop!(F64Lt, left, right, eg, rnd, n, recexpr),
                    Lang::F32Gt([left, right]) => binop!(F32Gt, left, right, eg, rnd, n, recexpr),
                    Lang::F64Gt([left, right]) => binop!(F64Gt, left, right, eg, rnd, n, recexpr),
                    Lang::F32Le([left, right]) => binop!(F32Le, left, right, eg, rnd, n, recexpr),
                    Lang::F64Le([left, right]) => binop!(F64Le, left, right, eg, rnd, n, recexpr),
                    Lang::F32Ge([left, right]) => binop!(F32Ge, left, right, eg, rnd, n, recexpr),
                    Lang::F64Ge([left, right]) => binop!(F64Ge, left, right, eg, rnd, n, recexpr),

                    Lang::LocalSet(idx, arg) => {
                        local_or_global!(LocalSet, idx, arg, eg, rnd, n, recexpr)
                    }
                    Lang::LocalTee(idx, arg) => {
                        local_or_global!(LocalTee, idx, arg, eg, rnd, n, recexpr)
                    }
                    Lang::GlobalSet(idx, arg) => {
                        local_or_global!(GlobalSet, idx, arg, eg, rnd, n, recexpr)
                    }
                    Lang::Call(idx, arguments) => {
                        // FIXME
                        // I could not find a way to have a cartesian product of dynamic size collection of Iterators
                        // This can also be solved if we turn Call and Container enodes to be binary trees where
                        // the left operand is a real Id and the second is a container, and so on until all
                        // arguments are expressed in the binary tree
                        // For example
                        // (call.$1 a b c d) can be turned into (call.$1 a (container b (container c (container d)))))
                        let mut operands = vec![];
                        for a in &arguments {
                            let na = lazy_expand(*a, eg.clone(), n, rnd.clone(), recexpr.clone())
                                .next()
                                .unwrap();
                            operands.push(na);
                        }

                        let t =
                            std::iter::once(recexpr.borrow_mut().add(Lang::Call(idx, operands)));
                        Box::new(t)
                    }
                    Lang::Container(arguments) => {
                        // FIXME
                        // Same as Call
                        let mut operands = vec![];
                        for a in &arguments {
                            let na = lazy_expand(*a, eg.clone(), n, rnd.clone(), recexpr.clone())
                                .next()
                                .unwrap();
                            operands.push(na);
                        }

                        let t =
                            std::iter::once(recexpr.borrow_mut().add(Lang::Container(operands)));
                        Box::new(t)
                    }
                    Lang::I64Load32U {
                        offset: arg,
                        align,
                        static_offset,
                        mem,
                    } => load!(
                        I64Load32U,
                        arg,
                        eg,
                        rnd,
                        n,
                        recexpr,
                        align,
                        static_offset,
                        mem
                    ),

                    Lang::F32Load {
                        offset: arg,
                        align,
                        static_offset,
                        mem,
                    } => load!(F32Load, arg, eg, rnd, n, recexpr, align, static_offset, mem),
                    Lang::F64Load {
                        offset: arg,
                        align,
                        static_offset,
                        mem,
                    } => load!(F64Load, arg, eg, rnd, n, recexpr, align, static_offset, mem),
                    Lang::I32Load8S {
                        offset: arg,
                        align,
                        static_offset,
                        mem,
                    } => load!(
                        I32Load8S,
                        arg,
                        eg,
                        rnd,
                        n,
                        recexpr,
                        align,
                        static_offset,
                        mem
                    ),
                    Lang::I32Load8U {
                        offset: arg,
                        align,
                        static_offset,
                        mem,
                    } => load!(
                        I32Load8U,
                        arg,
                        eg,
                        rnd,
                        n,
                        recexpr,
                        align,
                        static_offset,
                        mem
                    ),
                    Lang::I32Load16S {
                        offset: arg,
                        align,
                        static_offset,
                        mem,
                    } => load!(
                        I32Load16S,
                        arg,
                        eg,
                        rnd,
                        n,
                        recexpr,
                        align,
                        static_offset,
                        mem
                    ),
                    Lang::I32Load16U {
                        offset: arg,
                        align,
                        static_offset,
                        mem,
                    } => load!(
                        I32Load16U,
                        arg,
                        eg,
                        rnd,
                        n,
                        recexpr,
                        align,
                        static_offset,
                        mem
                    ),
                    Lang::I64Load8S {
                        offset: arg,
                        align,
                        static_offset,
                        mem,
                    } => load!(
                        I64Load8S,
                        arg,
                        eg,
                        rnd,
                        n,
                        recexpr,
                        align,
                        static_offset,
                        mem
                    ),
                    Lang::I64Load8U {
                        offset: arg,
                        align,
                        static_offset,
                        mem,
                    } => load!(
                        I64Load8U,
                        arg,
                        eg,
                        rnd,
                        n,
                        recexpr,
                        align,
                        static_offset,
                        mem
                    ),
                    Lang::I64Load16S {
                        offset: arg,
                        align,
                        static_offset,
                        mem,
                    } => load!(
                        I64Load16S,
                        arg,
                        eg,
                        rnd,
                        n,
                        recexpr,
                        align,
                        static_offset,
                        mem
                    ),
                    Lang::I64Load16U {
                        offset: arg,
                        align,
                        static_offset,
                        mem,
                    } => load!(
                        I64Load16U,
                        arg,
                        eg,
                        rnd,
                        n,
                        recexpr,
                        align,
                        static_offset,
                        mem
                    ),
                    Lang::I64Load32S {
                        offset: arg,
                        align,
                        static_offset,
                        mem,
                    } => load!(
                        I64Load32S,
                        arg,
                        eg,
                        rnd,
                        n,
                        recexpr,
                        align,
                        static_offset,
                        mem
                    ),
                    Lang::I64Load {
                        offset: arg,
                        align,
                        static_offset,
                        mem,
                    } => load!(I64Load, arg, eg, rnd, n, recexpr, align, static_offset, mem),
                    Lang::I32Load {
                        offset: arg,
                        align,
                        static_offset,
                        mem,
                    } => load!(I32Load, arg, eg, rnd, n, recexpr, align, static_offset, mem),
                    Lang::F32Abs([arg]) => unop!(F32Abs, arg, eg, rnd, n, recexpr),
                    Lang::F64Abs([arg]) => unop!(F64Abs, arg, eg, rnd, n, recexpr),
                    Lang::F32Neg([arg]) => unop!(F32Neg, arg, eg, rnd, n, recexpr),
                    Lang::F64Neg([arg]) => unop!(F64Neg, arg, eg, rnd, n, recexpr),
                    Lang::F32Sqrt([arg]) => unop!(F32Sqrt, arg, eg, rnd, n, recexpr),
                    Lang::F64Sqrt([arg]) => unop!(F64Sqrt, arg, eg, rnd, n, recexpr),
                    Lang::F32Ceil([arg]) => unop!(F32Ceil, arg, eg, rnd, n, recexpr),
                    Lang::F64Ceil([arg]) => unop!(F64Ceil, arg, eg, rnd, n, recexpr),
                    Lang::F32Floor([arg]) => unop!(F32Floor, arg, eg, rnd, n, recexpr),
                    Lang::F64Floor([arg]) => unop!(F64Floor, arg, eg, rnd, n, recexpr),
                    Lang::F32Trunc([arg]) => unop!(F32Trunc, arg, eg, rnd, n, recexpr),
                    Lang::F64trunc([arg]) => unop!(F64trunc, arg, eg, rnd, n, recexpr),
                    Lang::F32Nearest([arg]) => unop!(F32Nearest, arg, eg, rnd, n, recexpr),
                    Lang::F64Nearest([arg]) => unop!(F64Nearest, arg, eg, rnd, n, recexpr),
                    Lang::Wrap([arg]) => unop!(Wrap, arg, eg, rnd, n, recexpr),
                    Lang::I32Extend8S([arg]) => unop!(I32Extend8S, arg, eg, rnd, n, recexpr),
                    Lang::I64Extend8S([arg]) => unop!(I64Extend8S, arg, eg, rnd, n, recexpr),
                    Lang::I32Extend16S([arg]) => unop!(I32Extend16S, arg, eg, rnd, n, recexpr),
                    Lang::I64Extend16S([arg]) => unop!(I64Extend16S, arg, eg, rnd, n, recexpr),
                    Lang::I64Extend32S([arg]) => unop!(I64Extend32S, arg, eg, rnd, n, recexpr),
                    Lang::I64ExtendI32S([arg]) => unop!(I64ExtendI32S, arg, eg, rnd, n, recexpr),
                    Lang::I64ExtendI32U([arg]) => unop!(I64ExtendI32U, arg, eg, rnd, n, recexpr),
                    Lang::I32TruncF32S([arg]) => unop!(I32TruncF32S, arg, eg, rnd, n, recexpr),
                    Lang::I32TruncF32U([arg]) => unop!(I32TruncF32U, arg, eg, rnd, n, recexpr),
                    Lang::I32TruncF64S([arg]) => unop!(I32TruncF64S, arg, eg, rnd, n, recexpr),
                    Lang::I32TruncF64U([arg]) => unop!(I32TruncF64U, arg, eg, rnd, n, recexpr),
                    Lang::I64TruncF32S([arg]) => unop!(I64TruncF32S, arg, eg, rnd, n, recexpr),
                    Lang::I64TruncF32U([arg]) => unop!(I64TruncF32U, arg, eg, rnd, n, recexpr),
                    Lang::I64TruncF64S([arg]) => unop!(I64TruncF64S, arg, eg, rnd, n, recexpr),
                    Lang::I64TruncF64U([arg]) => unop!(I64TruncF64U, arg, eg, rnd, n, recexpr),
                    Lang::F32ConvertI32S([arg]) => unop!(F32ConvertI32S, arg, eg, rnd, n, recexpr),
                    Lang::F32ConvertI32U([arg]) => unop!(F32ConvertI32U, arg, eg, rnd, n, recexpr),
                    Lang::F32ConvertI64S([arg]) => unop!(F32ConvertI64S, arg, eg, rnd, n, recexpr),
                    Lang::F32ConvertI64U([arg]) => unop!(F32ConvertI64U, arg, eg, rnd, n, recexpr),
                    Lang::F32DemoteF64([arg]) => unop!(F32DemoteF64, arg, eg, rnd, n, recexpr),
                    Lang::F64ConvertI32S([arg]) => unop!(F64ConvertI32S, arg, eg, rnd, n, recexpr),
                    Lang::F64ConvertI32U([arg]) => unop!(F64ConvertI32U, arg, eg, rnd, n, recexpr),
                    Lang::F64ConvertI64S([arg]) => unop!(F64ConvertI64S, arg, eg, rnd, n, recexpr),
                    Lang::F64ConvertI64U([arg]) => unop!(F64ConvertI64U, arg, eg, rnd, n, recexpr),
                    Lang::F64PromoteF32([arg]) => unop!(F64PromoteF32, arg, eg, rnd, n, recexpr),
                    Lang::I32ReinterpretF32([arg]) => {
                        unop!(I32ReinterpretF32, arg, eg, rnd, n, recexpr)
                    }
                    Lang::I64ReinterpretF64([arg]) => {
                        unop!(I64ReinterpretF64, arg, eg, rnd, n, recexpr)
                    }
                    Lang::F32ReinterpretI32([arg]) => {
                        unop!(F32ReinterpretI32, arg, eg, rnd, n, recexpr)
                    }
                    Lang::F64ReinterpretI64([arg]) => {
                        unop!(F64ReinterpretI64, arg, eg, rnd, n, recexpr)
                    }
                    Lang::I32TruncSatF32S([arg]) => {
                        unop!(I32TruncSatF32S, arg, eg, rnd, n, recexpr)
                    }
                    Lang::I32TruncSatF32U([arg]) => {
                        unop!(I32TruncSatF32U, arg, eg, rnd, n, recexpr)
                    }
                    Lang::I32TruncSatF64S([arg]) => {
                        unop!(I32TruncSatF64S, arg, eg, rnd, n, recexpr)
                    }
                    Lang::I32TruncSatF64U([arg]) => {
                        unop!(I32TruncSatF64U, arg, eg, rnd, n, recexpr)
                    }
                    Lang::I64TruncSatF32S([arg]) => {
                        unop!(I64TruncSatF32S, arg, eg, rnd, n, recexpr)
                    }
                    Lang::I64TruncSatF32U([arg]) => {
                        unop!(I64TruncSatF32U, arg, eg, rnd, n, recexpr)
                    }
                    Lang::I64TruncSatF64S([arg]) => {
                        unop!(I64TruncSatF64S, arg, eg, rnd, n, recexpr)
                    }
                    Lang::I64TruncSatF64U([arg]) => {
                        unop!(I64TruncSatF64U, arg, eg, rnd, n, recexpr)
                    }
                    Lang::Drop([arg]) => unop!(Drop, arg, eg, rnd, n, recexpr),
                    Lang::I32Eqz([arg]) => unop!(I32Eqz, arg, eg, rnd, n, recexpr),
                    Lang::I64Eqz([arg]) => unop!(I64Eqz, arg, eg, rnd, n, recexpr),
                    Lang::I32Popcnt([arg]) => unop!(I32Popcnt, arg, eg, rnd, n, recexpr),
                    Lang::I64Popcnt([arg]) => unop!(I64Popcnt, arg, eg, rnd, n, recexpr),
                    Lang::I32Clz([arg]) => unop!(I32Clz, arg, eg, rnd, n, recexpr),
                    Lang::I32Ctz([arg]) => unop!(I32Ctz, arg, eg, rnd, n, recexpr),
                    Lang::I64Ctz([arg]) => unop!(I64Ctz, arg, eg, rnd, n, recexpr),
                    Lang::I64Clz([arg]) => unop!(I64Clz, arg, eg, rnd, n, recexpr),
                    Lang::UnfoldI32(arg) => {
                        let lcope = arg;
                        let rec = recexpr.clone();
                        let t = lazy_expand(
                            lcope,
                            eg,
                            0, /* This is a patch to avoid expansion of
                               non statically known values */
                            rnd.clone(),
                            recexpr.clone(),
                        )
                        .map(move |l| rec.clone().borrow_mut().add(Lang::UnfoldI32(l)));

                        Box::new(t)
                    }
                    Lang::UnfoldI64(arg) => {
                        let lcope = arg;
                        let rec = recexpr.clone();
                        let t = lazy_expand(lcope, eg, 0, rnd.clone(), recexpr.clone())
                            .map(move |l| rec.clone().borrow_mut().add(Lang::UnfoldI64(l)));

                        Box::new(t)
                    }
                    Lang::Select(arguments) => {
                        // FIXME
                        // Same as Call
                        let mut operands = vec![];
                        let rec = recexpr.clone();
                        for a in &arguments {
                            let na = lazy_expand(*a, eg.clone(), n, rnd.clone(), recexpr.clone())
                                .next()
                                .unwrap();
                            operands.push(na);
                        }

                        let t = std::iter::once(recexpr.borrow_mut().add(Lang::Select([
                            operands[0],
                            operands[1],
                            operands[2],
                        ])));

                        Box::new(t)
                    }
                    Lang::MemoryGrow { mem, mem_byte, by } => {
                        let lcope = by;
                        let rec = recexpr.clone();
                        let t =
                            lazy_expand(lcope, eg, n, rnd.clone(), recexpr.clone()).map(move |l| {
                                rec.clone().borrow_mut().add(Lang::MemoryGrow {
                                    mem,
                                    mem_byte,
                                    by: l,
                                })
                            });

                        Box::new(t)
                    }
                    i @ Lang::I32(_) => {
                        Box::new(vec![recexpr.borrow_mut().add(i.clone())].into_iter())
                    }

                    i @ Lang::I64(_) => {
                        Box::new(vec![recexpr.borrow_mut().add(i.clone())].into_iter())
                    }
                    i @ Lang::F32(_) => {
                        Box::new(vec![recexpr.borrow_mut().add(i.clone())].into_iter())
                    }
                    i @ Lang::F64(_) => {
                        Box::new(vec![recexpr.borrow_mut().add(i.clone())].into_iter())
                    }
                    l @ Lang::LocalGet(_) => {
                        Box::new(vec![recexpr.borrow_mut().add(l.clone())].into_iter())
                    }
                    g @ Lang::GlobalGet(_) => {
                        Box::new(vec![recexpr.borrow_mut().add(g.clone())].into_iter())
                    }
                    u @ Lang::Undef => {
                        Box::new(vec![recexpr.borrow_mut().add(u.clone())].into_iter())
                    }
                    n @ Lang::Nop => {
                        Box::new(vec![recexpr.borrow_mut().add(n.clone())].into_iter())
                    }
                    r32 @ Lang::RandI32 => {
                        Box::new(vec![recexpr.borrow_mut().add(r32.clone())].into_iter())
                    }
                    r64 @ Lang::RandI64 => {
                        Box::new(vec![recexpr.borrow_mut().add(r64.clone())].into_iter())
                    }
                    ms @ Lang::MemorySize { .. } => {
                        Box::new(vec![recexpr.borrow_mut().add(ms.clone())].into_iter())
                    }
                };
                iter
            })
            .flatten();
        Box::new(t)
    };
    t
}

pub fn lazy_expand_aux<'a>(
    id: Id,
    egraph: EG,
    depth: u32,
    seed: u64,
) -> Box<dyn Iterator<Item = RecExpr<Lang>> + 'a> {
    let expr_buffer = RecExpr::default();
    let recexpr = Rc::new(RefCell::new(expr_buffer));
    // FIXME,
    let mut r = SmallRng::seed_from_u64(seed);
    let refrnd = Rc::new(RefCell::new(r));
    let recexprcp = recexpr.clone();
    let recexprcp2 = recexpr.clone();
    let eg = Rc::new(egraph);
    let it = lazy_expand(id, eg.clone(), depth, refrnd, recexprcp).map(move |id| {
        let expr = RecExpr::from(recexprcp2.borrow().as_ref().to_vec());
        expr
    });

    return Box::new(it);
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
        let mut rnd = SmallRng::seed_from_u64(0);
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
