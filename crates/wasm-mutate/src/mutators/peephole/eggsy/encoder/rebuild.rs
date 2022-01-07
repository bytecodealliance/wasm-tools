//! Function to construct expressions
use std::collections::HashMap;

use crate::mutators::peephole::eggsy::encoder::TraversalEvent;
use crate::mutators::peephole::Lang;
use egg::{Id, Language, RecExpr};

/// Build RecExpr from tree information
pub fn build_expr<'a>(root: Id, id_to_lang: impl Fn(Id) -> &'a Lang) -> RecExpr<Lang> {
    let mut expr = RecExpr::default();
    build_expr_inner(root, &mut expr, |id| {
        let lang = id_to_lang(id);
        (lang, lang.children())
    });
    expr
}

pub(crate) fn build_expr_inner<'a>(
    root: Id,
    expr: &mut RecExpr<Lang>,
    id_to_lang: impl Fn(Id) -> (&'a Lang, &'a [Id]),
) -> Id {
    // A map from the `Id`s we assigned to each sub-expression when extracting a
    // random expression to the `Id`s assigned to each sub-expression by the
    // `RecExpr`.
    let mut node_to_id: HashMap<Id, Id> = Default::default();

    let mut to_visit = vec![(TraversalEvent::Exit, root), (TraversalEvent::Enter, root)];
    while let Some((event, node)) = to_visit.pop() {
        match event {
            TraversalEvent::Enter => {
                let start_children = to_visit.len();

                let (_lang, children) = id_to_lang(node);
                for child in children.iter().copied() {
                    to_visit.push((TraversalEvent::Enter, child));
                    to_visit.push((TraversalEvent::Exit, child));
                }

                // Reverse to make it so that we visit children in order
                // (e.g. operands are visited in order).
                to_visit[start_children..].reverse();
            }
            TraversalEvent::Exit => {
                let (term, operands) = id_to_lang(node);
                let mut new_term = term.clone();
                for (child, operand) in new_term.children_mut().iter_mut().zip(operands) {
                    *child = node_to_id[operand];
                }
                let sub_expr_id = expr.add(new_term);

                // Copy the id to stack entries to a new one
                let old_entry = node_to_id.insert(node, sub_expr_id);
                assert!(old_entry.is_none());
            }
        }
    }
    Id::from(expr.as_ref().len() - 1)
}
