use crate::token::{Id, Span};
use std::cell::Cell;

thread_local!(static NEXT: Cell<u32> = Cell::new(0));

pub fn reset() {
    NEXT.with(|c| c.set(0));
}

pub fn generate(span: Span) -> Id<'static> {
    NEXT.with(|next| {
        let generation = next.get() + 1;
        next.set(generation);
        Id::gensym(span, generation)
    })
}

pub fn fill<'a>(span: Span, slot: &mut Option<Id<'a>>) -> Id<'a> {
    *slot.get_or_insert_with(|| generate(span))
}
