use crate::ast::{Id, Span};
use spin::Mutex;

static NEXT: Mutex<u32> = Mutex::new(0);

pub fn reset() {
    let mut next = NEXT.lock();
    *next = 0;
}

pub fn gen(span: Span) -> Id<'static> {
    let mut next = NEXT.lock();
    *next = *next + 1;
    Id::gensym(span, *next)
}

pub fn fill<'a>(span: Span, slot: &mut Option<Id<'a>>) -> Id<'a> {
    *slot.get_or_insert_with(|| gen(span))
}
