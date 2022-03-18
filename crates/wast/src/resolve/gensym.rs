use crate::ast::{Id, Span};

pub(crate) struct Gensym {
    next: u32,
}

impl Gensym {
    pub fn new() -> Self {
        Self { next: 0 }
    }

    pub fn gen(&mut self, span: Span) -> Id<'static> {
        let gen = self.next + 1;
        self.next = gen;
        Id::gensym(span, gen)
    }

    pub fn fill<'a>(&mut self, span: Span, slot: &mut Option<Id<'a>>) -> Id<'a> {
        *slot.get_or_insert_with(|| self.gen(span))
    }
}
