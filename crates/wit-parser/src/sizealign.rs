use crate::{FlagsRepr, Int, Interface, Type, TypeDef, TypeDefKind};

#[derive(Default)]
pub struct SizeAlign {
    map: Vec<(usize, usize)>,
}

impl SizeAlign {
    pub fn fill(&mut self, iface: &Interface) {
        self.map = vec![(0, 0); iface.types.len()];
        for ty in iface.topological_types() {
            let pair = self.calculate(&iface.types[ty]);
            self.map[ty.index()] = pair;
        }
    }

    fn calculate(&self, ty: &TypeDef) -> (usize, usize) {
        match &ty.kind {
            TypeDefKind::Type(t) => (self.size(t), self.align(t)),
            TypeDefKind::List(_) => (8, 4),
            TypeDefKind::Record(r) => self.record(r.fields.iter().map(|f| &f.ty)),
            TypeDefKind::Tuple(t) => self.record(t.types.iter()),
            TypeDefKind::Flags(f) => match f.repr() {
                FlagsRepr::U8 => (1, 1),
                FlagsRepr::U16 => (2, 2),
                FlagsRepr::U32(n) => (n * 4, 4),
            },
            TypeDefKind::Variant(v) => self.variant(v.tag(), v.cases.iter().map(|c| &c.ty)),
            TypeDefKind::Enum(e) => self.variant(e.tag(), []),
            TypeDefKind::Option(t) => self.variant(Int::U8, [&Type::Unit, t]),
            TypeDefKind::Expected(e) => self.variant(Int::U8, [&e.ok, &e.err]),
            TypeDefKind::Union(u) => self.variant(u.tag(), u.cases.iter().map(|c| &c.ty)),
            // A future is represented as an index.
            TypeDefKind::Future(_) => (4, 4),
            // A stream is represented as an index.
            TypeDefKind::Stream(_) => (4, 4),
        }
    }

    pub fn size(&self, ty: &Type) -> usize {
        match ty {
            Type::Unit => 0,
            Type::Bool | Type::U8 | Type::S8 => 1,
            Type::U16 | Type::S16 => 2,
            Type::U32 | Type::S32 | Type::Float32 | Type::Char | Type::Handle(_) => 4,
            Type::U64 | Type::S64 | Type::Float64 | Type::String => 8,
            Type::Id(id) => self.map[id.index()].0,
        }
    }

    pub fn align(&self, ty: &Type) -> usize {
        match ty {
            Type::Unit | Type::Bool | Type::U8 | Type::S8 => 1,
            Type::U16 | Type::S16 => 2,
            Type::U32 | Type::S32 | Type::Float32 | Type::Char | Type::Handle(_) | Type::String => {
                4
            }
            Type::U64 | Type::S64 | Type::Float64 => 8,
            Type::Id(id) => self.map[id.index()].1,
        }
    }

    pub fn field_offsets<'a>(&self, types: impl IntoIterator<Item = &'a Type>) -> Vec<usize> {
        let mut cur = 0;
        types
            .into_iter()
            .map(|ty| {
                let ret = align_to(cur, self.align(ty));
                cur = ret + self.size(ty);
                ret
            })
            .collect()
    }

    pub fn payload_offset<'a>(&self, tag: Int, cases: impl IntoIterator<Item = &'a Type>) -> usize {
        let mut max_align = 1;
        for ty in cases {
            max_align = max_align.max(self.align(ty));
        }
        let tag_size = int_size_align(tag).0;
        align_to(tag_size, max_align)
    }

    pub fn record<'a>(&self, types: impl Iterator<Item = &'a Type>) -> (usize, usize) {
        let mut size = 0;
        let mut align = 1;
        for ty in types {
            let field_size = self.size(ty);
            let field_align = self.align(ty);
            size = align_to(size, field_align) + field_size;
            align = align.max(field_align);
        }
        (align_to(size, align), align)
    }

    fn variant<'a>(&self, tag: Int, types: impl IntoIterator<Item = &'a Type>) -> (usize, usize) {
        let (discrim_size, discrim_align) = int_size_align(tag);
        let mut size = discrim_size;
        let mut align = discrim_align;
        for ty in types {
            let case_size = self.size(ty);
            let case_align = self.align(ty);
            align = align.max(case_align);
            size = size.max(align_to(discrim_size, case_align) + case_size);
        }
        (size, align)
    }
}

fn int_size_align(i: Int) -> (usize, usize) {
    match i {
        Int::U8 => (1, 1),
        Int::U16 => (2, 2),
        Int::U32 => (4, 4),
        Int::U64 => (8, 8),
    }
}

pub(crate) fn align_to(val: usize, align: usize) -> usize {
    (val + align - 1) & !(align - 1)
}
