use std::{
    num::{NonZero, NonZeroUsize},
    ops::{Add, AddAssign},
};

use crate::{FlagsRepr, Int, Resolve, Type, TypeDef, TypeDefKind};

/// Architecture specific alignment
#[derive(Eq, PartialEq, PartialOrd, Clone, Copy, Debug)]
pub enum Alignment {
    Pointer,
    Bytes(NonZeroUsize),
}

impl Default for Alignment {
    fn default() -> Self {
        Alignment::Bytes(NonZero::new(1).unwrap())
    }
}

impl std::fmt::Display for Alignment {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Alignment::Pointer => f.write_str("ptr"),
            Alignment::Bytes(b) => f.write_fmt(format_args!("{}", b.get())),
        }
    }
}

impl Ord for Alignment {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        match (self, other) {
            (Alignment::Pointer, Alignment::Pointer) => std::cmp::Ordering::Equal,
            (Alignment::Pointer, Alignment::Bytes(b)) => {
                if b.get() > 4 {
                    std::cmp::Ordering::Greater
                } else {
                    std::cmp::Ordering::Less
                }
            }
            (Alignment::Bytes(b), Alignment::Pointer) => {
                if b.get() > 4 {
                    std::cmp::Ordering::Less
                } else {
                    std::cmp::Ordering::Greater
                }
            }
            (Alignment::Bytes(a), Alignment::Bytes(b)) => a.cmp(b),
        }
    }
}

/// Architecture specific measurement of position,
/// the combined amount in bytes is
/// `bytes + if 4 < std::sizeof::<usize> { add_for_64bit } else { 0 }`
#[derive(Default, Clone, Copy, Eq, PartialEq, Debug)]
pub struct ArchitectureSize {
    /// exact value for 32-bit pointers
    pub bytes: usize,
    /// amount of bytes to add for 64-bit architecture
    pub add_for_64bit: usize,
}

impl Add<ArchitectureSize> for ArchitectureSize {
    type Output = ArchitectureSize;

    fn add(self, rhs: ArchitectureSize) -> Self::Output {
        ArchitectureSize {
            bytes: self.bytes + rhs.bytes,
            add_for_64bit: self.add_for_64bit + rhs.add_for_64bit,
        }
    }
}

impl AddAssign<ArchitectureSize> for ArchitectureSize {
    fn add_assign(&mut self, rhs: ArchitectureSize) {
        self.bytes += rhs.bytes;
        self.add_for_64bit += rhs.add_for_64bit;
    }
}

impl From<Alignment> for ArchitectureSize {
    fn from(align: Alignment) -> Self {
        match align {
            Alignment::Bytes(bytes) => ArchitectureSize {
                bytes: bytes.get(),
                add_for_64bit: 0,
            },
            Alignment::Pointer => ArchitectureSize {
                bytes: 4,
                add_for_64bit: 4,
            },
        }
    }
}

impl std::fmt::Display for ArchitectureSize {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.add_for_64bit != 0 {
            if self.bytes > self.add_for_64bit {
                // both
                f.write_fmt(format_args!(
                    "{}+{}*ptrsz",
                    self.constant_bytes(),
                    self.usize_to_add()
                ))
            } else {
                // only pointer
                f.write_fmt(format_args!("{}*ptrsz", self.usize_to_add()))
            }
        } else {
            // only bytes
            f.write_fmt(format_args!("{}", self.constant_bytes()))
        }
    }
}

impl ArchitectureSize {
    fn max(&self, other: &Self) -> Self {
        let new_bytes = self.bytes.max(other.bytes);
        Self {
            bytes: new_bytes,
            add_for_64bit: (self.bytes + self.add_for_64bit).max(other.bytes + other.add_for_64bit)
                - new_bytes,
        }
    }

    pub fn add_bytes(&self, b: usize) -> Self {
        Self {
            bytes: self.bytes + b,
            add_for_64bit: self.add_for_64bit,
        }
    }

    /// The effective offset/size is
    /// `constant_bytes() + std::sizeof::<usize> * usize_to_add()`
    pub fn constant_bytes(&self) -> usize {
        self.bytes - self.add_for_64bit
    }

    pub fn usize_to_add(&self) -> usize {
        self.add_for_64bit / 4
    }

    /// Shortcut for compatibility with previous versions
    pub fn size_wasm32(&self) -> usize {
        self.bytes
    }
}

/// Information per structure element
#[derive(Default)]
pub struct ElementInfo {
    pub size: ArchitectureSize,
    pub align: Alignment,
}

impl From<Alignment> for ElementInfo {
    fn from(align: Alignment) -> Self {
        ElementInfo {
            size: align.into(),
            align,
        }
    }
}

#[derive(Default)]
pub struct SizeAlign {
    map: Vec<ElementInfo>,
}

impl SizeAlign {
    pub fn new() -> Self {
        Self { map: Vec::new() }
    }

    pub fn fill(&mut self, resolve: &Resolve) {
        self.map = Vec::new();
        for (_, ty) in resolve.types.iter() {
            let pair = self.calculate(ty);
            self.map.push(pair);
        }
    }

    fn calculate(&self, ty: &TypeDef) -> ElementInfo {
        match &ty.kind {
            TypeDefKind::Type(t) => ElementInfo {
                size: self.size(t),
                align: self.align(t),
            },
            TypeDefKind::List(_) => ElementInfo {
                size: ArchitectureSize {
                    bytes: 8,
                    add_for_64bit: 8,
                },
                align: Alignment::Pointer,
            },
            TypeDefKind::Record(r) => self.record(r.fields.iter().map(|f| &f.ty)),
            TypeDefKind::Tuple(t) => self.record(t.types.iter()),
            TypeDefKind::Flags(f) => match f.repr() {
                FlagsRepr::U8 => int_size_align(Int::U8),
                FlagsRepr::U16 => int_size_align(Int::U16),
                FlagsRepr::U32(n) => ElementInfo {
                    size: ArchitectureSize {
                        bytes: n * 4,
                        add_for_64bit: 0,
                    },
                    align: Alignment::Bytes(NonZero::new(4).unwrap()),
                },
            },
            TypeDefKind::Variant(v) => self.variant(v.tag(), v.cases.iter().map(|c| c.ty.as_ref())),
            TypeDefKind::Enum(e) => self.variant(e.tag(), []),
            TypeDefKind::Option(t) => self.variant(Int::U8, [Some(t)]),
            TypeDefKind::Result(r) => self.variant(Int::U8, [r.ok.as_ref(), r.err.as_ref()]),
            // A resource is represented as an index.
            TypeDefKind::Handle(_) => int_size_align(Int::U32),
            // A future is represented as an index.
            TypeDefKind::Future(_) => int_size_align(Int::U32),
            // A stream is represented as an index.
            TypeDefKind::Stream(_) => int_size_align(Int::U32),
            // This shouldn't be used for anything since raw resources aren't part of the ABI -- just handles to
            // them.
            TypeDefKind::Resource => unreachable!(),
            TypeDefKind::Unknown => unreachable!(),
        }
    }

    pub fn size(&self, ty: &Type) -> ArchitectureSize {
        match ty {
            Type::Bool | Type::U8 | Type::S8 => ArchitectureSize {
                bytes: 1,
                add_for_64bit: 0,
            },
            Type::U16 | Type::S16 => ArchitectureSize {
                bytes: 2,
                add_for_64bit: 0,
            },
            Type::U32 | Type::S32 | Type::F32 | Type::Char => ArchitectureSize {
                bytes: 4,
                add_for_64bit: 0,
            },
            Type::U64 | Type::S64 | Type::F64 => ArchitectureSize {
                bytes: 8,
                add_for_64bit: 0,
            },
            Type::String => ArchitectureSize {
                bytes: 8,
                add_for_64bit: 8,
            },
            Type::Id(id) => self.map[id.index()].size,
        }
    }

    pub fn align(&self, ty: &Type) -> Alignment {
        match ty {
            Type::Bool | Type::U8 | Type::S8 => Alignment::Bytes(NonZero::new(1).unwrap()),
            Type::U16 | Type::S16 => Alignment::Bytes(NonZero::new(2).unwrap()),
            Type::U32 | Type::S32 | Type::F32 | Type::Char => {
                Alignment::Bytes(NonZero::new(4).unwrap())
            }
            Type::U64 | Type::S64 | Type::F64 => Alignment::Bytes(NonZero::new(8).unwrap()),
            Type::String => Alignment::Pointer,
            Type::Id(id) => self.map[id.index()].align,
        }
    }

    pub fn field_offsets<'a>(
        &self,
        types: impl IntoIterator<Item = &'a Type>,
    ) -> Vec<(ArchitectureSize, &'a Type)> {
        let mut cur = ArchitectureSize::default();
        types
            .into_iter()
            .map(|ty| {
                let ret = align_to_arch(cur, self.align(ty));
                cur = ret + self.size(ty);
                (ret, ty)
            })
            .collect()
    }

    pub fn payload_offset<'a>(
        &self,
        tag: Int,
        cases: impl IntoIterator<Item = Option<&'a Type>>,
    ) -> ArchitectureSize {
        let mut max_align = Alignment::default();
        for ty in cases {
            if let Some(ty) = ty {
                max_align = max_align.max(self.align(ty));
            }
        }
        let tag_size = int_size_align(tag).size;
        align_to_arch(tag_size, max_align)
    }

    pub fn record<'a>(&self, types: impl Iterator<Item = &'a Type>) -> ElementInfo {
        let mut size = ArchitectureSize::default();
        let mut align = Alignment::default();
        for ty in types {
            let field_size = self.size(ty);
            let field_align = self.align(ty);
            size = align_to_arch(size, field_align) + field_size;
            align = align.max(field_align);
        }
        ElementInfo {
            size: align_to_arch(size, align),
            align,
        }
    }

    pub fn params<'a>(&self, types: impl IntoIterator<Item = &'a Type>) -> ElementInfo {
        self.record(types.into_iter())
    }

    fn variant<'a>(
        &self,
        tag: Int,
        types: impl IntoIterator<Item = Option<&'a Type>>,
    ) -> ElementInfo {
        let ElementInfo {
            size: discrim_size,
            align: discrim_align,
        } = int_size_align(tag);
        let mut case_size = ArchitectureSize::default();
        let mut case_align = Alignment::default();
        for ty in types {
            if let Some(ty) = ty {
                case_size = case_size.max(&self.size(ty));
                case_align = case_align.max(self.align(ty));
            }
        }
        let align = discrim_align.max(case_align);
        ElementInfo {
            size: align_to_arch(align_to_arch(discrim_size, case_align) + case_size, align),
            align,
        }
    }
}

fn int_size_align(i: Int) -> ElementInfo {
    match i {
        Int::U8 => Alignment::Bytes(NonZero::new(1).unwrap()),
        Int::U16 => Alignment::Bytes(NonZero::new(2).unwrap()),
        Int::U32 => Alignment::Bytes(NonZero::new(4).unwrap()),
        Int::U64 => Alignment::Bytes(NonZero::new(8).unwrap()),
    }
    .into()
}

pub(crate) fn align_to(val: usize, align: usize) -> usize {
    (val + align - 1) & !(align - 1)
}

pub fn align_to_arch(val: ArchitectureSize, align: Alignment) -> ArchitectureSize {
    match align {
        Alignment::Pointer => {
            let new_bytes = align_to(val.bytes, 4);
            let unaligned64 = new_bytes + val.add_for_64bit;
            ArchitectureSize {
                bytes: new_bytes,
                add_for_64bit:
                    // increase if necessary for 64bit alignment
                    val.add_for_64bit
                    + if unaligned64 != align_to(unaligned64, 8) {
                        4
                    } else {
                        0
                    },
            }
        }
        Alignment::Bytes(align_bytes) => {
            let new_bytes = align_to(val.bytes, align_bytes.get());
            ArchitectureSize {
                bytes: new_bytes,
                add_for_64bit: align_to(val.bytes + val.add_for_64bit, align_bytes.get())
                    - new_bytes,
            }
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn align() {
        // u8 + ptr
        assert_eq!(
            align_to_arch(
                ArchitectureSize {
                    bytes: 1,
                    add_for_64bit: 0
                },
                Alignment::Pointer
            ),
            ArchitectureSize {
                bytes: 4,
                add_for_64bit: 4
            }
        );
        // u8 + u64
        assert_eq!(
            align_to_arch(
                ArchitectureSize {
                    bytes: 1,
                    add_for_64bit: 0
                },
                Alignment::Bytes(NonZero::new(8).unwrap())
            ),
            ArchitectureSize {
                bytes: 8,
                add_for_64bit: 0
            }
        );
        // u8 + u32
        assert_eq!(
            align_to_arch(
                ArchitectureSize {
                    bytes: 1,
                    add_for_64bit: 0
                },
                Alignment::Bytes(NonZero::new(4).unwrap())
            ),
            ArchitectureSize {
                bytes: 4,
                add_for_64bit: 0
            }
        );
        // ptr + u64
        assert_eq!(
            align_to_arch(
                ArchitectureSize {
                    bytes: 4,
                    add_for_64bit: 4
                },
                Alignment::Bytes(NonZero::new(8).unwrap())
            ),
            ArchitectureSize {
                bytes: 8,
                add_for_64bit: 0
            }
        );
        // u32 + ptr
        assert_eq!(
            align_to_arch(
                ArchitectureSize {
                    bytes: 4,
                    add_for_64bit: 0
                },
                Alignment::Pointer
            ),
            ArchitectureSize {
                bytes: 4,
                add_for_64bit: 4
            }
        );
        // u32, ptr + u64
        assert_eq!(
            align_to_arch(
                ArchitectureSize {
                    bytes: 8,
                    add_for_64bit: 8
                },
                Alignment::Bytes(NonZero::new(8).unwrap())
            ),
            ArchitectureSize {
                bytes: 8,
                add_for_64bit: 8
            }
        );
        // ptr, u8 + u64
        assert_eq!(
            align_to_arch(
                ArchitectureSize {
                    bytes: 5,
                    add_for_64bit: 4
                },
                Alignment::Bytes(NonZero::new(8).unwrap())
            ),
            ArchitectureSize {
                bytes: 8,
                add_for_64bit: 8
            }
        );
        // ptr, u8 + ptr
        assert_eq!(
            align_to_arch(
                ArchitectureSize {
                    bytes: 5,
                    add_for_64bit: 4
                },
                Alignment::Pointer
            ),
            ArchitectureSize {
                bytes: 8,
                add_for_64bit: 8
            }
        );

        assert_eq!(
            ArchitectureSize {
                bytes: 12,
                add_for_64bit: 0
            }
            .max(&ArchitectureSize {
                bytes: 8,
                add_for_64bit: 8
            }),
            ArchitectureSize {
                bytes: 12,
                add_for_64bit: 4
            }
        );
    }
}
