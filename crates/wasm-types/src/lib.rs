//! Basic WebAssembly types.
//!
//! These are simple types, mostly from the WebAssembly spec, that can be shared across different
//! crates like `wasmparser`, `wasmprinter`, and `wasm-encoder`.

use core::fmt;
use index_vec::Idx;

/// A Wasm _typeidx_.
#[derive(Clone, Copy, Debug, Default, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub struct TypeIdx(pub u32);

/// A Wasm _funcidx_.
#[derive(Clone, Copy, Debug, Default, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub struct FuncIdx(pub u32);

/// A Wasm _tableidx_.
#[derive(Clone, Copy, Debug, Default, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub struct TableIdx(pub u32);

/// A Wasm _memidx_.
#[derive(Clone, Copy, Debug, Default, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub struct MemIdx(pub u32);

/// A Wasm _tagidx_.
#[derive(Clone, Copy, Debug, Default, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub struct TagIdx(pub u32);

/// A Wasm _globalidx_.
#[derive(Clone, Copy, Debug, Default, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub struct GlobalIdx(pub u32);

/// A Wasm _elemidx_.
#[derive(Clone, Copy, Debug, Default, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub struct ElemIdx(pub u32);

/// A Wasm _dataidx_.
#[derive(Clone, Copy, Debug, Default, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub struct DataIdx(pub u32);

/// A Wasm _localidx_.
#[derive(Clone, Copy, Debug, Default, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub struct LocalIdx(pub u32);

/// A Wasm _labelidx_.
#[derive(Clone, Copy, Debug, Default, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub struct LabelIdx(pub u32);

/// A Wasm _fieldidx_.
#[derive(Clone, Copy, Debug, Default, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub struct FieldIdx(pub u32);

/// An absolute label index within a Wasm function.
#[derive(Clone, Copy, Debug, Default, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub struct AbsoluteLabelIdx(pub u32);

/// A Wasm core module index.
#[derive(Clone, Copy, Debug, Default, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub struct CoreModuleIdx(pub u32);

/// A Wasm core instance index.
#[derive(Clone, Copy, Debug, Default, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub struct CoreInstanceIdx(pub u32);

/// A Wasm component type index.
#[derive(Clone, Copy, Debug, Default, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub struct ComponentTypeIdx(pub u32);

/// A Wasm component function index.
#[derive(Clone, Copy, Debug, Default, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub struct ComponentFuncIdx(pub u32);

/// A Wasm component index.
#[derive(Clone, Copy, Debug, Default, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub struct ComponentIdx(pub u32);

/// A Wasm component instance index.
#[derive(Clone, Copy, Debug, Default, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub struct ComponentInstanceIdx(pub u32);

/// A Wasm component value index.
#[derive(Clone, Copy, Debug, Default, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub struct ComponentValueIdx(pub u32);

macro_rules! idx_impls {
    ($name:ident) => {
        impl AsMut<u32> for $name {
            fn as_mut(&mut self) -> &mut u32 {
                &mut self.0
            }
        }

        impl fmt::Display for $name {
            fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
                write!(f, "{}", self.0)
            }
        }

        impl Idx for $name {
            fn from_usize(idx: usize) -> Self {
                Self(idx.try_into().unwrap())
            }

            fn index(self) -> usize {
                self.0 as usize
            }
        }
    };
}

idx_impls!(TypeIdx);
idx_impls!(FuncIdx);
idx_impls!(TableIdx);
idx_impls!(MemIdx);
idx_impls!(TagIdx);
idx_impls!(GlobalIdx);
idx_impls!(ElemIdx);
idx_impls!(DataIdx);
idx_impls!(LocalIdx);
idx_impls!(LabelIdx);
idx_impls!(FieldIdx);
idx_impls!(AbsoluteLabelIdx);
idx_impls!(CoreModuleIdx);
idx_impls!(CoreInstanceIdx);
idx_impls!(ComponentTypeIdx);
idx_impls!(ComponentFuncIdx);
idx_impls!(ComponentIdx);
idx_impls!(ComponentInstanceIdx);
idx_impls!(ComponentValueIdx);
