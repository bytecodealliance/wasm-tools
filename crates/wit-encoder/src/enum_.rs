// use std::fmt::Display;

use crate::Case;

/// A variant without a payload
#[derive(Debug, Clone, PartialEq, Default)]
pub struct Enum {
    pub cases: Vec<Case>,
}

// impl Display for Enum {
//     fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
//         todo!()
//     }
// }
