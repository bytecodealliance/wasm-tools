use serde_derive::Serialize;
use std::ops::Range;

use crate::{Author, Description, Homepage, Licenses, Producers, Source};

/// Metadata associated with a Wasm Component or Module
#[derive(Debug, Serialize, Default)]
#[serde(rename_all = "lowercase")]
pub struct Metadata {
    /// The component name, if any. Found in the component-name section.
    pub name: Option<String>,
    /// The component's producers section, if any.
    pub producers: Option<Producers>,
    /// The component's author section, if any.
    pub author: Option<Author>,
    /// Human-readable description of the binary
    pub description: Option<Description>,
    /// License(s) under which contained software is distributed as an SPDX License Expression.
    pub licenses: Option<Licenses>,
    /// URL to get source code for building the image
    pub source: Option<Source>,
    /// URL to find more information on the binary
    pub homepage: Option<Homepage>,
    /// Byte range of the module in the parent binary
    pub range: Range<usize>,
}
