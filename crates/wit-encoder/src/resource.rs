use crate::{Docs, Params, Results};

#[derive(Debug, Clone, PartialEq)]
pub struct Resource {
    pub funcs: Vec<ResourceFunc>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ResourceFunc {
    pub(crate) kind: ResourceFuncKind,
    #[cfg_attr(feature = "serde", serde(serialize_with = "serialize_params"))]
    pub(crate) params: Params,
    #[cfg_attr(feature = "serde", serde(skip_serializing_if = "Docs::is_empty"))]
    pub(crate) docs: Option<Docs>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum ResourceFuncKind {
    Method(String, Results),
    Static(String, Results),
    Constructor,
}

impl ResourceFunc {
    pub fn method(name: impl Into<String>) -> Self {
        Self {
            kind: ResourceFuncKind::Method(name.into(), Results::empty()),
            params: Params::empty(),
            docs: None,
        }
    }

    pub fn static_(name: impl Into<String>) -> Self {
        Self {
            kind: ResourceFuncKind::Static(name.into(), Results::empty()),
            params: Params::empty(),
            docs: None,
        }
    }

    pub fn constructor() -> Self {
        Self {
            kind: ResourceFuncKind::Constructor,
            params: Params::empty(),
            docs: None,
        }
    }

    pub fn name(&mut self, name: impl Into<String>) {
        match &self.kind {
            ResourceFuncKind::Method(_, results) => {
                self.kind = ResourceFuncKind::Method(name.into(), results.clone())
            }
            ResourceFuncKind::Static(_, results) => {
                self.kind = ResourceFuncKind::Static(name.into(), results.clone())
            }
            ResourceFuncKind::Constructor => panic!("constructors cannot have a name"),
        }
    }

    pub fn params(&mut self, params: Params) {
        self.params = params;
    }

    pub fn results(&mut self, results: Results) {
        match &self.kind {
            ResourceFuncKind::Method(name, _) => {
                self.kind = ResourceFuncKind::Method(name.clone(), results)
            }
            ResourceFuncKind::Static(name, _) => {
                self.kind = ResourceFuncKind::Static(name.clone(), results)
            }
            ResourceFuncKind::Constructor => panic!("constructors cannot have results"),
        }
    }

    pub fn docs(&mut self, docs: Option<impl Into<Docs>>) {
        self.docs = docs.map(|d| d.into());
    }
}
