use crate::{ident::Ident, Docs, Params, Results};

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Resource {
    pub funcs: Vec<ResourceFunc>,
}

impl Resource {
    pub fn empty() -> Self {
        Self { funcs: vec![] }
    }

    pub fn func(&mut self, func: ResourceFunc) {
        self.funcs.push(func);
    }

    pub fn funcs(&self) -> &[ResourceFunc] {
        &self.funcs
    }

    pub fn funcs_mut(&mut self) -> &mut Vec<ResourceFunc> {
        &mut self.funcs
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ResourceFunc {
    pub(crate) kind: ResourceFuncKind,
    pub(crate) params: Params,
    pub(crate) docs: Option<Docs>,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum ResourceFuncKind {
    Method(Ident, Results),
    Static(Ident, Results),
    Constructor,
}

impl ResourceFunc {
    pub fn method(name: impl Into<Ident>) -> Self {
        Self {
            kind: ResourceFuncKind::Method(name.into(), Results::empty()),
            params: Params::empty(),
            docs: None,
        }
    }

    pub fn static_(name: impl Into<Ident>) -> Self {
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

    pub fn name(&mut self, name: impl Into<Ident>) {
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

    pub fn params(&mut self, params: impl Into<Params>) {
        self.params = params.into();
    }

    pub fn results(&mut self, results: impl Into<Results>) {
        match &self.kind {
            ResourceFuncKind::Method(name, _) => {
                self.kind = ResourceFuncKind::Method(name.clone(), results.into())
            }
            ResourceFuncKind::Static(name, _) => {
                self.kind = ResourceFuncKind::Static(name.clone(), results.into())
            }
            ResourceFuncKind::Constructor => panic!("constructors cannot have results"),
        }
    }

    pub fn docs(&mut self, docs: Option<impl Into<Docs>>) {
        self.docs = docs.map(|d| d.into());
    }
}
