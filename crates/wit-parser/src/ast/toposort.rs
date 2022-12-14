use crate::ast::{Id, Span};
use anyhow::Result;
use indexmap::{IndexMap, IndexSet};
use std::collections::HashSet;
use std::fmt;

pub fn toposort<'a>(
    kind: &str,
    deps: &IndexMap<&'a str, Vec<Id<'a>>>,
) -> Result<Vec<&'a str>, Error> {
    // First make sure that all dependencies actually point to other valid items
    // that are known.
    for (_, names) in deps {
        for name in names {
            deps.get(name.name).ok_or_else(|| Error::NonexistentDep {
                span: name.span,
                name: name.name.to_string(),
                kind: kind.to_string(),
            })?;
        }
    }

    // Then recursively visit all dependencies building up the topological order
    // as we go and guarding against cycles with a separate visitation set.
    let mut order = IndexSet::new();
    let mut visiting = HashSet::new();
    for dep in deps.keys() {
        visit(dep, deps, &mut order, &mut visiting, kind)?;
    }
    Ok(order.into_iter().collect())
}

fn visit<'a>(
    dep: &'a str,
    deps: &IndexMap<&'a str, Vec<Id<'a>>>,
    order: &mut IndexSet<&'a str>,
    visiting: &mut HashSet<&'a str>,
    kind: &str,
) -> Result<(), Error> {
    if order.contains(dep) {
        return Ok(());
    }

    for dep in deps[dep].iter() {
        if !visiting.insert(dep.name) {
            return Err(Error::Cycle {
                span: dep.span,
                name: dep.name.to_string(),
                kind: kind.to_string(),
            });
        }
        visit(dep.name, deps, order, visiting, kind)?;
        assert!(visiting.remove(&dep.name));
    }

    assert!(order.insert(dep));
    Ok(())
}

#[derive(Debug)]
pub enum Error {
    NonexistentDep {
        span: Span,
        name: String,
        kind: String,
    },
    Cycle {
        span: Span,
        name: String,
        kind: String,
    },
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Error::NonexistentDep { kind, name, .. } => {
                write!(f, "{kind} `{name}` does not exist")
            }
            Error::Cycle { kind, name, .. } => {
                write!(f, "{kind} `{name}` depends on itself")
            }
        }
    }
}

impl std::error::Error for Error {}

#[cfg(test)]
mod tests {
    use super::*;

    fn id(name: &str) -> Id<'_> {
        Id {
            name,
            span: Span { start: 0, end: 0 },
        }
    }

    #[test]
    fn smoke() {
        let empty: Vec<&str> = Vec::new();
        assert_eq!(toposort("", &IndexMap::new()).unwrap(), empty);

        let mut nonexistent = IndexMap::new();
        nonexistent.insert("a", vec![id("b")]);
        assert!(matches!(
            toposort("", &nonexistent),
            Err(Error::NonexistentDep { .. })
        ));

        let mut one = IndexMap::new();
        one.insert("a", vec![]);
        assert_eq!(toposort("", &one).unwrap(), ["a"]);

        let mut two = IndexMap::new();
        two.insert("a", vec![]);
        two.insert("b", vec![id("a")]);
        assert_eq!(toposort("", &two).unwrap(), ["a", "b"]);

        let mut two = IndexMap::new();
        two.insert("a", vec![id("b")]);
        two.insert("b", vec![]);
        assert_eq!(toposort("", &two).unwrap(), ["b", "a"]);
    }

    #[test]
    fn cycles() {
        let mut cycle = IndexMap::new();
        cycle.insert("a", vec![id("a")]);
        let mut set = IndexSet::new();
        set.insert("a");
        assert!(matches!(toposort("", &cycle), Err(Error::Cycle { .. })));

        let mut cycle = IndexMap::new();
        cycle.insert("a", vec![id("b")]);
        cycle.insert("b", vec![id("c")]);
        cycle.insert("c", vec![id("a")]);
        assert!(matches!(toposort("", &cycle), Err(Error::Cycle { .. })));
    }

    #[test]
    fn depend_twice() {
        let mut two = IndexMap::new();
        two.insert("b", vec![id("a"), id("a")]);
        two.insert("a", vec![]);
        assert_eq!(toposort("", &two).unwrap(), ["a", "b"]);
    }
}
