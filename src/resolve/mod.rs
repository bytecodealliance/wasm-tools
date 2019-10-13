use crate::ast::*;
use std::fmt;

mod expand;
mod names;
mod tyexpand;

#[derive(Debug)]
pub struct ResolveError {
    inner: Box<ResolveErrorInner>,
}

#[derive(Debug)]
struct ResolveErrorInner {
    line: usize,
    col: usize,
    message: String,
}

pub fn resolve(module: &mut Module) -> Result<(), ResolveError> {
    let fields = match &mut module.kind {
        ModuleKind::Text(fields) => fields,
        _ => return Ok(()),
    };

    // First up, let's de-inline import/export annotations since this'll
    // restructure the module and affect how we count indices in future passes
    // since function definitions turn into imports.
    //
    // The first pass switches all inline imports to explicit `Import` items.
    // This pass also counts all `Import` items per-type to start building up
    // the index space so we know the corresponding index for each item.
    //
    // In a second pass we then remove all inline `export` annotations, again
    // counting indices as we go along to ensure we always know the index for
    // what we're exporting.
    //
    // The final step is then taking all of the injected `export` fields and
    // actually pushing them onto our list of fields.
    let mut expander = expand::Expander::default();
    for field in fields.iter_mut() {
        expander.deinline_import(field);
    }
    for field in fields.iter_mut() {
        expander.deinline_export(field);
    }
    fields.extend(expander.to_append.drain(..));

    // For the second pass we resolve all inline type annotations. This will, in
    // the order that we see them, append to the list of types. Note that types
    // are indexed so we're careful to always insert new types just before the
    // field that we're looking at.
    let mut cur = 0;
    let mut expander = tyexpand::Expander::default();
    while cur < fields.len() {
        expander.expand(&mut fields[cur]);
        for new in expander.to_prepend.drain(..) {
            fields.insert(cur, new);
            cur += 1;
        }
        cur += 1;
    }

    // And finally perform name resolution over all `Index` items to resolve
    // them all to indices instead of symbolic names.
    //
    // For this operation we do need to make sure that imports are sorted first
    // because otherwise we'll be calculating indices in the wrong order.
    move_imports_first(fields);
    let mut resolver = names::Resolver::default();
    for field in fields.iter_mut() {
        resolver.register(field);
    }
    for field in fields.iter_mut() {
        resolver.resolve(field)?;
    }
    Ok(())
}

fn move_imports_first(fields: &mut [ModuleField<'_>]) {
    fields.sort_by_key(|f| match f {
        ModuleField::Import(_) => false,
        _ => true,
    });
}

impl ResolveError {
    pub fn line(&self) -> usize {
        self.inner.line
    }
    pub fn col(&self) -> usize {
        self.inner.col
    }
}

impl fmt::Display for ResolveError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.inner.message.fmt(f)
    }
}

impl std::error::Error for ResolveError {}
