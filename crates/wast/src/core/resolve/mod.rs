use crate::core::*;
use crate::token::Index;
use crate::{Error, gensym};

mod deinline_import_export;
mod names;
pub(crate) mod types;

#[cfg(feature = "component-model")]
pub(crate) use names::ResolveCoreType;

#[derive(PartialEq, Eq, Hash, Copy, Clone, Debug)]
pub enum Ns {
    Func,
    Table,
    Global,
    Memory,
    Tag,
    Type,
}

pub fn resolve<'a>(fields: &mut Vec<ModuleField<'a>>) -> Result<Names<'a>, Error> {
    // Ensure that each resolution of a module is deterministic in the names
    // that it generates by resetting our thread-local symbol generator.
    gensym::reset();

    // First up, de-inline import/export annotations.
    //
    // This ensures we only have to deal with inline definitions and to
    // calculate exports we only have to look for a particular kind of module
    // field.
    deinline_import_export::run(fields);

    // With a canonical form of imports make sure that imports are all listed
    // first.
    let mut last = None;
    for field in fields.iter() {
        match field {
            ModuleField::Import(i) => {
                if let Some(name) = last {
                    return Err(Error::new(i.span, format!("import after {name}")));
                }
            }
            ModuleField::Memory(_) => last = Some("memory"),
            ModuleField::Func(_) => last = Some("function"),
            ModuleField::Table(_) => last = Some("table"),
            ModuleField::Global(_) => last = Some("global"),
            _ => continue,
        }
    }

    // Expand all `TypeUse` annotations so all necessary `type` nodes are
    // present in the AST.
    types::expand(fields);

    // Perform name resolution over all `Index` items to resolve them all to
    // indices instead of symbolic names.
    let resolver = names::resolve(fields)?;
    Ok(Names { resolver })
}

/// Representation of the results of name resolution for a module.
///
/// This structure is returned from the
/// [`Module::resolve`](crate::core::Module::resolve) function and can be used
/// to resolve your own name arguments if you have any.
#[derive(Default)]
pub struct Names<'a> {
    resolver: names::Resolver<'a>,
}

impl<'a> Names<'a> {
    /// Resolves `idx` within the function namespace.
    ///
    /// If `idx` is a `Num`, it is ignored, but if it's an `Id` then it will be
    /// looked up in the function namespace and converted to a `Num`. If the
    /// `Id` is not defined then an error will be returned.
    pub fn resolve_func(&self, idx: &mut Index<'a>) -> Result<(), Error> {
        self.resolver.resolve(idx, Ns::Func)?;
        Ok(())
    }

    /// Resolves `idx` within the memory namespace.
    ///
    /// If `idx` is a `Num`, it is ignored, but if it's an `Id` then it will be
    /// looked up in the memory namespace and converted to a `Num`. If the
    /// `Id` is not defined then an error will be returned.
    pub fn resolve_memory(&self, idx: &mut Index<'a>) -> Result<(), Error> {
        self.resolver.resolve(idx, Ns::Memory)?;
        Ok(())
    }

    /// Resolves `idx` within the table namespace.
    ///
    /// If `idx` is a `Num`, it is ignored, but if it's an `Id` then it will be
    /// looked up in the table namespace and converted to a `Num`. If the
    /// `Id` is not defined then an error will be returned.
    pub fn resolve_table(&self, idx: &mut Index<'a>) -> Result<(), Error> {
        self.resolver.resolve(idx, Ns::Table)?;
        Ok(())
    }

    /// Resolves `idx` within the global namespace.
    ///
    /// If `idx` is a `Num`, it is ignored, but if it's an `Id` then it will be
    /// looked up in the global namespace and converted to a `Num`. If the
    /// `Id` is not defined then an error will be returned.
    pub fn resolve_global(&self, idx: &mut Index<'a>) -> Result<(), Error> {
        self.resolver.resolve(idx, Ns::Global)?;
        Ok(())
    }
}
