use crate::ast::*;
use crate::Error;
use gensym::Gensym;

mod component_names;
mod deinline_import_export;
mod gensym;
mod names;
mod types;

#[derive(PartialEq, Eq, Hash, Copy, Clone, Debug)]
pub enum Ns {
    Func,
    Table,
    Global,
    Memory,
    Tag,
    Type,
}

pub fn resolve<'a>(module: &mut Module<'a>) -> Result<Names<'a>, Error> {
    let fields = match &mut module.kind {
        ModuleKind::Text(fields) => fields,
        _ => return Ok(Default::default()),
    };

    let mut gensym = Gensym::new();

    // First up, de-inline import/export annotations.
    //
    // This ensures we only have to deal with inline definitions and to
    // calculate exports we only have to look for a particular kind of module
    // field.
    deinline_import_export::run(fields, &mut gensym);

    // With a canonical form of imports make sure that imports are all listed
    // first.
    let mut last = None;
    for field in fields.iter() {
        match field {
            ModuleField::Import(i) => {
                if let Some(name) = last {
                    return Err(Error::new(i.span, format!("import after {}", name)));
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
    types::expand(fields, &mut gensym);

    // Perform name resolution over all `Index` items to resolve them all to
    // indices instead of symbolic names.
    let resolver = names::resolve(fields)?;
    Ok(Names { resolver })
}

pub fn resolve_component<'a>(component: &mut Component<'a>) -> Result<(), Error> {
    let fields = match &mut component.kind {
        ComponentKind::Text(fields) => fields,
        _ => return Ok(Default::default()),
    };

    // Perform name resolution over all `Index` items to resolve them all to
    // indices instead of symbolic names.
    component_names::resolve(fields)?;
    Ok(())
}

/// Representation of the results of name resolution for a module.
///
/// This structure is returned from the
/// [`Module::resolve`](crate::Module::resolve) function and can be used to
/// resolve your own name arguments if you have any.
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
