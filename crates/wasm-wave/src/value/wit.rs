use wit_parser::{
    Enum, Flags, Function, Record, Resolve, Result_, Tuple, Type, TypeDefKind, TypeId, Variant,
};

use crate::{value, wasm::WasmValueError};

/// Resolves a [`value::Type`] from the given [`wit_parser::Resolve`] and [`TypeId`].
/// # Panics
/// Panics if `type_id` is not valid in `resolve`.
pub fn resolve_wit_type(resolve: &Resolve, type_id: TypeId) -> Result<value::Type, WasmValueError> {
    TypeResolver { resolve }.resolve_type_id(type_id)
}

/// Resolves a [`value::FuncType`] from the given [`wit_parser::Resolve`] and [`Function`].
/// # Panics
/// Panics if `function`'s types are not valid in `resolve`.
pub fn resolve_wit_func_type(
    resolve: &Resolve,
    function: &Function,
) -> Result<value::FuncType, WasmValueError> {
    let resolver = TypeResolver { resolve };
    let params = resolver.resolve_params(&function.params)?;
    let results = match &function.results {
        wit_parser::Results::Named(results) => resolver.resolve_params(results)?,
        wit_parser::Results::Anon(ty) => vec![("".into(), resolver.resolve_type(*ty)?)],
    };
    value::FuncType::new(params, results)
}

struct TypeResolver<'a> {
    resolve: &'a Resolve,
}

type ValueResult = Result<value::Type, WasmValueError>;

impl<'a> TypeResolver<'a> {
    fn resolve_type_id(&self, type_id: TypeId) -> ValueResult {
        self.resolve(&self.resolve.types.get(type_id).unwrap().kind)
    }

    fn resolve_type(&self, ty: Type) -> ValueResult {
        self.resolve(&TypeDefKind::Type(ty))
    }

    fn resolve_params(
        &self,
        params: &[(String, Type)],
    ) -> Result<Vec<(String, value::Type)>, WasmValueError> {
        params
            .iter()
            .map(|(name, ty)| {
                let ty = self.resolve_type(*ty)?;
                Ok((name.clone(), ty))
            })
            .collect()
    }

    fn resolve(&self, mut kind: &'a TypeDefKind) -> ValueResult {
        // Recursively resolve any type defs.
        while let &TypeDefKind::Type(Type::Id(id)) = kind {
            kind = &self.resolve.types.get(id).unwrap().kind;
        }

        match kind {
            TypeDefKind::Record(record) => self.resolve_record(record),
            TypeDefKind::Flags(flags) => self.resolve_flags(flags),
            TypeDefKind::Tuple(tuple) => self.resolve_tuple(tuple),
            TypeDefKind::Variant(variant) => self.resolve_variant(variant),
            TypeDefKind::Enum(enum_) => self.resolve_enum(enum_),
            TypeDefKind::Option(some_type) => self.resolve_option(some_type),
            TypeDefKind::Result(result) => self.resolve_result(result),
            TypeDefKind::List(element_type) => self.resolve_list(element_type),
            TypeDefKind::Type(Type::Bool) => Ok(value::Type::BOOL),
            TypeDefKind::Type(Type::U8) => Ok(value::Type::U8),
            TypeDefKind::Type(Type::U16) => Ok(value::Type::U16),
            TypeDefKind::Type(Type::U32) => Ok(value::Type::U32),
            TypeDefKind::Type(Type::U64) => Ok(value::Type::U64),
            TypeDefKind::Type(Type::S8) => Ok(value::Type::S8),
            TypeDefKind::Type(Type::S16) => Ok(value::Type::S16),
            TypeDefKind::Type(Type::S32) => Ok(value::Type::S32),
            TypeDefKind::Type(Type::S64) => Ok(value::Type::S64),
            TypeDefKind::Type(Type::F32) => Ok(value::Type::FLOAT32),
            TypeDefKind::Type(Type::F64) => Ok(value::Type::FLOAT64),
            TypeDefKind::Type(Type::Char) => Ok(value::Type::CHAR),
            TypeDefKind::Type(Type::String) => Ok(value::Type::STRING),
            TypeDefKind::Type(Type::Id(_)) => unreachable!(),
            other => Err(WasmValueError::UnsupportedType(other.as_str().into())),
        }
    }

    fn resolve_record(&self, record: &Record) -> ValueResult {
        let fields = record
            .fields
            .iter()
            .map(|f| Ok((f.name.as_str(), self.resolve_type(f.ty)?)))
            .collect::<Result<Vec<_>, _>>()?;
        Ok(value::Type::record(fields).unwrap())
    }

    fn resolve_flags(&self, flags: &Flags) -> ValueResult {
        let names = flags.flags.iter().map(|f| f.name.as_str());
        Ok(value::Type::flags(names).unwrap())
    }

    fn resolve_tuple(&self, tuple: &Tuple) -> ValueResult {
        let types = tuple
            .types
            .iter()
            .map(|ty| self.resolve_type(*ty))
            .collect::<Result<Vec<_>, _>>()?;
        Ok(value::Type::tuple(types).unwrap())
    }

    fn resolve_variant(&self, variant: &Variant) -> ValueResult {
        let cases = variant
            .cases
            .iter()
            .map(|case| {
                Ok((
                    case.name.as_str(),
                    case.ty.map(|ty| self.resolve_type(ty)).transpose()?,
                ))
            })
            .collect::<Result<Vec<_>, _>>()?;
        Ok(value::Type::variant(cases).unwrap())
    }

    fn resolve_enum(&self, enum_: &Enum) -> ValueResult {
        let cases = enum_.cases.iter().map(|c| c.name.as_str());
        Ok(value::Type::enum_ty(cases).unwrap())
    }

    fn resolve_option(&self, some_type: &Type) -> ValueResult {
        let some = self.resolve_type(*some_type)?;
        Ok(value::Type::option(some))
    }

    fn resolve_result(&self, result: &Result_) -> ValueResult {
        let ok = result.ok.map(|ty| self.resolve_type(ty)).transpose()?;
        let err = result.err.map(|ty| self.resolve_type(ty)).transpose()?;
        Ok(value::Type::result(ok, err))
    }

    fn resolve_list(&self, element_type: &Type) -> ValueResult {
        let element_type = self.resolve_type(*element_type)?;
        Ok(value::Type::list(element_type))
    }
}

#[cfg(test)]
mod tests {

    use super::*;

    #[test]
    fn resolve_wit_type_smoke_test() {
        let mut resolve = Resolve::new();
        resolve
            .push_str(
                "test.wit",
                "
package test:types;
interface types {
    type uint8 = u8;
}
                ",
            )
            .unwrap();

        let (type_id, _) = resolve.types.iter().next().unwrap();
        let ty = resolve_wit_type(&resolve, type_id).unwrap();
        assert_eq!(ty, value::Type::U8);
    }

    #[test]
    fn resolve_wit_func_type_smoke_test() {
        let mut resolve = Resolve::new();
        resolve
            .push_str(
                "test.wit",
                r#"
package test:types;
interface types {
    type uint8 = u8;
    no-results: func(a: uint8, b: string);
    one-result: func(c: uint8, d: string) -> uint8;
    named-results: func(e: uint8, f: string) -> (x: u8, y: string);
}
                "#,
            )
            .unwrap();

        for (func_name, expected_display) in [
            ("no-results", "func(a: u8, b: string)"),
            ("one-result", "func(c: u8, d: string) -> u8"),
            (
                "named-results",
                "func(e: u8, f: string) -> (x: u8, y: string)",
            ),
        ] {
            let function = resolve
                .interfaces
                .iter()
                .flat_map(|(_, i)| &i.functions)
                .find_map(|(name, function)| (name == func_name).then_some(function))
                .unwrap();
            let ty = resolve_wit_func_type(&resolve, function).unwrap();
            assert_eq!(ty.to_string(), expected_display, "for {function:?}");
        }
    }
}
