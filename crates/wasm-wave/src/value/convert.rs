use crate::{
    WasmValue,
    value::ValueEnum,
    wasm::{WasmType, WasmTypeKind},
};

use super::{Type, Value};

pub fn from_wasm_type(ty: &impl WasmType) -> Option<Type> {
    if let Some(ty) = Type::simple(ty.kind()) {
        return Some(ty);
    }
    Some(match ty.kind() {
        WasmTypeKind::List => Type::list(from_wasm_type(&ty.list_element_type()?)?),
        WasmTypeKind::Record => Type::record(
            ty.record_fields()
                .map(|(name, ty)| Some((name, from_wasm_type(&ty)?)))
                .collect::<Option<Vec<_>>>()?,
        )?,
        WasmTypeKind::Tuple => Type::tuple(
            ty.tuple_element_types()
                .map(|ty| from_wasm_type(&ty))
                .collect::<Option<Vec<_>>>()?,
        )?,
        WasmTypeKind::Variant => Type::variant(
            ty.variant_cases()
                .map(|(name, payload)| Some((name, from_optional_wasm_type(payload)?)))
                .collect::<Option<Vec<_>>>()?,
        )?,
        WasmTypeKind::Enum => Type::enum_ty(ty.enum_cases())?,
        WasmTypeKind::Option => Type::option(from_wasm_type(&ty.option_some_type()?)?),
        WasmTypeKind::Result => {
            let (ok, err) = ty.result_types()?;
            Type::result(from_optional_wasm_type(ok)?, from_optional_wasm_type(err)?)
        }
        WasmTypeKind::Flags => Type::flags(ty.flags_names())?,
        _ => return None,
    })
}

fn from_optional_wasm_type(ty: Option<impl WasmType>) -> Option<Option<Type>> {
    Some(match ty {
        Some(ty) => Some(from_wasm_type(&ty)?),
        None => None,
    })
}

pub trait ValueTyped {
    fn value_type() -> Type;
}
pub trait ToRust<T> {
    fn to_rust(&self) -> T;
}
pub trait ToValue {
    fn to_value(&self) -> Value;
}

macro_rules! impl_primitives {
    ($Self:ty, $(($case:ident, $ty:ty, $unwrap:ident)),*) => {
        $(
            impl ValueTyped for $ty {
                fn value_type() -> Type {
                    Type::must_simple(WasmTypeKind::$case)
                }
            }

            impl From<$ty> for $Self {
                fn from(value: $ty) -> Self {
                    Self(ValueEnum::$case(value))
                }
            }

            impl ToRust<$ty> for $Self {
                fn to_rust(&self) -> $ty {
                    self.$unwrap()
                }
            }
            impl ToValue for $ty {
                fn to_value(&self) -> Value {
                    Value(ValueEnum::$case(*self))
                }
            }
        )*
    };
}

impl_primitives!(
    Value,
    (Bool, bool, unwrap_bool),
    (S8, i8, unwrap_s8),
    (S16, i16, unwrap_s16),
    (S32, i32, unwrap_s32),
    (S64, i64, unwrap_s64),
    (U8, u8, unwrap_u8),
    (U16, u16, unwrap_u16),
    (U32, u32, unwrap_u32),
    (U64, u64, unwrap_u64),
    (F32, f32, unwrap_f32),
    (F64, f64, unwrap_f64),
    (Char, char, unwrap_char)
);

impl ValueTyped for String {
    fn value_type() -> Type {
        Type::STRING
    }
}

impl From<String> for Value {
    fn from(value: String) -> Self {
        Self(ValueEnum::String(value.into()))
    }
}
impl ToRust<String> for Value {
    fn to_rust(&self) -> String {
        self.unwrap_string().into()
    }
}
impl ToValue for String {
    fn to_value(&self) -> Value {
        Value(ValueEnum::String(self.to_owned().into()))
    }
}

impl ValueTyped for &str {
    fn value_type() -> Type {
        String::value_type()
    }
}

impl<'a> From<&'a str> for Value {
    fn from(value: &'a str) -> Self {
        value.to_string().into()
    }
}
impl<'a> ToValue for &'a str {
    fn to_value(&self) -> Value {
        self.to_string().to_value()
    }
}

impl<const N: usize, T: ValueTyped> ValueTyped for [T; N] {
    fn value_type() -> Type {
        Type::list(T::value_type())
    }
}
impl<T: ValueTyped> ValueTyped for [T] {
    fn value_type() -> Type {
        Type::list(T::value_type())
    }
}

impl<const N: usize, T: ValueTyped + Into<Value>> From<[T; N]> for Value {
    fn from(values: [T; N]) -> Self {
        let ty = Vec::<T>::value_type();
        let values = values.into_iter().map(Into::into);
        Value::make_list(&ty, values).unwrap()
    }
}
impl<T: ValueTyped + ToValue> ToValue for [T] {
    fn to_value(&self) -> Value {
        let ty = <[T]>::value_type();
        let values = self.iter().map(|x| x.to_value());
        Value::make_list(&ty, values).unwrap()
    }
}

impl<T: ValueTyped> ValueTyped for Vec<T> {
    fn value_type() -> Type {
        Type::list(T::value_type())
    }
}

impl<T: ValueTyped + Into<Value>> From<Vec<T>> for Value {
    fn from(values: Vec<T>) -> Self {
        let ty = Vec::<T>::value_type();
        let values = values.into_iter().map(Into::into);
        Value::make_list(&ty, values).unwrap()
    }
}
impl<T: ToValue + ValueTyped> ToValue for Vec<T> {
    fn to_value(&self) -> Value {
        let ty = Vec::<T>::value_type();
        let values = self.iter().map(|x| x.to_value());
        Value::make_list(&ty, values).unwrap()
    }
}
impl<T> ToRust<Vec<T>> for Value
where
    Value: ToRust<T>,
{
    fn to_rust(&self) -> Vec<T> {
        self.unwrap_list().map(|x| x.to_rust()).collect()
    }
}

impl<T: ValueTyped> ValueTyped for Option<T> {
    fn value_type() -> Type {
        Type::option(T::value_type())
    }
}

impl<T: ValueTyped + Into<Value>> From<Option<T>> for Value {
    fn from(value: Option<T>) -> Self {
        let ty = Option::<T>::value_type();
        Value::make_option(&ty, value.map(Into::into)).unwrap()
    }
}
impl<T: ValueTyped + ToValue> ToValue for Option<T> {
    fn to_value(&self) -> Value {
        let ty = Option::<T>::value_type();
        Value::make_option(&ty, self.as_ref().map(|x| x.to_value())).unwrap()
    }
}
impl<T> ToRust<Option<T>> for Value
where
    Value: ToRust<T>,
{
    fn to_rust(&self) -> Option<T> {
        self.unwrap_option().map(|x| x.to_rust())
    }
}

impl ValueTyped for Result<(), ()> {
    fn value_type() -> Type {
        Type::result(None, None)
    }
}
impl<T: ValueTyped> ValueTyped for Result<T, ()> {
    fn value_type() -> Type {
        Type::result(Some(T::value_type()), None)
    }
}
impl<U: ValueTyped> ValueTyped for Result<(), U> {
    fn value_type() -> Type {
        Type::result(None, Some(U::value_type()))
    }
}
impl<T: ValueTyped, U: ValueTyped> ValueTyped for Result<T, U> {
    fn value_type() -> Type {
        Type::result(Some(T::value_type()), Some(U::value_type()))
    }
}

impl<T: ValueTyped + Into<Value>, U: ValueTyped + Into<Value>> From<Result<T, U>> for Value {
    fn from(value: Result<T, U>) -> Self {
        let ty = Result::<T, U>::value_type();
        let value = match value {
            Ok(ok) => Ok(Some(ok.into())),
            Err(err) => Err(Some(err.into())),
        };
        Value::make_result(&ty, value).unwrap()
    }
}
impl ToValue for Result<(), ()> {
    fn to_value(&self) -> Value {
        let ty = Result::<(), ()>::value_type();
        let value = match self {
            Ok(()) => Ok(None),
            Err(()) => Err(None),
        };
        Value::make_result(&ty, value).unwrap()
    }
}
impl<T: ValueTyped + ToValue> ToValue for Result<T, ()> {
    fn to_value(&self) -> Value {
        let ty = Result::<T, ()>::value_type();
        let value = match self {
            Ok(ok) => Ok(Some(ok.to_value())),
            Err(()) => Err(None),
        };
        Value::make_result(&ty, value).unwrap()
    }
}
impl<U: ValueTyped + ToValue> ToValue for Result<(), U> {
    fn to_value(&self) -> Value {
        let ty = Result::<(), U>::value_type();
        let value = match self {
            Ok(()) => Ok(None),
            Err(err) => Err(Some(err.to_value())),
        };
        Value::make_result(&ty, value).unwrap()
    }
}
impl<T: ValueTyped + ToValue, U: ValueTyped + ToValue> ToValue for Result<T, U> {
    fn to_value(&self) -> Value {
        let ty = Result::<T, U>::value_type();
        let value = match self {
            Ok(ok) => Ok(Some(ok.to_value())),
            Err(err) => Err(Some(err.to_value())),
        };
        Value::make_result(&ty, value).unwrap()
    }
}
impl ToRust<Result<(), ()>> for Value {
    fn to_rust(&self) -> Result<(), ()> {
        match self.unwrap_result() {
            Ok(None) => Ok(()),
            Err(None) => Err(()),
            _ => unreachable!(),
        }
    }
}
impl<T> ToRust<Result<T, ()>> for Value
where
    Value: ToRust<T>,
{
    fn to_rust(&self) -> Result<T, ()> {
        match self.unwrap_result() {
            Ok(Some(ok)) => Ok(ok.to_rust()),
            Err(None) => Err(()),
            _ => unreachable!(),
        }
    }
}
impl<U> ToRust<Result<(), U>> for Value
where
    Value: ToRust<U>,
{
    fn to_rust(&self) -> Result<(), U> {
        match self.unwrap_result() {
            Ok(None) => Ok(()),
            Err(Some(err)) => Err(err.to_rust()),
            _ => unreachable!(),
        }
    }
}
impl<T, U> ToRust<Result<T, U>> for Value
where
    Value: ToRust<T> + ToRust<U>,
{
    fn to_rust(&self) -> Result<T, U> {
        match self.unwrap_result() {
            Ok(Some(ok)) => Ok(ok.to_rust()),
            Err(Some(err)) => Err(err.to_rust()),
            _ => unreachable!(),
        }
    }
}

macro_rules! impl_tuple {
    ($(($($var:ident),*)),*) => {
        $(
            impl<$($var: ValueTyped),*> ValueTyped for ($($var),*,) {
                fn value_type() -> Type {
                    Type::tuple(vec![$($var::value_type()),*]).unwrap()
                }
            }

            #[allow(non_snake_case)]
            impl<$($var: ValueTyped + Into<Value>),*> From<($($var),*,)> for Value {
                fn from(($($var),*,): ($($var),*,)) -> Value {
                    let ty = <($($var),*,)>::value_type();
                    $(
                        let $var = $var.into();
                    )*
                    Value::make_tuple(&ty, vec![$($var),*]).unwrap()
                }
            }
            #[allow(non_snake_case)]
            impl<$($var: ValueTyped + ToValue),*> ToValue for ($($var),*,) {
                fn to_value(&self) -> Value {
                    let ty = <($($var),*,)>::value_type();
                    let ($($var),*,) = self;
                    $(
                        let $var = $var.to_value();
                    )*
                    Value::make_tuple(&ty, vec![$($var),*]).unwrap()
                }
            }

            impl<$($var),*> ToRust<($($var),*,)> for Value where $( Value: ToRust<$var> ),* {
                fn to_rust(&self) -> ($($var),*,) {
                    let mut iter = self.unwrap_tuple();
                    ($(
                        ToRust::<$var>::to_rust(iter.next().unwrap().as_ref()),
                    )*)
                }
            }
        )*
    };
}

impl_tuple!(
    (T1),
    (T1, T2),
    (T1, T2, T3),
    (T1, T2, T3, T4),
    (T1, T2, T3, T4, T5),
    (T1, T2, T3, T4, T5, T6),
    (T1, T2, T3, T4, T5, T6, T7),
    (T1, T2, T3, T4, T5, T6, T7, T8),
    (T1, T2, T3, T4, T5, T6, T7, T8, T9),
    (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10),
    (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11),
    (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12),
    (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13),
    (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14),
    (
        T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15
    ),
    (
        T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16
    )
);

#[cfg(test)]
mod tests {
    use crate::value::{Type, Value};

    #[test]
    fn type_conversion_round_trips() {
        for ty in [
            Type::BOOL,
            Type::U8,
            Type::F32,
            Type::STRING,
            Type::list(Type::BOOL),
            Type::record([("a", Type::BOOL)]).unwrap(),
            Type::tuple([Type::BOOL]).unwrap(),
            Type::variant([("a", None), ("b", Some(Type::BOOL))]).unwrap(),
            Type::enum_ty(["north", "south"]).unwrap(),
            Type::option(Type::BOOL),
            Type::result(Some(Type::BOOL), None),
            Type::flags(["read", "write"]).unwrap(),
        ] {
            let got = Type::from_wasm_type(&ty).unwrap();
            assert_eq!(got, ty);
        }
    }

    #[test]
    fn value_conversions() {
        for (val, expect) in [
            (1u8.into(), "1"),
            ((-123i8).into(), "-123"),
            (f32::NAN.into(), "nan"),
            (f64::NEG_INFINITY.into(), "-inf"),
            ('x'.into(), "'x'"),
            ("str".into(), "\"str\""),
            (vec![1, 2, 3].into(), "[1, 2, 3]"),
            ([1, 2, 3].into(), "[1, 2, 3]"),
            (['a'; 3].into(), "['a', 'a', 'a']"),
            (Some(1).into(), "some(1)"),
            (None::<u8>.into(), "none"),
            (Ok::<u8, String>(1).into(), "ok(1)"),
            (Ok::<(), ()>(()).into(), "ok"),
            (Err::<u8, String>("oops".into()).into(), "err(\"oops\")"),
            ((1,).into(), "(1)"),
            ((1, "str", [9; 2]).into(), "(1, \"str\", [9, 9])"),
        ] {
            let val: Value = val;
            let got = crate::to_string(&val).unwrap();
            assert_eq!(got, expect);
        }
    }
}
