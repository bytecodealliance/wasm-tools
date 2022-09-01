//! Dummy implementations of things that a Wasm module can import.

use anyhow::Result;
use wasmtime::*;

/// Create a set of dummy functions/globals/etc for the given imports.
pub fn dummy_imports<T>(store: &mut Store<T>, module: &Module) -> Result<Vec<Extern>> {
    let mut result = Vec::new();
    for import in module.imports() {
        result.push(dummy_extern(store, import.ty())?);
    }
    Ok(result)
}

/// Construct a dummy `Extern` from its type signature
pub fn dummy_extern<T>(store: &mut Store<T>, ty: ExternType) -> Result<Extern> {
    Ok(match ty {
        ExternType::Func(func_ty) => Extern::Func(dummy_func(store, func_ty)),
        ExternType::Global(global_ty) => Extern::Global(dummy_global(store, global_ty)),
        ExternType::Table(table_ty) => Extern::Table(dummy_table(store, table_ty)?),
        ExternType::Memory(mem_ty) => Extern::Memory(dummy_memory(store, mem_ty)?),
    })
}

/// Construct a dummy function for the given function type
pub fn dummy_func<T>(store: &mut Store<T>, ty: FuncType) -> Func {
    Func::new(store, ty.clone(), move |_, _, results| {
        for (ret_ty, result) in ty.results().zip(results) {
            *result = dummy_value(ret_ty);
        }
        Ok(())
    })
}

/// Construct a dummy value for the given value type.
pub fn dummy_value(val_ty: ValType) -> Val {
    match val_ty {
        ValType::I32 => Val::I32(0),
        ValType::I64 => Val::I64(0),
        ValType::F32 => Val::F32(0),
        ValType::F64 => Val::F64(0),
        ValType::V128 => Val::V128(0),
        ValType::ExternRef => Val::ExternRef(None),
        ValType::FuncRef => Val::FuncRef(None),
    }
}

/// Construct a sequence of dummy values for the given types.
pub fn dummy_values(val_tys: impl IntoIterator<Item = ValType>) -> Vec<Val> {
    val_tys.into_iter().map(dummy_value).collect()
}

/// Construct a dummy global for the given global type.
pub fn dummy_global<T>(store: &mut Store<T>, ty: GlobalType) -> Global {
    let val = dummy_value(ty.content().clone());
    Global::new(store, ty, val).unwrap()
}

/// Construct a dummy table for the given table type.
pub fn dummy_table<T>(store: &mut Store<T>, ty: TableType) -> Result<Table> {
    let init_val = dummy_value(ty.element());
    Table::new(store, ty, init_val)
}

/// Construct a dummy memory for the given memory type.
pub fn dummy_memory<T>(store: &mut Store<T>, ty: MemoryType) -> Result<Memory> {
    Memory::new(store, ty)
}
