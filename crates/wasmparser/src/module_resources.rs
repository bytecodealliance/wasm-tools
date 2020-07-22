/* Copyright 2019 Mozilla Foundation
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

use std::ops::Range;

/// Types that qualify as Wasm types for validation purposes.
///
/// Must be comparable with `wasmparser` given Wasm types and
/// must be comparable to themselves.
pub trait WasmType: PartialEq<crate::Type> + PartialEq + Eq {
    /// Converts the custom type into a `wasmparser` known type.
    ///
    /// # Note
    ///
    /// This interface is required as bridge until transitioning is complete.
    fn to_parser_type(&self) -> crate::Type;
}

/// Types that qualify as Wasm function types for validation purposes.
pub trait WasmFuncType {
    /// A type that is comparable with Wasm types.
    type Type: WasmType;

    /// Returns the number of input types.
    fn len_inputs(&self) -> usize;
    /// Returns the number of output types.
    fn len_outputs(&self) -> usize;
    /// Returns the type at given index if any.
    ///
    /// # Note
    ///
    /// The returned type may be wrapped by the user crate and thus
    /// the actually returned type only has to be comparable to a Wasm type.
    fn input_at(&self, at: u32) -> Option<&Self::Type>;
    /// Returns the type at given index if any.
    ///
    /// # Note
    ///
    /// The returned type may be wrapped by the user crate and thus
    /// the actually returned type only has to be comparable to a Wasm type.
    fn output_at(&self, at: u32) -> Option<&Self::Type>;

    /// Returns the list of inputs as an iterator.
    fn inputs(&self) -> WasmFuncTypeInputs<'_, Self>
    where
        Self: Sized,
    {
        WasmFuncTypeInputs {
            func_type: self,
            range: 0..self.len_inputs() as u32,
        }
    }

    /// Returns the list of outputs as an iterator.
    fn outputs(&self) -> WasmFuncTypeOutputs<'_, Self>
    where
        Self: Sized,
    {
        WasmFuncTypeOutputs {
            func_type: self,
            range: 0..self.len_outputs() as u32,
        }
    }
}

/// Iterator over the inputs of a Wasm function type.
pub struct WasmFuncTypeInputs<'a, T> {
    /// The iterated-over function type.
    func_type: &'a T,
    /// The range we're iterating over.
    range: Range<u32>,
}

impl<T> Iterator for WasmFuncTypeInputs<'_, T>
where
    T: WasmFuncType,
{
    type Item = crate::Type;

    fn next(&mut self) -> Option<Self::Item> {
        self.range
            .next()
            .map(|i| self.func_type.input_at(i).unwrap().to_parser_type())
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        self.range.size_hint()
    }
}

impl<T> DoubleEndedIterator for WasmFuncTypeInputs<'_, T>
where
    T: WasmFuncType,
{
    fn next_back(&mut self) -> Option<Self::Item> {
        self.range
            .next_back()
            .map(|i| self.func_type.input_at(i).unwrap().to_parser_type())
    }
}

impl<T> ExactSizeIterator for WasmFuncTypeInputs<'_, T>
where
    T: WasmFuncType,
{
    fn len(&self) -> usize {
        self.range.len()
    }
}

impl<'a, T> Clone for WasmFuncTypeInputs<'a, T> {
    fn clone(&self) -> WasmFuncTypeInputs<'a, T> {
        WasmFuncTypeInputs {
            func_type: self.func_type,
            range: self.range.clone(),
        }
    }
}

/// Iterator over the outputs of a Wasm function type.
pub struct WasmFuncTypeOutputs<'a, T> {
    /// The iterated-over function type.
    func_type: &'a T,
    /// The range we're iterating over.
    range: Range<u32>,
}

impl<T> Iterator for WasmFuncTypeOutputs<'_, T>
where
    T: WasmFuncType,
{
    type Item = crate::Type;

    fn next(&mut self) -> Option<Self::Item> {
        self.range
            .next()
            .map(|i| self.func_type.output_at(i).unwrap().to_parser_type())
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        self.range.size_hint()
    }
}

impl<T> DoubleEndedIterator for WasmFuncTypeOutputs<'_, T>
where
    T: WasmFuncType,
{
    fn next_back(&mut self) -> Option<Self::Item> {
        self.range
            .next_back()
            .map(|i| self.func_type.output_at(i).unwrap().to_parser_type())
    }
}

impl<T> ExactSizeIterator for WasmFuncTypeOutputs<'_, T>
where
    T: WasmFuncType,
{
    fn len(&self) -> usize {
        self.range.len()
    }
}

impl<'a, T> Clone for WasmFuncTypeOutputs<'a, T> {
    fn clone(&self) -> WasmFuncTypeOutputs<'a, T> {
        WasmFuncTypeOutputs {
            func_type: self.func_type,
            range: self.range.clone(),
        }
    }
}

/// Types that qualify as Wasm table types for validation purposes.
pub trait WasmTableType {
    /// A type that is comparable with Wasm types.
    type Type: WasmType;

    /// Returns the element type of the table.
    fn element_type(&self) -> &Self::Type;
    /// Returns the initial limit of the table.
    fn initial_limit(&self) -> u32;
    /// Returns the maximum limit of the table if any.
    fn maximum_limit(&self) -> Option<u32>;
}

/// Types that qualify as Wasm memory types for validation purposes.
pub trait WasmMemoryType {
    /// Returns `true` if the linear memory is shared.
    fn is_shared(&self) -> bool;
    /// Returns the initial limit of the linear memory.
    fn initial_limit(&self) -> u32;
    /// Returns the maximum limit of the linear memory if any.
    fn maximum_limit(&self) -> Option<u32>;
}

/// Types that qualify as Wasm global types for validation purposes.
pub trait WasmGlobalType {
    /// A type that is comparable with Wasm types.
    type Type: WasmType;

    /// Returns `true` if the global variable is mutable.
    fn is_mutable(&self) -> bool;
    /// Returns the content type of the global variable.
    fn content_type(&self) -> &Self::Type;
}

/// Types  that qualify as Wasm valiation database.
///
/// # Note
///
/// The `wasmparser` crate provides a builtin validation framework but allows
/// users of this crate to also feed the parsed Wasm into their own data
/// structure while parsing and also validate at the same time without
/// the need of an additional parsing or validation step or copying data around.
pub trait WasmModuleResources {
    /// The function type used for validation.
    type FuncType: WasmFuncType;
    /// The table type used for validation.
    type TableType: WasmTableType;
    /// The memory type used for validation.
    type MemoryType: WasmMemoryType;
    /// The global type used for validation.
    type GlobalType: WasmGlobalType;

    /// Returns the table at given index if any.
    fn table_at(&self, at: u32) -> Option<&Self::TableType>;
    /// Returns the linear memory at given index.
    fn memory_at(&self, at: u32) -> Option<&Self::MemoryType>;
    /// Returns the global variable at given index.
    fn global_at(&self, at: u32) -> Option<&Self::GlobalType>;
    /// Returns the `FuncType` associated with the given type index.
    fn func_type_at(&self, type_idx: u32) -> Option<&Self::FuncType>;
    /// Returns the `FuncType` associated with the given function index.
    fn type_of_function(&self, func_idx: u32) -> Option<&Self::FuncType>;
    /// Returns the element type at the given index.
    fn element_type_at(&self, at: u32) -> Option<crate::Type>;

    /// Returns the number of elements.
    fn element_count(&self) -> u32;
    /// Returns the number of bytes in the Wasm data section.
    fn data_count(&self) -> u32;
    /// Returns whether the function index is referenced in the module anywhere
    /// outside of the start/function sections.
    fn is_function_referenced(&self, idx: u32) -> bool;
}

impl<T> WasmModuleResources for &'_ T
where
    T: ?Sized + WasmModuleResources,
{
    type FuncType = T::FuncType;
    type TableType = T::TableType;
    type MemoryType = T::MemoryType;
    type GlobalType = T::GlobalType;

    fn table_at(&self, at: u32) -> Option<&Self::TableType> {
        T::table_at(self, at)
    }
    fn memory_at(&self, at: u32) -> Option<&Self::MemoryType> {
        T::memory_at(self, at)
    }
    fn global_at(&self, at: u32) -> Option<&Self::GlobalType> {
        T::global_at(self, at)
    }
    fn func_type_at(&self, at: u32) -> Option<&Self::FuncType> {
        T::func_type_at(self, at)
    }
    fn type_of_function(&self, func_idx: u32) -> Option<&Self::FuncType> {
        T::type_of_function(self, func_idx)
    }
    fn element_type_at(&self, at: u32) -> Option<crate::Type> {
        T::element_type_at(self, at)
    }

    fn element_count(&self) -> u32 {
        T::element_count(self)
    }
    fn data_count(&self) -> u32 {
        T::data_count(self)
    }
    fn is_function_referenced(&self, idx: u32) -> bool {
        T::is_function_referenced(self, idx)
    }
}

impl WasmType for crate::Type {
    fn to_parser_type(&self) -> crate::Type {
        *self
    }
}

impl WasmFuncType for crate::FuncType {
    type Type = crate::Type;

    fn len_inputs(&self) -> usize {
        self.params.len()
    }

    fn len_outputs(&self) -> usize {
        self.returns.len()
    }

    fn input_at(&self, at: u32) -> Option<&Self::Type> {
        self.params.get(at as usize)
    }

    fn output_at(&self, at: u32) -> Option<&Self::Type> {
        self.returns.get(at as usize)
    }
}

impl WasmGlobalType for crate::GlobalType {
    type Type = crate::Type;

    fn is_mutable(&self) -> bool {
        self.mutable
    }

    fn content_type(&self) -> &Self::Type {
        &self.content_type
    }
}

impl WasmTableType for crate::TableType {
    type Type = crate::Type;

    fn element_type(&self) -> &Self::Type {
        &self.element_type
    }

    fn initial_limit(&self) -> u32 {
        self.limits.initial
    }

    fn maximum_limit(&self) -> Option<u32> {
        self.limits.maximum
    }
}

impl WasmMemoryType for crate::MemoryType {
    fn is_shared(&self) -> bool {
        self.shared
    }

    fn initial_limit(&self) -> u32 {
        self.limits.initial
    }
    fn maximum_limit(&self) -> Option<u32> {
        self.limits.maximum
    }
}
