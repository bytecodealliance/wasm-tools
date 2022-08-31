use super::operators::{Frame, OperatorValidator};
use crate::{BinaryReader, HeapType, Result, ValType};
use crate::{FunctionBody, Operator, WasmFeatures, WasmModuleResources};

/// Validation context for a WebAssembly function.
///
/// This structure is created by
/// [`Validator::code_section_entry`](crate::Validator::code_section_entry)
/// and is created per-function in a WebAssembly module. This structure is
/// suitable for sending to other threads while the original
/// [`Validator`](crate::Validator) continues processing other functions.
pub struct FuncValidator<T> {
    validator: OperatorValidator,
    resources: T,
    index: u32,
}

impl<T: WasmModuleResources> FuncValidator<T> {
    /// Creates a new `FuncValidator`.
    ///
    /// The returned `FuncValidator` can be used to validate a function with
    /// the type `ty` specified. The `resources` indicate what the containing
    /// module has for the function to use, and the `features` configure what
    /// WebAssembly proposals are enabled for this function.
    ///
    /// The returned validator can be used to then parse a [`FunctionBody`], for
    /// example, to read locals and validate operators.
    pub fn new(
        index: u32,
        ty: u32,
        offset: usize,
        resources: T,
        features: &WasmFeatures,
    ) -> Result<FuncValidator<T>> {
        Ok(FuncValidator {
            validator: OperatorValidator::new_func(ty, offset, features, &resources)?,
            resources,
            index,
        })
    }

    /// Convenience function to validate an entire function's body.
    ///
    /// You may not end up using this in final implementations because you'll
    /// often want to interleave validation with parsing.
    pub fn validate(&mut self, body: &FunctionBody<'_>) -> Result<()> {
        let mut reader = body.get_binary_reader();
        self.read_locals(&mut reader)?;
        reader.allow_memarg64(self.validator.features.memory64);
        while !reader.eof() {
            reader.visit_operator(self)??;
        }
        self.finish(reader.original_position())
    }

    /// Reads the local defintions from the given `BinaryReader`, often sourced
    /// from a `FunctionBody`.
    ///
    /// This function will automatically advance the `BinaryReader` forward,
    /// leaving reading operators up to the caller afterwards.
    pub fn read_locals(&mut self, reader: &mut BinaryReader<'_>) -> Result<()> {
        for _ in 0..reader.read_var_u32()? {
            let offset = reader.original_position();
            let cnt = reader.read_var_u32()?;
            let ty = reader.read_val_type()?;
            self.define_locals(offset, cnt, ty)?;
        }
        Ok(())
    }

    /// Defines locals into this validator.
    ///
    /// This should be used if the application is already reading local
    /// definitions and there's no need to re-parse the function again.
    pub fn define_locals(&mut self, offset: usize, count: u32, ty: ValType) -> Result<()> {
        self.validator
            .define_locals(offset, count, ty, &self.resources)
    }

    /// Validates the next operator in a function.
    ///
    /// This functions is expected to be called once-per-operator in a
    /// WebAssembly function. Each operator's offset in the original binary and
    /// the operator itself are passed to this function to provide more useful
    /// error messages.
    pub fn op(&mut self, offset: usize, operator: &Operator<'_>) -> Result<()> {
        self.validator
            .with_resources(&self.resources)
            .visit_operator(offset, operator)
    }

    /// Function that must be called after the last opcode has been processed.
    ///
    /// This will validate that the function was properly terminated with the
    /// `end` opcode. If this function is not called then the function will not
    /// be properly validated.
    ///
    /// The `offset` provided to this function will be used as a position for an
    /// error if validation fails.
    pub fn finish(&mut self, offset: usize) -> Result<()> {
        self.validator.finish(offset)
    }

    /// Returns the underlying module resources that this validator is using.
    pub fn resources(&self) -> &T {
        &self.resources
    }

    /// The index of the function within the module's function index space that
    /// is being validated.
    pub fn index(&self) -> u32 {
        self.index
    }

    /// Returns the number of defined local variables in the function.
    pub fn len_locals(&self) -> u32 {
        self.validator.locals.len_locals()
    }

    /// Returns the type of the local variable at the given `index` if any.
    pub fn get_local_type(&self, index: u32) -> Option<ValType> {
        self.validator.locals.get(index)
    }

    /// Get the current height of the operand stack.
    ///
    /// This returns the height of the whole operand stack for this function,
    /// not just for the current control frame.
    pub fn operand_stack_height(&self) -> u32 {
        self.validator.operand_stack_height() as u32
    }

    /// Returns the optional value type of the value operand at the given
    /// `depth` from the top of the operand stack.
    ///
    /// - Returns `None` if the `depth` is out of bounds.
    /// - Returns `Some(None)` if there is a value with unknown type
    /// at the given `depth`.
    ///
    /// # Note
    ///
    /// A `depth` of 0 will refer to the last operand on the stack.
    pub fn get_operand_type(&self, depth: usize) -> Option<Option<ValType>> {
        self.validator.peek_operand_at(depth)
    }

    /// Returns the number of frames on the control flow stack.
    ///
    /// This returns the height of the whole control stack for this function,
    /// not just for the current control frame.
    pub fn control_stack_height(&self) -> u32 {
        self.validator.control_stack_height() as u32
    }

    /// Returns a shared reference to the control flow [`Frame`] of the
    /// control flow stack at the given `depth` if any.
    ///
    /// Returns `None` if the `depth` is out of bounds.
    ///
    /// # Note
    ///
    /// A `depth` of 0 will refer to the last frame on the stack.
    pub fn get_control_frame(&self, depth: usize) -> Option<&Frame> {
        self.validator.get_frame(depth)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::WasmFuncType;

    struct EmptyResources;

    impl WasmModuleResources for EmptyResources {
        type FuncType = EmptyFuncType;

        fn table_at(&self, _at: u32) -> Option<crate::TableType> {
            todo!()
        }
        fn memory_at(&self, _at: u32) -> Option<crate::MemoryType> {
            todo!()
        }
        fn tag_at(&self, _at: u32) -> Option<&Self::FuncType> {
            todo!()
        }
        fn global_at(&self, _at: u32) -> Option<crate::GlobalType> {
            todo!()
        }
        fn func_type_at(&self, _type_idx: u32) -> Option<&Self::FuncType> {
            Some(&EmptyFuncType)
        }
        fn type_index_of_function(&self, _at: u32) -> Option<u32> {
            todo!()
        }
        fn type_of_function(&self, _func_idx: u32) -> Option<&Self::FuncType> {
            todo!()
        }
        fn check_value_type(
            &self,
            _t: ValType,
            _features: &WasmFeatures,
            _offset: usize,
        ) -> Result<()> {
            Ok(())
        }
        fn element_type_at(&self, _at: u32) -> Option<crate::RefType> {
            todo!()
        }
        fn matches(&self, _t1: ValType, _t2: ValType) -> bool {
            todo!()
        }
        fn element_count(&self) -> u32 {
            todo!()
        }
        fn data_count(&self) -> Option<u32> {
            todo!()
        }
        fn is_function_referenced(&self, _idx: u32) -> bool {
            todo!()
        }
    }

    struct EmptyFuncType;

    impl WasmFuncType for EmptyFuncType {
        fn len_inputs(&self) -> usize {
            0
        }
        fn len_outputs(&self) -> usize {
            0
        }
        fn input_at(&self, _at: u32) -> Option<ValType> {
            todo!()
        }
        fn output_at(&self, _at: u32) -> Option<ValType> {
            todo!()
        }
    }

    #[test]
    fn operand_stack_height() {
        let mut v = FuncValidator::new(0, 0, 0, &EmptyResources, &Default::default()).unwrap();

        // Initially zero values on the stack.
        assert_eq!(v.operand_stack_height(), 0);

        // Pushing a constant value makes use have one value on the stack.
        assert!(v.op(0, &Operator::I32Const { value: 0 }).is_ok());
        assert_eq!(v.operand_stack_height(), 1);

        // Entering a new control block does not affect the stack height.
        assert!(v
            .op(
                1,
                &Operator::Block {
                    ty: crate::BlockType::Empty
                }
            )
            .is_ok());
        assert_eq!(v.operand_stack_height(), 1);

        // Pushing another constant value makes use have two values on the stack.
        assert!(v.op(2, &Operator::I32Const { value: 99 }).is_ok());
        assert_eq!(v.operand_stack_height(), 2);
    }
}

use crate::{BlockType, BrTable, Ieee32, Ieee64, MemArg, VisitOperator, V128};

macro_rules! define_visit_operator {
    ($(@$proposal:ident $op:ident $({ $($arg:ident: $argty:ty),* })? => $visit:ident)*) => {
        $(
            fn $visit(&mut self, offset: usize $($(,$arg: $argty)*)?) -> Result<()> {
                self.validator.with_resources(&self.resources)
                    .$visit(offset $($(,$arg)*)?)
            }
        )*
    }
}

#[allow(unused_variables)]
impl<'a, T> VisitOperator<'a> for FuncValidator<T>
where
    T: WasmModuleResources,
{
    type Output = Result<()>;

    for_each_operator!(define_visit_operator);
}
