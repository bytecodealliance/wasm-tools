#![doc=r###"
    ## Mutator harnesses


"###]

use std::io::Write;
use wasm_encoder::{Function, Instruction};
use wasmparser::Payload;
use crate::{WasmMutate};


#[doc=r###"

    ```
"###]
pub trait Mutator<T>: Sized
{
    
    /// Method where the mutation happpens
    ///
    /// * `context` instance of WasmMutate
    /// * `chunk` piece of the byte stream corresponding with the payload
    /// * `out_buffer` resulting mutated byte stream
    /// * `mutable` mutable object
    fn mutate<'a>(&mut self, _:&'a WasmMutate, chunk: &'a [u8], out_buffer:&'a mut dyn Write, _: &mut T) -> () {
        // The default behavior for a mutator is to pass the same input
        out_buffer.write(&chunk).expect("Could not write to out buffer");
    }
}

/// A mutator can only be used over "mutable" object, in this context, mutable means pieces of the Wasm module that can be transformed
pub trait Mutable: Sized {
    /// The call to the mutator can be bypassed based on a generic validation. For example, call only if a Payload is an instance of CodeSectionEntry.
    ///
    /// * `context` instance of WasmMutate
    /// * `chunk` piece of the byte stream corresponding with the payload
    /// * `out_buffer` resulting mutated byte stream
    /// * `mutator` mutation operator
    fn run_mutator<'a, V>(&mut self, options:&'a WasmMutate,  chunk: &'a [u8], out_buffer: &'a mut dyn Write, mutator: &mut V)
        where V: Mutator<Self> {
            mutator.mutate(options, chunk, out_buffer, self);
    }
}

impl Mutable for Payload<'_> {
    // Custom validation could be implemented here, for example if the mutator can by applied to this payload
}

// Concrete implementations
pub struct ReturnI32SnipMutator {

}

impl Mutator<Payload<'_>> for ReturnI32SnipMutator{
    fn mutate<'a>(&mut self, _:&'a crate::WasmMutate, chunk: &'a [u8], out_buffer:&'a mut dyn Write, payload: &mut Payload<'_>) -> () {
        match payload {
            
            Payload::CodeSectionEntry(reader) => {
                let locals = vec![];                    
                let mut tmpbuff: Vec<u8> = Vec::new();
                let mut f = Function::new(locals);
                f.instruction(Instruction::I32Const(0));
                f.encode(&mut tmpbuff);
                out_buffer.write(&tmpbuff).expect("Could not write code body");

            },
            _ => panic!("Only code entries are allowed"),
        }
    }
}

pub struct SetFunction2Unreachable {

}

impl Mutator<Payload<'_>> for SetFunction2Unreachable{
    fn mutate<'a>(&mut self, _:&'a crate::WasmMutate, chunk: &'a [u8], out_buffer:&'a mut dyn Write, payload: &mut Payload<'_>) -> () {
        match payload {
            
            Payload::CodeSectionEntry(reader) => {
                let locals = vec![];                    
                let mut tmpbuff: Vec<u8> = Vec::new();
                let mut f = Function::new(locals);
                f.instruction(Instruction::Unreachable);
                f.encode(&mut tmpbuff);
                out_buffer.write(&tmpbuff).expect("Could not write code body");

            },
            _ => panic!("Only code entries are allowed"),
        }
    }
}