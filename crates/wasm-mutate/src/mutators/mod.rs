#![doc=r###"
    ## Mutator harnesses


"###]

use std::io::Write;
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

