#![doc=r###"
    ## Mutator harnesses

    Mutator and Mutable traits compose a visitor pattern. A WasmMutate instance receives a Wasm module (a byte stream), parses it and visits its internal structure. Each one of the **Payload** instances is passed to a mutation operators, which writes to a potential output buffer the resulting mutated piece.

    The call to a mutation operator can be handled inside the `Mutable` implementation. For example, call only if a Payload is an instance of Version.

    ```no_run
    TODO
    ```

    A mutator basically writes a chunk of bytes for each Payload that is passed to it. For example, the following code changes the header version of a Wasm module.


    ```no_run
    TODO
    ```


"###]

use std::io::Write;
use wasmparser::Payload;
use crate::{MutationContext, WasmMutate};


#[doc=r###"
    Every mutation operator should implement the Mutator trait for Payload
    ```
    TODO
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
    fn mutate<'a>(&mut self, _:&'a WasmMutate, chunk: &'a [u8], out_buffer:&'a mut dyn Write, _: &mut T, mutation_context: &mut MutationContext) -> () {
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
    fn run_mutator<'a, V>(&mut self, options:&'a WasmMutate,  chunk: &'a [u8], out_buffer: &'a mut dyn Write, mutator: &mut V, mutation_context: &mut MutationContext)
        where V: Mutator<Self> {
            mutator.mutate(options, chunk, out_buffer, self, mutation_context);
    }
}

impl Mutable for Payload<'_> {
    fn run_mutator<'a, V>(&mut self, options:&'a WasmMutate,  chunk: &'a [u8], out_buffer: &'a mut dyn Write, mutator: &mut V, mutation_context: &mut MutationContext)
        where V: Mutator<Self> {

            mutator.mutate(options, chunk, out_buffer, self, mutation_context);

            match self {
                
                Payload::CodeSectionEntry(_) => {
                    // TODO Mutate code section
                    if options.preserve_semantics {
                        // TODO Then evaluate semantic if semantic-equivalence options
                        todo!()
                    }
                },
                _ => {
                   
                }
            }
    }
}

