#![doc=r###"
    ## Mutator harnesses


"###]

use std::io::Write;
use wasm_encoder::{Export, ExportSection, Function, Instruction};
use wasmparser::{BinaryReaderError, Payload};
use crate::{WasmMutate};


#[doc=r###"

    ```
"###]
pub trait Mutator<T>: Sized + 'static
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

    /// Provides the name of the mutator
    fn name(&self) -> String {
        return format!("{:?}", std::any::type_name::<Self>())
    }
}


/// Default behavior of NoMutator is to be idempotent
pub struct NoMutator;

impl Mutator<Payload<'_>> for NoMutator {
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
                f.instruction(Instruction::End);
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
                f.instruction(Instruction::End);
                f.encode(&mut tmpbuff);
                out_buffer.write(&tmpbuff).expect("Could not write code body");

            },
            _ => panic!("Only code entries are allowed"),
        }
    }
}


pub struct RemoveExportMutator {

}

impl Mutator<Payload<'_>> for RemoveExportMutator{
    fn mutate<'a>(&mut self, config:&'a crate::WasmMutate, chunk: &'a [u8], out_buffer:&'a mut dyn Write, payload: &mut Payload<'_>) -> () {
        match payload {
            
            Payload::ExportSection(reader) => {
                // Select a random export
                let mut exports = ExportSection::new();
                let mut i = 0;
                let max_exports = reader.get_count() as u64;
                let skip_at = config.seed % max_exports;

                (0..max_exports).for_each(|i|{ 
                    let export = reader.read().unwrap();
                    if skip_at != i { // otherwise bypass
                        match export.kind {
                            wasmparser::ExternalKind::Function => { exports.export(export.field, Export::Function(export.index)); },
                            wasmparser::ExternalKind::Table => { exports.export(export.field, Export::Table(export.index)); },
                            wasmparser::ExternalKind::Memory => { exports.export(export.field, Export::Memory(export.index)); },
                            wasmparser::ExternalKind::Global => { exports.export(export.field, Export::Global(export.index)); },
                            wasmparser::ExternalKind::Module => { exports.export(export.field, Export::Module(export.index)); },
                            wasmparser::ExternalKind::Instance => { exports.export(export.field, Export::Instance(export.index)); },
                            _ => {
                                // TODO, Tag, etc
                            }
                        }
                    }
                });
                
                //exports.encode(out_buffer);
            },
            _ => panic!("Only export section is allowed"),
        }
    }
}