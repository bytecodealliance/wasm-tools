
#![doc=r###"
    ## Mutator harnesses


"###]

use std::io::Write;
use rand::RngCore;
use wasm_encoder::{Export, ExportSection, Function, Instruction, Module, Section, SectionId};
use wasmparser::Payload;
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
    fn mutate<'a, A>(&mut self, _:&'a WasmMutate, chunk: Vec<u8>, sink: &'a mut A, _: &mut T) -> () 
        where A: Extend<u8>
    {
        // The default behavior for a mutator is to pass the same input
        sink.extend::<Vec<u8>>(chunk);
    }

    /// Provides the name of the mutator, mostly used for debugging reasons
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
    fn mutate<'a, A>(&mut self, _:&'a crate::WasmMutate, chunk: Vec<u8>, sink:&'a mut A, payload: &mut Payload<'_>) -> () 
    where A: Extend<u8>
    {
        match payload {
            
            Payload::CodeSectionEntry(reader) => {
                let locals = vec![];                    
                let mut tmpbuff: Vec<u8> = Vec::new();
                let mut f = Function::new(locals);
                f.instruction(Instruction::I32Const(0));
                f.instruction(Instruction::End);
                f.encode(&mut tmpbuff);
                sink.extend(tmpbuff)
            },
            _ => panic!("Only code entries are allowed"),
        }
    }
}

pub struct SetFunction2Unreachable {

}

impl Mutator<Payload<'_>> for SetFunction2Unreachable{
    fn mutate<'a, A>(&mut self, _:&'a crate::WasmMutate, chunk: Vec<u8>, sink:&'a mut A, payload: &mut Payload<'_>) -> () 
        where A: Extend<u8>
    {
        match payload {
            
            Payload::CodeSectionEntry(reader) => {
                let locals = vec![];                    
                let mut tmpbuff: Vec<u8> = Vec::new();
                let mut f = Function::new(locals);
                f.instruction(Instruction::Unreachable);
                f.instruction(Instruction::End);
                f.encode(&mut tmpbuff);
                sink.extend(tmpbuff);

            },
            _ => panic!("Only code entries are allowed"),
        }
    }
}


pub struct RemoveExportMutator {

}

impl Mutator<Payload<'_>> for RemoveExportMutator{
    fn mutate<'a, A>(&mut self, config:&'a crate::WasmMutate, chunk: Vec<u8>, sink:&'a mut A, payload: &mut Payload<'_>) -> () 
        where A: Extend<u8>
    {
        match payload {
            
            Payload::ExportSection(reader) => {
                // Select a random export
                let mut exports = ExportSection::new();
                let max_exports = reader.get_count() as u64;
                let skip_at = config.get_rnd().next_u64() % max_exports;

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
                                panic!("Unknown export {:?}", export)
                            }
                        }
                    } else {
                        #[cfg(debug_assertions)] {
                            eprintln!("Removing export {:?} idx {:?}", export, skip_at);
                        }
                    }
                });
                sink.extend_one(SectionId::Export.into());
                exports.encode(sink);
                
            },
            _ => panic!("Only export section is allowed"),
        }
    }
}