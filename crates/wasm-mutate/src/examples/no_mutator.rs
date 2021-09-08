use wasmparser::Payload;

use crate::mutators::Mutator;

#[derive(Debug)]
pub struct NoMutator;


impl NoMutator {
    pub fn new() -> Self {
        NoMutator{}
    }
}

impl Mutator<Payload<'_>> for NoMutator{
    
}