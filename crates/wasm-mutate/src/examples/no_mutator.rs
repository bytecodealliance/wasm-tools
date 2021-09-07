use wasmparser::Payload;

use crate::mutators::Mutator;

pub struct NoMutator {

}

impl Mutator<Payload<'_>> for NoMutator{
    
}