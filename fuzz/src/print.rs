use arbitrary::{Unstructured,Result};

pub fn run(u: &mut Unstructured<'_>) -> Result<()> {
    let data = u.bytes(u.len())?;
    drop(wasmprinter::print_bytes(data));
    Ok(())
}

