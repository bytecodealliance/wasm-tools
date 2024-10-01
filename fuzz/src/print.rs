use arbitrary::{Result, Unstructured};

pub fn run(u: &mut Unstructured<'_>) -> Result<()> {
    let mut cfg = wasmprinter::Config::new();
    cfg.fold_instructions(u.arbitrary()?);
    cfg.print_skeleton(u.arbitrary()?);
    cfg.print_offsets(u.arbitrary()?);
    cfg.name_unnamed(u.arbitrary()?);

    let data = u.bytes(u.len())?;
    crate::log_wasm(&data, &cfg);
    let mut dst = String::new();
    let _ = cfg.print(&data, &mut wasmprinter::PrintFmtWrite(&mut dst));
    Ok(())
}
