use anyhow::Result;
use wasmtime::{component::*, Config, Engine, Store};

fn main() -> Result<()> {
    let mut config = Config::default();
    config.wasm_component_model(true);

    let engine = Engine::new(&config)?;
    let component = Component::from_file(&engine, "math.wasm")?;
    let linker: Linker<()> = Linker::new(&engine);
    
    let mut store = Store::new(&engine, ());
    let instance = linker.instantiate(&mut store, &component)?;
    let execute = instance.get_typed_func::<(), u32, _>(&mut store, "execute")?;

    println!("{}", execute.call(&mut store, ())?);
    Ok(())
}
