use anyhow::Result;
use std::io::{self, Read};
use wasmtime::{component::*, Config, Engine, Store};

fn main() -> Result<()> {
    let mut config = Config::default();
    config.wasm_component_model(true);

    let engine = Engine::new(&config)?;
    let component = Component::from_file(&engine, "encryptor.wasm")?;
    let linker: Linker<()> = Linker::new(&engine);

    let mut store = Store::new(&engine, ());
    let instance = linker.instantiate(&mut store, &component)?;
    let encrypt = instance.get_typed_func::<(&[u8],), Vec<u8>, _>(&mut store, "encrypt")?;

    let plain_text: Vec<u8> = io::stdin().bytes().collect::<Result<_, _>>()?;
    let cipher_text = encrypt.call(&mut store, (&plain_text,))?;

    println!("{}", std::str::from_utf8(&cipher_text)?);
    Ok(())
}
