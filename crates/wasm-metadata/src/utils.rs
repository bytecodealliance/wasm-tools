use anyhow::Result;
use index_vec::Idx;
use wasm_encoder::Encode;
use wasmparser::FromReader;

pub(crate) fn name_map<'a, I: Idx + FromReader<'a> + Encode>(
    map: &wasmparser::NameMap<'a, I>,
) -> Result<wasm_encoder::NameMap<I>> {
    let mut out = wasm_encoder::NameMap::new();
    for m in map.clone().into_iter() {
        let m = m?;
        out.append(m.index, m.name);
    }
    Ok(out)
}

pub(crate) fn indirect_name_map<
    'a,
    I: Idx + FromReader<'a> + Encode,
    J: Idx + FromReader<'a> + Encode,
>(
    map: &wasmparser::IndirectNameMap<'a, I, J>,
) -> Result<wasm_encoder::IndirectNameMap<I>> {
    let mut out = wasm_encoder::IndirectNameMap::new();
    for m in map.clone().into_iter() {
        let m = m?;
        out.append(m.index, &name_map(&m.names)?);
    }
    Ok(out)
}
