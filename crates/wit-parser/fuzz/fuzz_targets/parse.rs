#![no_main]

use libfuzzer_sys::fuzz_target;

fuzz_target!(|data: &[u8]| {
    drop(env_logger::try_init());

    let data = match std::str::from_utf8(data) {
        Ok(s) => s,
        Err(_) => return,
    };

    drop(wit_parser::UnresolvedPackage::parse("foo".as_ref(), &data));
});
