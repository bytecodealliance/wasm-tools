#![no_main]

use arbitrary::Unstructured;
use libfuzzer_sys::fuzz_target;
use std::sync::OnceLock;

// Helper macro which takes a static list of fuzzers as input which are then
// delegated to internally based on the fuzz target selected.
//
// In general this fuzz target will execute a number of fuzzers all with the
// same input. The `FUZZER` environment variable can be used to forcibly disable
// all but one.
macro_rules! run_fuzzers {
    ($($fuzzer:ident: $kind:ident,)*) => {
        static ENABLED: OnceLock<u32> = OnceLock::new();

        fuzz_target!(|bytes: &[u8]| {
            // Lazily initialize this fuzzer in terms of logging as well as
            // enabled fuzzers via the `FUZZER` env var.
            let enabled = *ENABLED.get_or_init(|| {
                env_logger::init();
                let configured = std::env::var("FUZZER").ok();
                let configured = configured.as_deref();
                let mut enabled = 0;
                let mut index = 0;

                $(
                    if configured.is_none() || configured == Some(stringify!($fuzzer)) {
                        enabled |= 1 << index;
                    }
                    index += 1;
                )*
                let _ = index;

                enabled
            });

            let mut index = 0;
            $(
                if enabled & (1 << index) != 0 {
                    run_fuzzers!(@run $fuzzer $kind bytes index);
                }
                index += 1;
            )*
            let _ = index;
        });
    };

    (@run $fuzzer:ident unstructured $bytes:ident $index:ident) => {
        // Use the first byte of input as a discriminant of which fuzzer to
        // select.
        //
        // Afterwards run the specific fuzzer that the fuzz input is
        // targeted for so long as it's enabled.
        if let Some((which_fuzzer, bytes)) = $bytes.split_first() {
            if *which_fuzzer == $index {
                let mut u = Unstructured::new(bytes);
                let _ = wasm_tools_fuzz::$fuzzer::run(&mut u);
            }
        }
    };

    (@run $fuzzer:ident string $bytes:ident $index:ident) => {
        // For string-based fuzzers run all fuzzers enabled for all
        // string-looking inputs.
        if let Ok(s) = std::str::from_utf8($bytes) {
            wasm_tools_fuzz::$fuzzer::run(s);
        }
    };
}

run_fuzzers! {
    mutate: unstructured,
    validate_valid_module: unstructured,
    roundtrip_wit: unstructured,
    no_traps: unstructured,
    validate: unstructured,
    incremental_parse: unstructured,
    print: unstructured,
    roundtrip: string,
    text_parser: string,
}
