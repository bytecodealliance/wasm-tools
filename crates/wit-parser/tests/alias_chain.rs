//! A chain of type aliases is as long as the source makes it, and how deep the
//! parser recurses must not depend on that: a stack overflow aborts the process.

use wit_parser::Resolve;

fn chain(aliases: usize) -> String {
    let mut wit = String::from("package a:b;\ninterface i {\n  type t0 = u32;\n");
    for i in 1..aliases {
        wit.push_str(&format!("  type t{i} = t{};\n", i - 1));
    }
    let last = aliases - 1;
    wit.push_str(&format!(
        "  get: func(x: t{last}) -> t{last};\n}}\nworld w {{ import i; }}\n"
    ));
    wit
}

#[test]
fn a_long_alias_chain_parses_on_a_small_stack() {
    let wit = chain(200_000);
    let parse = move || {
        let mut resolve = Resolve::default();
        resolve.push_str("chain.wit", &wit).unwrap();
    };
    // Without threads, as on wasm32-wasip1, parse on the main thread, whose
    // stack is as small as the one asked for here.
    if cfg!(target_family = "wasm") && !cfg!(target_feature = "atomics") {
        parse();
    } else {
        std::thread::Builder::new()
            .stack_size(1 << 20)
            .spawn(parse)
            .unwrap()
            .join()
            .unwrap();
    }
}

#[test]
fn a_borrow_at_the_end_of_an_alias_chain_is_still_found() {
    let wit = "package a:b;
        interface i {
            resource r;
            type b = borrow<r>;
            type c = b;
            type d = c;
            get: func() -> d;
            other: func() -> d;
        }";
    let mut resolve = Resolve::default();
    let err = resolve.push_str("borrow.wit", wit).unwrap_err();
    assert!(format!("{err:#}").contains("borrow<T>"), "{err:#}");
}
