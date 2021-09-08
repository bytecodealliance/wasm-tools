

#[cfg(test)]
mod tests{

    use wasm_encoder::Instruction;
    use wasm_mutate::WasmMutate;
    use wasmparser::{Chunk, Operator, Parser, Payload, Validator};


    // Copied from wasm-smith
    fn validate(validator: &mut Validator, bytes: &[u8]) {
        let err = match validator.validate_all(bytes) {
            Ok(()) => return,
            Err(e) => e,
        };
        drop(std::fs::write("test.wasm", &bytes));
        if let Ok(text) = wasmprinter::print_bytes(bytes) {
            drop(std::fs::write("test.wat", &text));
        }

        panic!("wasm failed to validate {:?}", err);
    }

    /// Since there is no code section, any mutator registered under that pattern wont affect the module
    #[test]
    fn idempotent_header() {
        let original = b"\0asm\x01\0\0\0";
        let mutator = WasmMutate::default();
        
        let mutated = mutator.run(original).unwrap();

        assert_eq!(original.to_vec(), mutated)
    }


    /// Since there is no code section, any mutator registered under that pattern wont affect the module
    #[test]
    fn test_snip_mutator() {
        // From https://developer.mozilla.org/en-US/docs/WebAssembly/Text_format_to_wasm
        let wat = r#"
        (module
            (func (export "exported_func") (result i32)
                i32.const 42
            )
        )
        "#;
        let original = &wat::parse_str(wat).unwrap();
        let mutator = WasmMutate::default();
        // seed is zero, which means first mutator
        
        let mutated = mutator.run(original).unwrap();
        
        println!("{:?}", original);
        println!("{:?}", mutated);
            
        // Down here is the validation for the correct mutation

        let mut parser = Parser::new(0);
        let mut consumed = 0;
        
        loop {
            let (payload, chunksize) = match parser.parse(&mutated[consumed..], true).unwrap() {
                Chunk::NeedMoreData(__) => {
                    // In theory the passed buffer is a complete Wasm module, it should not be need for more data
                    continue;
                },
                Chunk::Parsed { consumed, payload } => (payload, consumed),
            };
            
            if let Payload::End = payload {
                // Break the loop and return
                break;
            }

            // Pass the payload and bytes chunk to the real mutator
            match payload {
                Payload::CodeSectionEntry(reader) => {
                    let ops_reader = reader.get_operators_reader()
                    .unwrap();
                    
                    // Check now that it is the default value, 0
                    for i in ops_reader.into_iter() {
                        match i.unwrap() {
                            Operator::I32Const{value} => assert_eq!(value, 0),
                            Operator::End => assert_eq!(0, 0),
                            _ => {

                                panic!("Only one default instruction should be")

                            }
                        }
                    }
                },
                _ => {
                    // pass
                }
            }

            consumed += chunksize
        }    


        let mut validator = Validator::new();
        validate(&mut validator, &mutated);
    
    }
    
    #[test]
    fn test_unreachable_mutator() {
        // From https://developer.mozilla.org/en-US/docs/WebAssembly/Text_format_to_wasm
        let wat = r#"
        (module
            (func (export "exported_func")
                i32.const 42
            )
        )
        "#;
        let original = &wat::parse_str(wat).unwrap();
        let mut mutator = WasmMutate::default();
        // seed is zero, which means first mutator
        
        let mutated = mutator.run(original).unwrap();
        
        println!("{:?}", original);
        println!("{:?}", mutated);
            
        // Down here is the validation for the correct mutation

        let mut parser = Parser::new(0);
        let mut consumed = 0;
        
        loop {
            let (payload, chunksize) = match parser.parse(&mutated[consumed..], true).unwrap() {
                Chunk::NeedMoreData(__) => {
                    // In theory the passed buffer is a complete Wasm module, it should not be need for more data
                    continue;
                },
                Chunk::Parsed { consumed, payload } => (payload, consumed),
            };
            
            if let Payload::End = payload {
                // Break the loop and return
                break;
            }

            println!("{:?}", payload);

            // Pass the payload and bytes chunk to the real mutator
            match payload {
                Payload::CodeSectionEntry(reader) => {
                    let ops_reader = reader.get_operators_reader()
                    .unwrap();
                    
                    // Check now that it is the default value, 0
                    for i in ops_reader.into_iter() {
                        let ival = i.unwrap();
                        match ival {
                            Operator::Unreachable => assert_eq!(0, 0),
                            Operator::End => assert_eq!(0, 0),
                            _ => {
                                panic!("Other instruction than unreachable should not be possible for this function {:?}", ival)
                            }
                        }
                    }
                },
                _ => {
                    // pass
                }
            }

            consumed += chunksize
        }
            

        let mut validator = Validator::new();
        validate(&mut validator, &mutated);
    
    }

    
    #[test]
    fn test_remove_export_mutator() {
        // From https://developer.mozilla.org/en-US/docs/WebAssembly/Text_format_to_wasm
        let wat = r#"
        (module
            (func (export "exported_func") (result i32)
                i32.const 42
            )
        )
        "#;
        let original = &wat::parse_str(wat).unwrap();
        let mut mutator = WasmMutate::default();
        // seed is zero, which means first mutator
        
        let mutated = mutator.run(original).unwrap();
        
        println!("{:?}", original);
        println!("{:?}", mutated);
            
        // Down here is the validation for the correct mutation


        let mut validator = Validator::new();
        validate(&mut validator, &mutated);
    
    }
}