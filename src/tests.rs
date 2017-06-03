/* Copyright 2017 Mozilla Foundation
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

#[cfg(test)]
mod simple_tests {
    use std::io::prelude::*;
    use std::fs::File;
    use parser::Parser;

    fn read_file_data(path: &str) -> Vec<u8> {
        let mut data = Vec::new();
        let mut f = File::open(path).ok().unwrap();
        f.read_to_end(&mut data).unwrap();
        data
    }

    fn parse_file(path: &str) {
        println!("Parsing {}", path);
        let data = read_file_data(path);
        let mut parser = Parser::new(data.as_slice());
        let mut max_iteration = 100000000;
        loop {
            let state = parser.read();
            if state.is_none() {
                break;
            }
            max_iteration -= 1;
            if max_iteration == 0 {
                panic!("Max iterations exceeded");
            }
        }
    }

    #[test]
    fn test_w_naming() {
        parse_file("tests/naming.wasm");
    }

    #[test]
    fn test_w_reloc() {
        parse_file("tests/reloc.wasm");
    }

    #[test]
    fn test_specs() {
        parse_file("tests/spec.wasm");
    }
}
