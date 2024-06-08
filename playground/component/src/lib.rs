#[allow(warnings)]
mod bindings;

use bindings::Guest;

struct Component;

impl Guest for Component {
    fn parse(contents: String) -> Result<Vec<u8>, String> {
        wat::parse_str(contents).map_err(|e| e.to_string())
    }
}

bindings::export!(Component with_types_in bindings);
