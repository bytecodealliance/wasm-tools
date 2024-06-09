#[allow(warnings)]
mod bindings;

use bindings::Guest;

struct Component;

struct StringWriter(pub String);

impl wasmprinter::Print for StringWriter {
    fn write_str(&mut self, s: &str) -> std::io::Result<()> {
        self.0.push_str(s);
        Ok(())
    }
}


impl Guest for Component {
    fn parse(contents: String) -> Result<Vec<u8>, String> {
        wat::parse_str(contents).map_err(|e| e.to_string())
    }

    fn print(contents: Vec<u8>) -> Result<String, String> {
        let config = wasmprinter::Config::new();

        let mut writer = StringWriter(String::new());
        let result = config.print(&contents, &mut writer);

        result.map(|_| writer.0).map_err(|e| e.to_string())
    }
}

bindings::export!(Component with_types_in bindings);
