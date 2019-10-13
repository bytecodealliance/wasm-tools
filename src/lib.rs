pub mod ast;
pub mod lexer;
pub mod parser;
pub mod resolve;
pub mod binary;

/// Converts an offset within `self.input` to a line/column number.
///
/// Note that both the line and the column are 0-based.
fn to_linecol(input: &str, offset: usize) -> (usize, usize) {
    let mut cur = 0;
    // Use split_terminator instead of lines so that if there is a `\r`,
    // it is included in the offset calculation. The `+1` values below
    // account for the `\n`.
    for (i, line) in input.split_terminator('\n').enumerate() {
        if cur + line.len() + 1 > offset {
            return (i, offset - cur);
        }
        cur += line.len() + 1;
    }
    (input.lines().count(), 0)
}
