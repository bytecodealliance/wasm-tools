use alloc::string::String;
#[cfg(test)]
use alloc::vec;
use alloc::vec::Vec;
use core::char;
use core::fmt;
use core::result::Result;
use core::str;

use self::Token::*;

#[derive(Clone)]
pub struct Tokenizer<'a> {
    input: &'a str,
    span_offset: u32,
    chars: CrlfFold<'a>,
}

#[derive(Clone)]
struct CrlfFold<'a> {
    chars: str::CharIndices<'a>,
}

/// A span, designating a range of bytes where a token is located.
///
/// Uses `u32::MAX` as a sentinel value to represent unknown spans (e.g.,
/// decoded from binary).
#[derive(Eq, PartialEq, Debug, Clone, Copy, Hash)]
pub struct Span {
    start: u32,
    end: u32,
}

impl Default for Span {
    fn default() -> Span {
        Span {
            start: u32::MAX,
            end: u32::MAX,
        }
    }
}

impl Span {
    pub fn new(start: u32, end: u32) -> Span {
        let span = Span { start, end };
        assert!(span.is_known(), "cannot create a span with u32::MAX");
        span
    }

    /// Adjusts this span by adding the given byte offset to both start and end.
    pub fn adjust(&mut self, offset: u32) {
        if self.is_known() {
            self.start += offset;
            self.end += offset;
        }
    }

    /// Returns the start offset, panicking if this is an unknown span.
    pub fn start(&self) -> u32 {
        assert!(self.is_known(), "cannot get start of unknown span");
        self.start
    }

    /// Returns the end offset, panicking if this is an unknown span.
    pub fn end(&self) -> u32 {
        assert!(self.is_known(), "cannot get end of unknown span");
        self.end
    }

    /// Sets the end offset. If this is unknown, converts to a zero-width span at that position.
    pub fn set_end(&mut self, new_end: u32) {
        if !self.is_known() {
            self.start = new_end;
        }
        self.end = new_end;
    }

    /// Sets the start offset. If this is unknown, converts to a zero-width span at that position.
    pub fn set_start(&mut self, new_start: u32) {
        if !self.is_known() {
            self.end = new_start;
        }
        self.start = new_start;
    }

    /// Returns true if this span has a known source location.
    pub fn is_known(&self) -> bool {
        self.start != u32::MAX && self.end != u32::MAX
    }
}

#[derive(Eq, PartialEq, Debug, Copy, Clone)]
pub enum Token {
    Whitespace,
    Comment,

    Equals,
    Comma,
    Colon,
    Period,
    Semicolon,
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    LessThan,
    GreaterThan,
    RArrow,
    Star,
    At,
    Slash,
    Plus,
    Minus,
    StringLiteral,

    Use,
    Type,
    Func,
    U8,
    U16,
    U32,
    U64,
    S8,
    S16,
    S32,
    S64,
    F32,
    F64,
    Char,
    Record,
    Resource,
    Own,
    Borrow,
    Flags,
    Variant,
    Enum,
    Bool,
    String_,
    Option_,
    Result_,
    Future,
    Stream,
    ErrorContext,
    List,
    Map,
    Underscore,
    As,
    From_,
    Static,
    Interface,
    Tuple,
    Import,
    Export,
    World,
    Package,
    Constructor,
    Async,

    Id,
    ExplicitId,

    Integer,

    Include,
    With,
}

#[derive(Eq, PartialEq, Debug)]
#[allow(dead_code)]
pub enum Error {
    ControlCodepoint(u32, char),
    DeprecatedCodepoint(u32, char),
    ForbiddenCodepoint(u32, char),
    InvalidCharInId(u32, char),
    IdPartEmpty(u32),
    Unexpected(u32, char),
    UnterminatedComment(u32),
    Wanted {
        at: u32,
        expected: &'static str,
        found: &'static str,
    },
    InvalidUnicodeValue(u32, u32),
    InvalidStringElement(u32, char),
    InvalidStringEscape(u32, char),
    WantedChar(u32, char),
    UnexpectedEof(u32),
    InvalidUtf8(u32, core::str::Utf8Error),
    NumberTooBig(u32),
    LoneUnderscore(u32),
    InvalidHexDigit(u32, char),
}

impl<'a> Tokenizer<'a> {
    pub fn new(input: &'a str, span_offset: u32) -> Result<Tokenizer<'a>, Error> {
        detect_invalid_input(input)?;

        let mut t = Tokenizer {
            input,
            span_offset,
            chars: CrlfFold {
                chars: input.char_indices(),
            },
        };
        // Eat utf-8 BOM
        t.eatc('\u{feff}');
        Ok(t)
    }

    pub fn expect_semicolon(&mut self) -> Result<(), Error> {
        self.expect(Token::Semicolon)?;
        Ok(())
    }

    pub fn get_span(&self, span: Span) -> &'a str {
        let start = usize::try_from(span.start() - self.span_offset).unwrap();
        let end = usize::try_from(span.end() - self.span_offset).unwrap();
        &self.input[start..end]
    }

    pub fn parse_id(&self, span: Span) -> Result<&'a str, Error> {
        let ret = self.get_span(span);
        validate_id(span.start(), &ret)?;
        Ok(ret)
    }

    pub fn parse_explicit_id(&self, span: Span) -> Result<&'a str, Error> {
        let token = self.get_span(span);
        let id_part = token.strip_prefix('%').unwrap();
        validate_id(span.start(), id_part)?;
        Ok(id_part)
    }

    pub fn next(&mut self) -> Result<Option<(Span, Token)>, Error> {
        loop {
            match self.next_raw()? {
                Some((_, Token::Whitespace)) | Some((_, Token::Comment)) => {}
                other => break Ok(other),
            }
        }
    }

    /// Three possibilities when calling this method: an `Err(...)` indicates that lexing failed, an
    /// `Ok(Some(...))` produces the next token, and `Ok(None)` indicates that there are no more
    /// tokens available.
    pub fn next_raw(&mut self) -> Result<Option<(Span, Token)>, Error> {
        let (str_start, ch) = match self.chars.next() {
            Some(pair) => pair,
            None => return Ok(None),
        };
        let start = self.span_offset + u32::try_from(str_start).unwrap();
        let token = match ch {
            '\n' | '\t' | ' ' => {
                // Eat all contiguous whitespace tokens
                while self.eatc(' ') || self.eatc('\t') || self.eatc('\n') {}
                Whitespace
            }
            '/' => {
                // Eat a line comment if it's `//...`
                if self.eatc('/') {
                    for (_, ch) in &mut self.chars {
                        if ch == '\n' {
                            break;
                        }
                    }
                    Comment
                // eat a block comment if it's `/*...`
                } else if self.eatc('*') {
                    let mut depth = 1;
                    while depth > 0 {
                        let (_, ch) = match self.chars.next() {
                            Some(pair) => pair,
                            None => return Err(Error::UnterminatedComment(start)),
                        };
                        match ch {
                            '/' if self.eatc('*') => depth += 1,
                            '*' if self.eatc('/') => depth -= 1,
                            _ => {}
                        }
                    }
                    Comment
                } else {
                    Slash
                }
            }
            '=' => Equals,
            ',' => Comma,
            ':' => Colon,
            '.' => Period,
            ';' => Semicolon,
            '(' => LeftParen,
            ')' => RightParen,
            '{' => LeftBrace,
            '}' => RightBrace,
            '<' => LessThan,
            '>' => GreaterThan,
            '*' => Star,
            '@' => At,
            '-' => {
                if self.eatc('>') {
                    RArrow
                } else {
                    Minus
                }
            }
            '+' => Plus,
            '%' => {
                let mut iter = self.chars.clone();
                if let Some((_, ch)) = iter.next() {
                    if is_keylike_start(ch) {
                        self.chars = iter.clone();
                        while let Some((_, ch)) = iter.next() {
                            if !is_keylike_continue(ch) {
                                break;
                            }
                            self.chars = iter.clone();
                        }
                    }
                }
                ExplicitId
            }
            '"' => {
                self.expect_string_literal(start)?;
                StringLiteral
            }
            ch if is_keylike_start(ch) => {
                let remaining = self.chars.chars.as_str().len();
                let mut iter = self.chars.clone();
                while let Some((_, ch)) = iter.next() {
                    if !is_keylike_continue(ch) {
                        break;
                    }
                    self.chars = iter.clone();
                }
                let str_end =
                    str_start + ch.len_utf8() + (remaining - self.chars.chars.as_str().len());
                match &self.input[str_start..str_end] {
                    "use" => Use,
                    "type" => Type,
                    "func" => Func,
                    "u8" => U8,
                    "u16" => U16,
                    "u32" => U32,
                    "u64" => U64,
                    "s8" => S8,
                    "s16" => S16,
                    "s32" => S32,
                    "s64" => S64,
                    "f32" => F32,
                    "f64" => F64,
                    "char" => Char,
                    "resource" => Resource,
                    "own" => Own,
                    "borrow" => Borrow,
                    "record" => Record,
                    "flags" => Flags,
                    "variant" => Variant,
                    "enum" => Enum,
                    "bool" => Bool,
                    "string" => String_,
                    "option" => Option_,
                    "result" => Result_,
                    "future" => Future,
                    "stream" => Stream,
                    "error-context" => ErrorContext,
                    "list" => List,
                    "map" => Map,
                    "_" => Underscore,
                    "as" => As,
                    "from" => From_,
                    "static" => Static,
                    "interface" => Interface,
                    "tuple" => Tuple,
                    "world" => World,
                    "import" => Import,
                    "export" => Export,
                    "package" => Package,
                    "constructor" => Constructor,
                    "include" => Include,
                    "with" => With,
                    "async" => Async,
                    _ => Id,
                }
            }

            ch if ch.is_ascii_digit() => {
                let mut iter = self.chars.clone();
                while let Some((_, ch)) = iter.next() {
                    if !ch.is_ascii_digit() {
                        break;
                    }
                    self.chars = iter.clone();
                }

                Integer
            }

            ch => return Err(Error::Unexpected(start, ch)),
        };
        let end = match self.chars.clone().next() {
            Some((i, _)) => i,
            None => self.input.len(),
        };

        let end = self.span_offset + u32::try_from(end).unwrap();
        Ok(Some((Span::new(start, end), token)))
    }

    pub fn eat(&mut self, expected: Token) -> Result<bool, Error> {
        let mut other = self.clone();
        match other.next()? {
            Some((_span, found)) if expected == found => {
                *self = other;
                Ok(true)
            }
            Some(_) => Ok(false),
            None => Ok(false),
        }
    }

    pub fn expect(&mut self, expected: Token) -> Result<Span, Error> {
        match self.next()? {
            Some((span, found)) => {
                if expected == found {
                    Ok(span)
                } else {
                    Err(Error::Wanted {
                        at: span.start(),
                        expected: expected.describe(),
                        found: found.describe(),
                    })
                }
            }
            None => Err(Error::Wanted {
                at: self.span_offset + u32::try_from(self.input.len()).unwrap(),
                expected: expected.describe(),
                found: "eof",
            }),
        }
    }

    fn eatc(&mut self, ch: char) -> bool {
        let mut iter = self.chars.clone();
        match iter.next() {
            Some((_, ch2)) if ch == ch2 => {
                self.chars = iter;
                true
            }
            _ => false,
        }
    }

    fn expect_any_char(&mut self) -> Result<(u32, char), Error> {
        let end = self.eof_span().end;
        let (pos, c) = self.chars.next().ok_or(Error::UnexpectedEof(end))?;
        let pos = u32::try_from(pos).unwrap();
        Ok((pos, c))
    }

    fn expect_char(&mut self, expected: char) -> Result<(), Error> {
        let (pos, actual) = self.expect_any_char()?;
        if actual == expected {
            Ok(())
        } else {
            Err(Error::WantedChar(pos, expected))
        }
    }

    pub fn string_literal(&mut self, span: Span) -> Result<String, Error> {
        let input = self.get_span(span);
        let input = &input[1..];
        Tokenizer::new(input, span.start + 1)?.expect_string_literal(span.start + 1)
    }

    fn expect_string_literal(&mut self, start: u32) -> Result<String, Error> {
        let mut buf = Vec::new();
        loop {
            let (pos, c) = self.expect_any_char()?;
            match c {
                '"' => break,
                '\\' => {
                    let (pos, c) = self.expect_any_char()?;
                    match c {
                        '"' => buf.push(b'"'),
                        '\'' => buf.push(b'\''),
                        't' => buf.push(b'\t'),
                        'n' => buf.push(b'\n'),
                        'r' => buf.push(b'\r'),
                        '\\' => buf.push(b'\\'),
                        'u' => {
                            self.expect_char('{')?;
                            let n = self.eat_hexnum()?;
                            let c = char::from_u32(n).ok_or(Error::InvalidUnicodeValue(pos, n))?;
                            buf.extend(c.encode_utf8(&mut [0; 4]).as_bytes());
                            self.expect_char('}')?;
                        }
                        c1 if c1.is_ascii_hexdigit() => {
                            let (_, c2) = self.eat_hexdigit()?;
                            buf.push(to_hex(c1) * 16 + c2);
                        }
                        c => return Err(Error::InvalidStringEscape(pos, c)),
                    }
                }
                c if (c as u32) < 0x20 || c as u32 == 0x7f => {
                    return Err(Error::InvalidStringElement(pos, c));
                }
                c => buf.extend(c.encode_utf8(&mut [0; 4]).as_bytes()),
            }
        }
        match String::from_utf8(buf) {
            Ok(s) => Ok(s),
            Err(e) => Err(Error::InvalidUtf8(start, e.utf8_error())),
        }
    }

    pub fn eof_span(&self) -> Span {
        let end = self.span_offset + u32::try_from(self.input.len()).unwrap();
        Span::new(end, end)
    }

    fn eat_hexnum(&mut self) -> Result<u32, Error> {
        let (pos, n) = self.eat_hexdigit()?;
        let mut last_underscore = false;
        let mut n = n as u32;
        loop {
            if self.eatc('_') {
                last_underscore = true;
                continue;
            }
            let (pos, c) = self.clone().expect_any_char()?;
            if !c.is_ascii_hexdigit() {
                break;
            }
            last_underscore = false;
            self.chars.next();
            n = n
                .checked_mul(16)
                .and_then(|n| n.checked_add(to_hex(c) as u32))
                .ok_or(Error::NumberTooBig(pos))?;
        }
        if last_underscore {
            return Err(Error::LoneUnderscore(pos));
        }
        Ok(n)
    }

    /// Reads a hexadecimal digit from the input stream, returning where it's
    /// defined and the hex value. Returns an error on EOF or an invalid hex
    /// digit.
    fn eat_hexdigit(&mut self) -> Result<(u32, u8), Error> {
        let (pos, ch) = self.expect_any_char()?;
        if ch.is_ascii_hexdigit() {
            Ok((pos, to_hex(ch)))
        } else {
            Err(Error::InvalidHexDigit(pos, ch))
        }
    }
}

fn to_hex(c: char) -> u8 {
    match c {
        'a'..='f' => c as u8 - b'a' + 10,
        'A'..='F' => c as u8 - b'A' + 10,
        _ => c as u8 - b'0',
    }
}

impl<'a> Iterator for CrlfFold<'a> {
    type Item = (usize, char);

    fn next(&mut self) -> Option<(usize, char)> {
        self.chars.next().map(|(i, c)| {
            if c == '\r' {
                let mut attempt = self.chars.clone();
                if let Some((_, '\n')) = attempt.next() {
                    self.chars = attempt;
                    return (i, '\n');
                }
            }
            (i, c)
        })
    }
}

fn detect_invalid_input(input: &str) -> Result<(), Error> {
    // Disallow specific codepoints.
    for (pos, ch) in input.char_indices() {
        match ch {
            '\n' | '\r' | '\t' => {}

            // Bidirectional override codepoints can be used to craft source code that
            // appears to have a different meaning than its actual meaning. See
            // [CVE-2021-42574] for background and motivation.
            //
            // [CVE-2021-42574]: https://cve.mitre.org/cgi-bin/cvename.cgi?name=CVE-2021-42574
            '\u{202a}' | '\u{202b}' | '\u{202c}' | '\u{202d}' | '\u{202e}' | '\u{2066}'
            | '\u{2067}' | '\u{2068}' | '\u{2069}' => {
                return Err(Error::ForbiddenCodepoint(u32::try_from(pos).unwrap(), ch));
            }

            // Disallow several characters which are deprecated or discouraged in Unicode.
            //
            // U+149 deprecated; see Unicode 13.0.0, sec. 7.1 Latin, Compatibility Digraphs.
            // U+673 deprecated; see Unicode 13.0.0, sec. 9.2 Arabic, Additional Vowel Marks.
            // U+F77 and U+F79 deprecated; see Unicode 13.0.0, sec. 13.4 Tibetan, Vowels.
            // U+17A3 and U+17A4 deprecated, and U+17B4 and U+17B5 discouraged; see
            // Unicode 13.0.0, sec. 16.4 Khmer, Characters Whose Use Is Discouraged.
            '\u{149}' | '\u{673}' | '\u{f77}' | '\u{f79}' | '\u{17a3}' | '\u{17a4}'
            | '\u{17b4}' | '\u{17b5}' => {
                return Err(Error::DeprecatedCodepoint(u32::try_from(pos).unwrap(), ch));
            }

            // Disallow control codes other than the ones explicitly recognized above,
            // so that viewing a wit file on a terminal doesn't have surprising side
            // effects or appear to have a different meaning than its actual meaning.
            ch if ch.is_control() => {
                return Err(Error::ControlCodepoint(u32::try_from(pos).unwrap(), ch));
            }

            _ => {}
        }
    }

    Ok(())
}

fn is_keylike_start(ch: char) -> bool {
    // Lex any XID start, `_`, or '-'. These aren't all valid identifier chars,
    // but we'll diagnose that after we've lexed the full string.
    unicode_ident::is_xid_start(ch) || ch == '_' || ch == '-'
}

fn is_keylike_continue(ch: char) -> bool {
    // Lex any XID continue (which includes `_`) or '-'.
    unicode_ident::is_xid_continue(ch) || ch == '-'
}

pub fn validate_id(start: u32, id: &str) -> Result<(), Error> {
    // IDs must have at least one part.
    if id.is_empty() {
        return Err(Error::IdPartEmpty(start));
    }

    // Ids consist of parts separated by '-'s.
    for (idx, part) in id.split('-').enumerate() {
        // Parts must be non-empty and contain either all ASCII lowercase or
        // all ASCII uppercase. Non-first segment can also start with a digit.
        let Some(first_char) = part.chars().next() else {
            return Err(Error::IdPartEmpty(start));
        };
        if idx == 0 && !first_char.is_ascii_alphabetic() {
            return Err(Error::InvalidCharInId(start, first_char));
        }
        let mut upper = None;
        for ch in part.chars() {
            if ch.is_ascii_digit() {
                // Digits are accepted in both uppercase and lowercase segments.
            } else if ch.is_ascii_uppercase() {
                if upper.is_none() {
                    upper = Some(true);
                } else if let Some(false) = upper {
                    return Err(Error::InvalidCharInId(start, ch));
                }
            } else if ch.is_ascii_lowercase() {
                if upper.is_none() {
                    upper = Some(false);
                } else if let Some(true) = upper {
                    return Err(Error::InvalidCharInId(start, ch));
                }
            } else {
                return Err(Error::InvalidCharInId(start, ch));
            }
        }
    }

    Ok(())
}

impl Token {
    pub fn describe(&self) -> &'static str {
        match self {
            Whitespace => "whitespace",
            Comment => "a comment",
            Equals => "'='",
            Comma => "','",
            Colon => "':'",
            Period => "'.'",
            Semicolon => "';'",
            LeftParen => "'('",
            RightParen => "')'",
            LeftBrace => "'{'",
            RightBrace => "'}'",
            LessThan => "'<'",
            GreaterThan => "'>'",
            Use => "keyword `use`",
            Type => "keyword `type`",
            Func => "keyword `func`",
            U8 => "keyword `u8`",
            U16 => "keyword `u16`",
            U32 => "keyword `u32`",
            U64 => "keyword `u64`",
            S8 => "keyword `s8`",
            S16 => "keyword `s16`",
            S32 => "keyword `s32`",
            S64 => "keyword `s64`",
            F32 => "keyword `f32`",
            F64 => "keyword `f64`",
            Char => "keyword `char`",
            Own => "keyword `own`",
            Borrow => "keyword `borrow`",
            Resource => "keyword `resource`",
            Record => "keyword `record`",
            Flags => "keyword `flags`",
            Variant => "keyword `variant`",
            Enum => "keyword `enum`",
            Bool => "keyword `bool`",
            String_ => "keyword `string`",
            Option_ => "keyword `option`",
            Result_ => "keyword `result`",
            Future => "keyword `future`",
            Stream => "keyword `stream`",
            ErrorContext => "keyword `error-context`",
            List => "keyword `list`",
            Map => "keyword `map`",
            Underscore => "keyword `_`",
            Id => "an identifier",
            ExplicitId => "an '%' identifier",
            RArrow => "`->`",
            Star => "`*`",
            At => "`@`",
            Slash => "`/`",
            Plus => "`+`",
            Minus => "`-`",
            As => "keyword `as`",
            From_ => "keyword `from`",
            Static => "keyword `static`",
            Interface => "keyword `interface`",
            Tuple => "keyword `tuple`",
            Import => "keyword `import`",
            Export => "keyword `export`",
            World => "keyword `world`",
            Package => "keyword `package`",
            Constructor => "keyword `constructor`",
            Integer => "an integer",
            Include => "keyword `include`",
            With => "keyword `with`",
            Async => "keyword `async`",
            StringLiteral => "a string literal",
        }
    }
}

impl core::error::Error for Error {}

impl Error {
    /// Returns the byte offset in the source map where this error occurred.
    pub fn position(&self) -> u32 {
        match self {
            Error::ControlCodepoint(at, _)
            | Error::DeprecatedCodepoint(at, _)
            | Error::ForbiddenCodepoint(at, _)
            | Error::InvalidCharInId(at, _)
            | Error::IdPartEmpty(at)
            | Error::Unexpected(at, _)
            | Error::UnterminatedComment(at)
            | Error::InvalidUnicodeValue(at, _)
            | Error::InvalidStringElement(at, _)
            | Error::InvalidStringEscape(at, _)
            | Error::WantedChar(at, _)
            | Error::UnexpectedEof(at)
            | Error::InvalidUtf8(at, _)
            | Error::NumberTooBig(at)
            | Error::LoneUnderscore(at)
            | Error::InvalidHexDigit(at, _)
            | Error::Wanted { at, .. } => *at,
        }
    }
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Error::ControlCodepoint(_, ch) => write!(f, "Control code '{}'", ch.escape_unicode()),
            Error::DeprecatedCodepoint(_, ch) => {
                write!(
                    f,
                    "Codepoint {:?} is discouraged by Unicode",
                    ch.escape_unicode()
                )
            }
            Error::ForbiddenCodepoint(_, ch) => {
                write!(
                    f,
                    "Input contains bidirectional override codepoint {:?}",
                    ch.escape_unicode()
                )
            }
            Error::Unexpected(_, ch) => write!(f, "unexpected character {ch:?}"),
            Error::UnterminatedComment(_) => write!(f, "unterminated block comment"),
            Error::Wanted {
                expected, found, ..
            } => write!(f, "expected {expected}, found {found}"),
            Error::InvalidCharInId(_, ch) => write!(f, "invalid character in identifier {ch:?}"),
            Error::IdPartEmpty(_) => write!(f, "identifiers must have characters between '-'s"),
            Error::InvalidUnicodeValue(_, val) => write!(f, "invalid unicode value {val:#x}"),
            Error::InvalidStringElement(_, c) => write!(f, "invalid string character {c:?}"),
            Error::InvalidStringEscape(_, c) => write!(f, "invalid string escape {c:?}"),
            Error::WantedChar(_, c) => write!(f, "expected character {c:?}"),
            Error::UnexpectedEof(_) => write!(f, "unexpected end of file"),
            Error::InvalidUtf8(_, err) => write!(f, "invalid UTF-8: {err}"),
            Error::NumberTooBig(_) => write!(f, "number is too big to fit in a u32"),
            Error::LoneUnderscore(_) => write!(f, "trailing underscore in number"),
            Error::InvalidHexDigit(_, c) => write!(f, "invalid hex digit {c:?}"),
        }
    }
}

#[test]
fn test_validate_id() {
    validate_id(0, "apple").unwrap();
    validate_id(0, "apple-pear").unwrap();
    validate_id(0, "apple-pear-grape").unwrap();
    validate_id(0, "a0").unwrap();
    validate_id(0, "a").unwrap();
    validate_id(0, "a-a").unwrap();
    validate_id(0, "bool").unwrap();
    validate_id(0, "APPLE").unwrap();
    validate_id(0, "APPLE-PEAR").unwrap();
    validate_id(0, "APPLE-PEAR-GRAPE").unwrap();
    validate_id(0, "apple-PEAR-grape").unwrap();
    validate_id(0, "APPLE-pear-GRAPE").unwrap();
    validate_id(0, "ENOENT").unwrap();
    validate_id(0, "is-XML").unwrap();
    validate_id(0, "apple-0").unwrap();
    validate_id(0, "a0-000-3d4a-54FF").unwrap();

    assert!(validate_id(0, "").is_err());
    assert!(validate_id(0, "0").is_err());
    assert!(validate_id(0, "%").is_err());
    assert!(validate_id(0, "$").is_err());
    assert!(validate_id(0, "0a").is_err());
    assert!(validate_id(0, ".").is_err());
    assert!(validate_id(0, "·").is_err());
    assert!(validate_id(0, "a a").is_err());
    assert!(validate_id(0, "_").is_err());
    assert!(validate_id(0, "-").is_err());
    assert!(validate_id(0, "a-").is_err());
    assert!(validate_id(0, "-a").is_err());
    assert!(validate_id(0, "Apple").is_err());
    assert!(validate_id(0, "applE").is_err());
    assert!(validate_id(0, "-apple-pear").is_err());
    assert!(validate_id(0, "apple-pear-").is_err());
    assert!(validate_id(0, "apple_pear").is_err());
    assert!(validate_id(0, "apple.pear").is_err());
    assert!(validate_id(0, "apple pear").is_err());
    assert!(validate_id(0, "apple/pear").is_err());
    assert!(validate_id(0, "apple|pear").is_err());
    assert!(validate_id(0, "apple-Pear").is_err());
    assert!(validate_id(0, "()()").is_err());
    assert!(validate_id(0, "").is_err());
    assert!(validate_id(0, "*").is_err());
    assert!(validate_id(0, "apple\u{5f3}pear").is_err());
    assert!(validate_id(0, "apple\u{200c}pear").is_err());
    assert!(validate_id(0, "apple\u{200d}pear").is_err());
    assert!(validate_id(0, "apple--pear").is_err());
    assert!(validate_id(0, "_apple").is_err());
    assert!(validate_id(0, "apple_").is_err());
    assert!(validate_id(0, "_Znwj").is_err());
    assert!(validate_id(0, "__i386").is_err());
    assert!(validate_id(0, "__i386__").is_err());
    assert!(validate_id(0, "Москва").is_err());
    assert!(validate_id(0, "garçon-hühnervögel-Москва-東京").is_err());
    assert!(validate_id(0, "a0-000-3d4A-54Ff").is_err());
    assert!(validate_id(0, "😼").is_err(), "non-identifier");
    assert!(validate_id(0, "\u{212b}").is_err(), "non-ascii");
}

#[test]
fn test_tokenizer() {
    fn collect(s: &str) -> Result<Vec<Token>, Error> {
        let mut t = Tokenizer::new(s, 0)?;
        let mut tokens = Vec::new();
        while let Some(token) = t.next()? {
            tokens.push(token.1);
        }
        Ok(tokens)
    }

    assert_eq!(collect("").unwrap(), vec![]);
    assert_eq!(collect("_").unwrap(), vec![Token::Underscore]);
    assert_eq!(collect("apple").unwrap(), vec![Token::Id]);
    assert_eq!(collect("apple-pear").unwrap(), vec![Token::Id]);
    assert_eq!(collect("apple--pear").unwrap(), vec![Token::Id]);
    assert_eq!(collect("apple-Pear").unwrap(), vec![Token::Id]);
    assert_eq!(collect("apple-pear-grape").unwrap(), vec![Token::Id]);
    assert_eq!(collect("apple pear").unwrap(), vec![Token::Id, Token::Id]);
    assert_eq!(collect("_a_p_p_l_e_").unwrap(), vec![Token::Id]);
    assert_eq!(collect("garçon").unwrap(), vec![Token::Id]);
    assert_eq!(collect("hühnervögel").unwrap(), vec![Token::Id]);
    assert_eq!(collect("москва").unwrap(), vec![Token::Id]);
    assert_eq!(collect("東京").unwrap(), vec![Token::Id]);
    assert_eq!(
        collect("garçon-hühnervögel-москва-東京").unwrap(),
        vec![Token::Id]
    );
    assert_eq!(collect("a0").unwrap(), vec![Token::Id]);
    assert_eq!(collect("a").unwrap(), vec![Token::Id]);
    assert_eq!(collect("%a").unwrap(), vec![Token::ExplicitId]);
    assert_eq!(collect("%a-a").unwrap(), vec![Token::ExplicitId]);
    assert_eq!(collect("%bool").unwrap(), vec![Token::ExplicitId]);
    assert_eq!(collect("%").unwrap(), vec![Token::ExplicitId]);
    assert_eq!(collect("APPLE").unwrap(), vec![Token::Id]);
    assert_eq!(collect("APPLE-PEAR").unwrap(), vec![Token::Id]);
    assert_eq!(collect("APPLE-PEAR-GRAPE").unwrap(), vec![Token::Id]);
    assert_eq!(collect("apple-PEAR-grape").unwrap(), vec![Token::Id]);
    assert_eq!(collect("APPLE-pear-GRAPE").unwrap(), vec![Token::Id]);
    assert_eq!(collect("ENOENT").unwrap(), vec![Token::Id]);
    assert_eq!(collect("is-XML").unwrap(), vec![Token::Id]);

    assert_eq!(collect("func").unwrap(), vec![Token::Func]);
    assert_eq!(
        collect("a: func()").unwrap(),
        vec![
            Token::Id,
            Token::Colon,
            Token::Func,
            Token::LeftParen,
            Token::RightParen
        ]
    );

    assert_eq!(collect("resource").unwrap(), vec![Token::Resource]);

    assert_eq!(collect("own").unwrap(), vec![Token::Own]);
    assert_eq!(
        collect("own<some-id>").unwrap(),
        vec![Token::Own, Token::LessThan, Token::Id, Token::GreaterThan]
    );

    assert_eq!(collect("borrow").unwrap(), vec![Token::Borrow]);
    assert_eq!(
        collect("borrow<some-id>").unwrap(),
        vec![
            Token::Borrow,
            Token::LessThan,
            Token::Id,
            Token::GreaterThan
        ]
    );

    assert!(collect("\u{149}").is_err(), "strongly discouraged");
    assert!(collect("\u{673}").is_err(), "strongly discouraged");
    assert!(collect("\u{17a3}").is_err(), "strongly discouraged");
    assert!(collect("\u{17a4}").is_err(), "strongly discouraged");
    assert!(collect("\u{202a}").is_err(), "bidirectional override");
    assert!(collect("\u{2068}").is_err(), "bidirectional override");
    assert!(collect("\u{0}").is_err(), "control code");
    assert!(collect("\u{b}").is_err(), "control code");
    assert!(collect("\u{c}").is_err(), "control code");
    assert!(collect("\u{85}").is_err(), "control code");
}

#[test]
fn test_strings() {
    #[track_caller]
    fn test(s: &str, expected: Result<&str, Error>) {
        let actual = (|| {
            let mut t = Tokenizer::new(s, 0)?;
            let next = t.next()?;
            assert!(
                matches!(next, Some((_, Token::StringLiteral))),
                "{s:?} didn't tokenize as a string"
            );
            assert!(t.next()?.is_none(), "extra tokens after string: {s:?}");
            let (span, _) = next.unwrap();
            t.string_literal(span)
        })();
        match (&actual, &expected) {
            (Ok(actual), Ok(expected)) => assert_eq!(actual, expected),
            (Err(actual), Err(expected)) => assert_eq!(actual, expected),
            (Ok(_) | Err(_), _) => panic!("expected error {expected:?}, but got Ok({actual:?})"),
        }
    }

    // smoke test
    test("\"\"", Ok(""));
    test("\"a\"", Ok("a"));
    test("\"a b c\"", Ok("a b c"));

    // single-char-escapes
    test("\"\\\"\"", Ok("\""));
    test("\"\\t\"", Ok("\t"));
    test("\"\\n\"", Ok("\n"));
    test("\"\\r\"", Ok("\r"));
    test("\"\\\\\"", Ok("\\"));
    test("\"\\h\"", Err(Error::InvalidStringEscape(2, 'h')));

    // double-hex-digit
    test("\"\\00\"", Ok("\0"));
    test("\"\\01\"", Ok("\x01"));
    test("\"\\0f\"", Ok("\x0f"));
    test("\"\\0_1\"", Err(Error::InvalidHexDigit(3, '_')));
    test("\"\\0g\"", Err(Error::InvalidHexDigit(3, 'g')));
    #[allow(invalid_from_utf8)]
    test(
        "\"\\ff\"",
        Err(Error::InvalidUtf8(
            0,
            core::str::from_utf8(&[0xff]).unwrap_err(),
        )),
    );

    // unicode escape
    test("\"\\u{0}\"", Ok("\0"));
    test("\"\\u{1}\"", Ok("\x01"));
    test("\"\\u{1_2_3_f}\"", Ok("\u{123f}"));
    test("\"\\u0000\"", Err(Error::WantedChar(3, '{')));
    test("\"\\u{0h\"", Err(Error::WantedChar(5, '}')));
    test("\"\\u{1_}\"", Err(Error::LoneUnderscore(4)));
    test(
        "\"\\u{fffffffffffffffffffff}\"",
        Err(Error::NumberTooBig(12)),
    );
    test(
        "\"\\u{ffff_ffff}\"",
        Err(Error::InvalidUnicodeValue(2, u32::MAX)),
    );

    test("\"\t\"", Err(Error::InvalidStringElement(1, '\t')));
}
