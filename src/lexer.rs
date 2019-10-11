use std::borrow::Cow;
use std::char;
use std::convert::TryFrom;
use std::iter;
use std::str;

pub struct Lexer<'a> {
    it: iter::Peekable<str::CharIndices<'a>>,
    input: &'a str,
}

#[derive(Debug, PartialEq)]
pub enum Source<'a> {
    Comment(Comment<'a>),
    Whitespace(&'a str),
    Token(Token<'a>),
}

#[derive(Debug, PartialEq)]
pub enum Token<'a> {
    LParen(&'a str),
    RParen(&'a str),
    String { val: Cow<'a, [u8]>, src: &'a str },
    Id(&'a str),
    Keyword(&'a str),
    Reserved(&'a str),
    Integer(Integer<'a>),
    Float(Float<'a>),
}

#[derive(Debug, PartialEq)]
pub enum Comment<'a> {
    Line(&'a str),
    Block(&'a str),
}

#[derive(Debug)]
pub struct LexError {
    inner: Box<LexErrorInner>,
}

#[derive(Debug)]
struct LexErrorInner {
    line: usize,
    col: usize,
    pos: usize,
    kind: LexErrorKind,
}

#[derive(Debug, PartialEq)]
pub enum LexErrorKind {
    DanglingBlockComment,
    Unexpected(char),
    InvalidStringElement(char),
    InvalidStringEscape(char),
    InvalidHexDigit(char),
    InvalidDigit(char),
    Expected { wanted: char, found: char },
    UnexpectedEof,
    NumberTooBig,
    InvalidUnicodeValue(u32),
    InvalidIdchar(char),
    LoneUnderscore,
}

#[derive(Debug, PartialEq)]
pub struct Integer<'a> {
    negative: bool,
    src: &'a str,
    val: u64,
}

#[derive(Debug, PartialEq)]
pub struct Float<'a> {
    negative: bool,
    src: &'a str,
    val: FloatVal,
}

#[derive(Debug, PartialEq)]
enum FloatVal {
    Nan(Option<u64>),
    Inf,
    Val,
}

impl<'a> Lexer<'a> {
    pub fn new(input: &str) -> Lexer<'_> {
        Lexer {
            it: input.char_indices().peekable(),
            input,
        }
    }

    pub fn next(&mut self) -> Result<Option<Source<'a>>, LexError> {
        if let Some(ws) = self.ws() {
            return Ok(Some(Source::Whitespace(ws)));
        }
        if let Some(comment) = self.comment()? {
            return Ok(Some(Source::Comment(comment)));
        }
        if let Some(token) = self.token()? {
            return Ok(Some(Source::Token(token)));
        }
        match self.it.next() {
            Some((i, ch)) => Err(self.error(i, LexErrorKind::Unexpected(ch))),
            None => Ok(None),
        }
    }

    fn token(&mut self) -> Result<Option<Token<'a>>, LexError> {
        if let Some(pos) = self.eat_char('(') {
            return Ok(Some(Token::LParen(&self.input[pos..pos + 1])));
        }

        if let Some(pos) = self.eat_char(')') {
            return Ok(Some(Token::RParen(&self.input[pos..pos + 1])));
        }

        if let Some(pos) = self.eat_char('"') {
            let val = self.string()?;
            let src = &self.input[pos..self.cur()];
            return Ok(Some(Token::String { val, src }));
        }

        let (sign_start, negative) = if let Some(i) = self.eat_char('-') {
            (Some(i), true)
        } else if let Some(i) = self.eat_char('+') {
            (Some(i), false)
        } else {
            (None, false)
        };

        if let Some(n) = self.number(sign_start, negative)? {
            return Ok(Some(n));
        }

        let start = match sign_start {
            Some(i) => i,
            None => match self.it.peek().cloned() {
                Some((i, ch)) if is_idchar(ch) => i,
                Some((i, ch)) => return Err(self.error(i, LexErrorKind::Unexpected(ch))),
                None => return Ok(None),
            },
        };

        while let Some((_, ch)) = self.it.peek().cloned() {
            if is_idchar(ch) {
                self.it.next();
            } else {
                break;
            }
        }

        // Handle `inf` and `nan` which are special numbers here
        let src = &self.input[start..self.cur()];
        let num = if sign_start.is_some() {
            &src[1..]
        } else {
            src
        };

        if num == "inf" {
            return Ok(Some(Token::Float(Float {
                negative,
                src,
                val: FloatVal::Inf,
            })));
        }
        if num == "nan" {
            return Ok(Some(Token::Float(Float {
                negative,
                src,
                val: FloatVal::Nan(None),
            })));
        }
        if num.starts_with("nan:0x") {
            if let Ok(n) = u64::from_str_radix(&num[6..], 16) {
                return Ok(Some(Token::Float(Float {
                    negative,
                    src,
                    val: FloatVal::Nan(Some(n)),
                })));
            }
        }

        let ch = src.chars().next().unwrap();
        if ch == '$' && src.len() > 1 {
            Ok(Some(Token::Id(src)))
        } else if 'a' <= ch && ch <= 'z' {
            Ok(Some(Token::Keyword(src)))
        } else {
            Ok(Some(Token::Reserved(src)))
        }
    }

    fn number(&mut self, sign_start: Option<usize>, negative: bool) -> Result<Option<Token<'a>>, LexError> {
        let start = sign_start.unwrap_or(self.cur());

        // Make sure the next digit is an ascii digit, otherwise this isn't a
        // number but it's probably an identifier
        match self.it.peek() {
            Some((_, c)) if c.is_ascii_digit() => {},
            Some(_) | None => return Ok(None),
        }

        let (val, hex) = if self.eat_str("0x").is_some() {
            (self.hexnum()?.1, true)
        } else {
            (self.num()?.1, false)
        };

        // If there's a fractional part, parse that but don't record the value
        // since we defer float parsing until much later.
        let frac = self.eat_char('.').is_some();
        if frac {
            if let Some((_, ch)) = self.it.peek() {
                if hex {
                    if ch.is_ascii_hexdigit() {
                        self.hexnum()?;
                    }
                } else {
                    if ch.is_ascii_digit() {
                        self.num()?;
                    }
                }
            }
        }

        // Figure out if there's an exponential part here to make a float, and
        // if so parse it but defer its actual calculation until later.
        let exp = if hex {
            self.eat_char('p').is_some() || self.eat_char('P').is_some()
        } else {
            self.eat_char('e').is_some() || self.eat_char('E').is_some()
        };
        if exp {
            // chew a sign if it's there, again we'll parse it later if need be.
            if self.eat_char('-').is_none() {
                drop(self.eat_char('+'));
            }
            self.num()?;
        }

        if frac || exp {
            return Ok(Some(Token::Float(Float {
                negative,
                src: &self.input[start..self.cur()],
                val: FloatVal::Val,
            })));
        } else {
            return Ok(Some(Token::Integer(Integer {
                negative,
                src: &self.input[start..self.cur()],
                val,
            })));
        }
    }

    /// Attempts to consume whitespace from the input stream, returning `None`
    /// if there's no whitespace to consume
    fn ws(&mut self) -> Option<&'a str> {
        let start = self.cur();
        loop {
            match self.it.peek() {
                Some((_, ' ')) | Some((_, '\n')) | Some((_, '\r')) | Some((_, '\t')) => {
                    drop(self.it.next())
                }
                _ => break,
            }
        }
        let end = self.cur();
        if start != end {
            Some(&self.input[start..end])
        } else {
            None
        }
    }

    /// Attempts to read a comment from the input stream
    fn comment(&mut self) -> Result<Option<Comment<'a>>, LexError> {
        if let Some(start) = self.eat_str(";;") {
            loop {
                match self.it.peek() {
                    None | Some((_, '\n')) => break,
                    _ => drop(self.it.next()),
                }
            }
            let end = self.cur();
            return Ok(Some(Comment::Line(&self.input[start..end])));
        }
        if let Some(start) = self.eat_str("(;") {
            let mut level = 1;
            while let Some((_, ch)) = self.it.next() {
                if ch == '(' && self.eat_char(';').is_some() {
                    level += 1;
                }
                if ch == ';' && self.eat_char(')').is_some() {
                    level -= 1;
                    if level == 0 {
                        let end = self.cur();
                        return Ok(Some(Comment::Block(&self.input[start..end])));
                    }
                }
            }

            return Err(self.error(start, LexErrorKind::DanglingBlockComment));
        }
        Ok(None)
    }

    /// Reads everything for a literal string except the leading `"`. Returns
    /// the string value that has been read.
    fn string(&mut self) -> Result<Cow<'a, [u8]>, LexError> {
        enum State {
            Start(usize),
            String(Vec<u8>),
        }
        let mut state = State::Start(self.cur());
        loop {
            match self.it.next() {
                Some((i, '\\')) => {
                    match state {
                        State::String(_) => {}
                        State::Start(start) => {
                            state = State::String(self.input[start..i].as_bytes().to_vec());
                        }
                    }
                    let buf = match &mut state {
                        State::String(b) => b,
                        State::Start(_) => unreachable!(),
                    };
                    match self.it.next() {
                        Some((_, '"')) => buf.push(b'"'),
                        Some((_, '\'')) => buf.push(b'\''),
                        Some((_, 't')) => buf.push(b'\t'),
                        Some((_, 'n')) => buf.push(b'\n'),
                        Some((_, 'r')) => buf.push(b'\r'),
                        Some((_, '\\')) => buf.push(b'\\'),
                        Some((i, 'u')) => {
                            self.must_eat_char('{')?;
                            let (_, n) = self.hexnum()?;
                            let n = u32::try_from(n)
                                .map_err(|_| self.error(i, LexErrorKind::NumberTooBig))?;
                            let c = char::from_u32(n).ok_or_else(|| {
                                self.error(i, LexErrorKind::InvalidUnicodeValue(n))
                            })?;
                            buf.extend(c.encode_utf8(&mut [0; 4]).as_bytes());
                            self.must_eat_char('}')?;
                        }
                        Some((_, c1)) if c1.is_ascii_hexdigit() => {
                            let (_, c2) = self.hexdigit()?;
                            buf.push(to_hex(c1) * 16 + c2);
                        }
                        Some((i, c)) => {
                            return Err(self.error(i, LexErrorKind::InvalidStringEscape(c)))
                        }
                        None => {
                            return Err(self.error(self.input.len(), LexErrorKind::UnexpectedEof))
                        }
                    }
                }
                Some((_, '"')) => break,
                Some((i, c)) => {
                    if (c as u32) < 0x20 || c as u32 == 0x7f {
                        return Err(self.error(i, LexErrorKind::InvalidStringElement(c)));
                    }
                    match &mut state {
                        State::Start(_) => {}
                        State::String(v) => {
                            v.extend(c.encode_utf8(&mut [0; 4]).as_bytes());
                        }
                    }
                }
                None => return Err(self.error(self.input.len(), LexErrorKind::UnexpectedEof)),
            }
        }
        match state {
            State::Start(pos) => Ok(self.input[pos..self.cur() - 1].as_bytes().into()),
            State::String(s) => Ok(s.into()),
        }
    }

    /// Reads a hexadecimal number from the input string, returning the textual
    /// representation as well as the parsed number.
    fn hexnum(&mut self) -> Result<(&'a str, u64), LexError> {
        let (start, n) = self.hexdigit()?;
        let mut n = n as u64;
        let mut last_underscore = false;
        while let Some((_, c)) = self.it.peek().cloned() {
            if c == '_' {
                self.it.next();
                last_underscore = true;
                continue;
            }
            if !c.is_ascii_hexdigit() {
                break;
            }
            last_underscore = false;
            self.it.next();
            n = n
                .checked_mul(16)
                .and_then(|n| n.checked_add(to_hex(c) as u64))
                .ok_or_else(|| self.error(start, LexErrorKind::NumberTooBig))?;
        }
        let end = self.cur();
        if last_underscore {
            return Err(self.error(end, LexErrorKind::LoneUnderscore));
        }
        Ok((&self.input[start..end], n))
    }

    /// Reads an integer number from the input string, returning the textual
    /// representation as well as the parsed number.
    fn num(&mut self) -> Result<(&'a str, u64), LexError> {
        let (start, n) = self.digit()?;
        let mut n = n as u64;
        let mut last_underscore = false;
        while let Some((_, c)) = self.it.peek().cloned() {
            if c == '_' {
                self.it.next();
                last_underscore = true;
                continue;
            }
            if !c.is_ascii_digit() {
                break;
            }
            last_underscore = false;
            self.it.next();
            n = n
                .checked_mul(10)
                .and_then(|n| n.checked_add(c as u64 - '0' as u64))
                .ok_or_else(|| self.error(start, LexErrorKind::NumberTooBig))?;
        }
        let end = self.cur();
        if last_underscore {
            return Err(self.error(end, LexErrorKind::LoneUnderscore));
        }
        Ok((&self.input[start..end], n))
    }

    /// Reads a hexidecimal digit from the input stream, returning where it's
    /// defined and the hex value. Returns an error on EOF or an invalid hex
    /// digit.
    fn hexdigit(&mut self) -> Result<(usize, u8), LexError> {
        let (i, ch) = self.must_char()?;
        if ch.is_ascii_hexdigit() {
            Ok((i, to_hex(ch)))
        } else {
            Err(self.error(i, LexErrorKind::InvalidHexDigit(ch)))
        }
    }

    /// Reads a digit from the input stream, returning where it's
    /// defined and the hex value. Returns an error on EOF or an invalid hex
    /// digit.
    fn digit(&mut self) -> Result<(usize, u8), LexError> {
        let (i, ch) = self.must_char()?;
        if ch.is_ascii_digit() {
            Ok((i, ch as u8 - b'0'))
        } else {
            Err(self.error(i, LexErrorKind::InvalidDigit(ch)))
        }
    }

    /// Returns where the match started, if any
    fn eat_str(&mut self, s: &str) -> Option<usize> {
        if !self.cur_str().starts_with(s) {
            return None;
        }
        let ret = self.cur();
        for _ in s.chars() {
            self.it.next();
        }
        Some(ret)
    }

    /// Returns where the match happened, if any
    fn eat_char(&mut self, needle: char) -> Option<usize> {
        match self.it.peek() {
            Some((i, c)) if *c == needle => {
                let ret = *i;
                self.it.next();
                Some(ret)
            }
            _ => None,
        }
    }

    /// Reads the next character from the input string and where it's located,
    /// returning an error if the input stream is empty.
    fn must_char(&mut self) -> Result<(usize, char), LexError> {
        self.it
            .next()
            .ok_or_else(|| self.error(self.input.len(), LexErrorKind::UnexpectedEof))
    }

    /// Expects that a specific character must be read next
    fn must_eat_char(&mut self, wanted: char) -> Result<usize, LexError> {
        let (pos, found) = self.must_char()?;
        if wanted == found {
            Ok(pos)
        } else {
            Err(self.error(pos, LexErrorKind::Expected { wanted, found }))
        }
    }

    /// Returns the current position of our iterator through the input string
    fn cur(&mut self) -> usize {
        self.it.peek().map(|p| p.0).unwrap_or(self.input.len())
    }

    /// Returns the remaining string that we have left to parse
    fn cur_str(&mut self) -> &'a str {
        &self.input[self.cur()..]
    }

    /// Creates an error at `pos` with the specified `kind`
    fn error(&self, pos: usize, kind: LexErrorKind) -> LexError {
        let (line, col) = self.to_linecol(pos);
        LexError {
            inner: Box::new(LexErrorInner {
                line,
                col,
                pos,
                kind,
            }),
        }
    }

    /// Converts an offset within `self.input` to a line/column number
    fn to_linecol(&self, offset: usize) -> (usize, usize) {
        let mut cur = 0;
        // Use split_terminator instead of lines so that if there is a `\r`,
        // it is included in the offset calculation. The `+1` values below
        // account for the `\n`.
        for (i, line) in self.input.split_terminator('\n').enumerate() {
            if cur + line.len() + 1 > offset {
                return (i, offset - cur);
            }
            cur += line.len() + 1;
        }
        (self.input.lines().count(), 0)
    }
}

impl LexError {
    pub fn kind(&self) -> &LexErrorKind {
        &self.inner.kind
    }
}

fn to_hex(c: char) -> u8 {
    match c {
        'a'..='f' => c as u8 - b'a' + 10,
        'A'..='F' => c as u8 - b'A' + 10,
        _ => c as u8 - b'0',
    }
}

fn is_idchar(c: char) -> bool {
    match c {
        '0'..='9'
        | 'a'..='z'
        | 'A'..='Z'
        | '!'
        | '#'
        | '$'
        | '%'
        | '&'
        | '\''
        | '*'
        | '+'
        | '-'
        | '.'
        | '/'
        | ':'
        | '<'
        | '='
        | '>'
        | '?'
        | '@'
        | '\\'
        | '^'
        | '_'
        | '`'
        | '|'
        | '~' => true,
        _ => false,
    }
}

impl Integer<'_> {
    pub fn get_i64(&self) -> Option<i64> {
        let multiplier = if self.negative { -1 } else { 1i64 };
        Some(multiplier.checked_mul(i64::try_from(self.val).ok()?)?)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn ws_smoke() {
        fn get_whitespace(input: &str) -> &str {
            match Lexer::new(input).next().expect("no first token") {
                Some(Source::Whitespace(s)) => s,
                other => panic!("unexpected {:?}", other),
            }
        }
        assert_eq!(get_whitespace(" "), " ");
        assert_eq!(get_whitespace("  "), "  ");
        assert_eq!(get_whitespace("  \n "), "  \n ");
        assert_eq!(get_whitespace("  x"), "  ");
        assert_eq!(get_whitespace("  ;"), "  ");
    }

    #[test]
    fn line_comment_smoke() {
        fn get_line_comment(input: &str) -> &str {
            match Lexer::new(input).next().expect("no first token") {
                Some(Source::Comment(Comment::Line(s))) => s,
                other => panic!("unexpected {:?}", other),
            }
        }
        assert_eq!(get_line_comment(";;"), ";;");
        assert_eq!(get_line_comment(";; xyz"), ";; xyz");
        assert_eq!(get_line_comment(";; xyz\nabc"), ";; xyz");
        assert_eq!(get_line_comment(";;\nabc"), ";;");
        assert_eq!(get_line_comment(";;   \nabc"), ";;   ");
    }

    #[test]
    fn block_comment_smoke() {
        fn get_block_comment(input: &str) -> &str {
            match Lexer::new(input).next().expect("no first token") {
                Some(Source::Comment(Comment::Block(s))) => s,
                other => panic!("unexpected {:?}", other),
            }
        }
        assert_eq!(get_block_comment("(;;)"), "(;;)");
        assert_eq!(get_block_comment("(; ;)"), "(; ;)");
        assert_eq!(get_block_comment("(; (;;) ;)"), "(; (;;) ;)");
        assert_eq!(
            *Lexer::new("(; ").next().unwrap_err().kind(),
            LexErrorKind::DanglingBlockComment,
        );
        assert_eq!(
            *Lexer::new("(; (;;)").next().unwrap_err().kind(),
            LexErrorKind::DanglingBlockComment,
        );
        assert_eq!(
            *Lexer::new("(; ;").next().unwrap_err().kind(),
            LexErrorKind::DanglingBlockComment,
        );
    }

    fn get_token(input: &str) -> Token<'_> {
        match Lexer::new(input).next().expect("no first token") {
            Some(Source::Token(t)) => t,
            other => panic!("unexpected {:?}", other),
        }
    }

    #[test]
    fn lparen() {
        assert_eq!(get_token("(("), Token::LParen("("));
    }

    #[test]
    fn rparen() {
        assert_eq!(get_token(")("), Token::RParen(")"));
    }

    #[test]
    fn strings() {
        fn get_string(input: &str) -> Cow<'_, [u8]> {
            match get_token(input) {
                Token::String { val, src } => {
                    assert_eq!(input, src);
                    val
                }
                other => panic!("not string {:?}", other),
            }
        }
        assert_eq!(&*get_string("\"\""), b"");
        assert_eq!(&*get_string("\"a\""), b"a");
        assert_eq!(&*get_string("\"a b c d\""), b"a b c d");
        assert_eq!(&*get_string("\"\\\"\""), b"\"");
        assert_eq!(&*get_string("\"\\'\""), b"'");
        assert_eq!(&*get_string("\"\\n\""), b"\n");
        assert_eq!(&*get_string("\"\\t\""), b"\t");
        assert_eq!(&*get_string("\"\\r\""), b"\r");
        assert_eq!(&*get_string("\"\\\\\""), b"\\");
        assert_eq!(&*get_string("\"\\01\""), &[1]);
        assert_eq!(&*get_string("\"\\u{1}\""), &[1]);
        assert_eq!(
            &*get_string("\"\\u{0f3}\""),
            '\u{0f3}'.encode_utf8(&mut [0; 4]).as_bytes()
        );
        assert_eq!(
            &*get_string("\"\\u{0_f_3}\""),
            '\u{0f3}'.encode_utf8(&mut [0; 4]).as_bytes()
        );

        for i in 0..=255i32 {
            let s = format!("\"\\{:02x}\"", i);
            assert_eq!(&*get_string(&s), &[i as u8]);
        }

        assert_eq!(
            *Lexer::new("\"").next().unwrap_err().kind(),
            LexErrorKind::UnexpectedEof,
        );
        assert_eq!(
            *Lexer::new("\"\\x\"").next().unwrap_err().kind(),
            LexErrorKind::InvalidStringEscape('x'),
        );
        assert_eq!(
            *Lexer::new("\"\\0\"").next().unwrap_err().kind(),
            LexErrorKind::InvalidHexDigit('"'),
        );
        assert_eq!(
            *Lexer::new("\"\\0").next().unwrap_err().kind(),
            LexErrorKind::UnexpectedEof,
        );
        assert_eq!(
            *Lexer::new("\"\\").next().unwrap_err().kind(),
            LexErrorKind::UnexpectedEof,
        );
        assert_eq!(
            *Lexer::new("\"\u{7f}\"").next().unwrap_err().kind(),
            LexErrorKind::InvalidStringElement('\u{7f}'),
        );
        assert_eq!(
            *Lexer::new("\"\u{0}\"").next().unwrap_err().kind(),
            LexErrorKind::InvalidStringElement('\u{0}'),
        );
        assert_eq!(
            *Lexer::new("\"\u{1f}\"").next().unwrap_err().kind(),
            LexErrorKind::InvalidStringElement('\u{1f}'),
        );
        assert_eq!(
            *Lexer::new("\"\\u{x}\"").next().unwrap_err().kind(),
            LexErrorKind::InvalidHexDigit('x'),
        );
        assert_eq!(
            *Lexer::new("\"\\u{1_}\"").next().unwrap_err().kind(),
            LexErrorKind::LoneUnderscore,
        );
        assert_eq!(
            *Lexer::new("\"\\u{fffffffffffffffff}\"")
                .next()
                .unwrap_err()
                .kind(),
            LexErrorKind::NumberTooBig,
        );
        assert_eq!(
            *Lexer::new("\"\\u{ffffffff}\"").next().unwrap_err().kind(),
            LexErrorKind::InvalidUnicodeValue(0xffffffff),
        );
        assert_eq!(
            *Lexer::new("\"\\u\"").next().unwrap_err().kind(),
            LexErrorKind::Expected {
                wanted: '{',
                found: '"'
            },
        );
        assert_eq!(
            *Lexer::new("\"\\u{\"").next().unwrap_err().kind(),
            LexErrorKind::InvalidHexDigit('"'),
        );
        assert_eq!(
            *Lexer::new("\"\\u{1\"").next().unwrap_err().kind(),
            LexErrorKind::Expected {
                wanted: '}',
                found: '"'
            },
        );
        assert_eq!(
            *Lexer::new("\"\\u{1").next().unwrap_err().kind(),
            LexErrorKind::UnexpectedEof,
        );
    }

    #[test]
    fn id() {
        fn get_id(input: &str) -> &str {
            match get_token(input) {
                Token::Id(s) => s,
                other => panic!("not id {:?}", other),
            }
        }
        assert_eq!(get_id("$x"), "$x");
        assert_eq!(get_id("$xyz"), "$xyz");
        assert_eq!(get_id("$x_z"), "$x_z");
        assert_eq!(get_id("$0^"), "$0^");
        assert_eq!(get_id("$0^;;"), "$0^");
        assert_eq!(get_id("$0^ ;;"), "$0^");
    }

    #[test]
    fn keyword() {
        fn get_keyword(input: &str) -> &str {
            match get_token(input) {
                Token::Keyword(s) => s,
                other => panic!("not id {:?}", other),
            }
        }
        assert_eq!(get_keyword("x"), "x");
        assert_eq!(get_keyword("xyz"), "xyz");
        assert_eq!(get_keyword("x_z"), "x_z");
        assert_eq!(get_keyword("x_z "), "x_z");
        assert_eq!(get_keyword("x_z "), "x_z");
    }

    #[test]
    fn reserved() {
        fn get_reserved(input: &str) -> &str {
            match get_token(input) {
                Token::Reserved(s) => s,
                other => panic!("not reserved {:?}", other),
            }
        }
        assert_eq!(get_reserved("$ "), "$");
        assert_eq!(get_reserved("^_x "), "^_x");
    }

    #[test]
    fn integer() {
        fn get_integer(input: &str) -> i64 {
            match get_token(input) {
                Token::Integer(i) => i.get_i64().expect("didn't fit in i64"),
                other => panic!("not reserved {:?}", other),
            }
        }
        assert_eq!(get_integer("1"), 1);
        assert_eq!(get_integer("1 "), 1);
        assert_eq!(get_integer("0 "), 0);
        assert_eq!(get_integer("-1 "), -1);
        assert_eq!(get_integer("+1 "), 1);
        assert_eq!(get_integer("+1_000 "), 1_000);
        assert_eq!(get_integer("+1_0______0_0 "), 1_000);
        assert_eq!(get_integer("+0x10 "), 0x10);
        assert_eq!(get_integer("-0x10 "), -0x10);
        assert_eq!(get_integer("0x10 "), 0x10);

        assert_eq!(
            *Lexer::new("1_").next().unwrap_err().kind(),
            LexErrorKind::LoneUnderscore,
        );
        assert_eq!(
            *Lexer::new("0x ").next().unwrap_err().kind(),
            LexErrorKind::InvalidHexDigit(' '),
        );
        assert_eq!(
            *Lexer::new("0x").next().unwrap_err().kind(),
            LexErrorKind::UnexpectedEof,
        );
        assert_eq!(
            *Lexer::new("0xx").next().unwrap_err().kind(),
            LexErrorKind::InvalidHexDigit('x'),
        );
        assert_eq!(
            *Lexer::new("9999999999999999999999999")
                .next()
                .unwrap_err()
                .kind(),
            LexErrorKind::NumberTooBig,
        );
    }

    #[test]
    fn float() {
        fn get_float(input: &str) -> Float<'_> {
            match get_token(input) {
                Token::Float(i) => i,
                other => panic!("not reserved {:?}", other),
            }
        }
        assert_eq!(get_float("nan").val, FloatVal::Nan(None));
        assert_eq!(get_float("-nan").val, FloatVal::Nan(None));
        assert!(get_float("-nan").negative);
        assert_eq!(get_float("+nan").val, FloatVal::Nan(None));
        assert!(!get_float("+nan").negative);
        assert!(!get_float("nan").negative);
        assert_eq!(get_float("+nan:0x2").val, FloatVal::Nan(Some(2)));
        assert_eq!(get_float("inf").val, FloatVal::Inf);
        assert_eq!(get_float("-inf").val, FloatVal::Inf);
        assert!(get_float("-inf").negative);
        assert_eq!(get_float("+inf").val, FloatVal::Inf);
        assert!(!get_float("inf").negative);
        assert!(!get_float("+inf").negative);

        assert_eq!(get_float("1.1").src, "1.1");
        assert_eq!(get_float("1.1e0").src, "1.1e0");
        assert_eq!(get_float("1.1e0^").src, "1.1e0");
        assert_eq!(get_float("1_2.1_1e0_1^").src, "1_2.1_1e0_1");
        assert_eq!(get_float("1_2.1_1E0_1^").src, "1_2.1_1E0_1");
        assert_eq!(get_float("0xf_f.f_fp1_0^").src, "0xf_f.f_fp1_0");
        assert_eq!(get_float("0xf_f.f_fP1_0^").src, "0xf_f.f_fP1_0");
        assert_eq!(get_float("1.").src, "1.");
        assert_eq!(get_float("1. ").src, "1.");
    }
}
