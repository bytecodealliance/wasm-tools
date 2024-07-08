use std::{borrow::Cow, str::Split};

use crate::parser::{ParserError, ParserErrorKind};

/// An iterator over parsed string "parts".
pub enum StringPartsIter<'a> {
    Normal(StringParts<'a>),
    Multiline(MultilineStringParts<'a>),
}

impl<'a> StringPartsIter<'a> {
    /// Returns an iterator over string parts for the given substring containing
    /// only the _inner_ contents (between quotes) of a single-line string.
    /// The given pos is the source position of this substring for error reporting.
    pub fn new(src: &'a str, pos: usize) -> Self {
        Self::Normal(StringParts { src, pos })
    }

    /// Returns an iterator over string parts for the given substring containing
    /// only the _inner_ contents (between quotes) of a multiline string.
    /// The given pos is the source position of this substring for error reporting.
    pub fn new_multiline(src: &'a str, pos: usize) -> Result<Self, ParserError> {
        Ok(Self::Multiline(MultilineStringParts::new(src, pos)?))
    }
}

impl<'a> Iterator for StringPartsIter<'a> {
    type Item = Result<Cow<'a, str>, ParserError>;

    fn next(&mut self) -> Option<Self::Item> {
        match self {
            StringPartsIter::Normal(parts) => parts.next(),
            StringPartsIter::Multiline(parts) => parts.next(),
        }
    }
}

pub struct StringParts<'a> {
    src: &'a str,
    pos: usize,
}

impl<'a> StringParts<'a> {
    fn next_part(&mut self) -> Result<Cow<'a, str>, ParserError> {
        let (part, consumed) = match self.src.find('\\') {
            Some(0) => {
                let (esc_char, esc_len) = unescape(self.src).ok_or_else(|| {
                    ParserError::new(ParserErrorKind::InvalidEscape, self.pos..self.pos + 1)
                })?;
                (Cow::Owned(esc_char.to_string()), esc_len)
            }
            Some(next_esc) => {
                let esc_prefix = &self.src[..next_esc];
                (Cow::Borrowed(esc_prefix), esc_prefix.len())
            }
            None => (Cow::Borrowed(self.src), self.src.len()),
        };
        self.src = &self.src[consumed..];
        self.pos += consumed;
        Ok(part)
    }
}

impl<'a> Iterator for StringParts<'a> {
    type Item = Result<Cow<'a, str>, ParserError>;

    fn next(&mut self) -> Option<Self::Item> {
        if self.src.is_empty() {
            return None;
        }
        Some(self.next_part())
    }
}

/// An iterator over parsed string "parts", each of which is either a literal
/// substring of the source or a decoded escape sequence.
pub struct MultilineStringParts<'a> {
    curr: StringParts<'a>,
    lines: Split<'a, char>,
    next_pos: usize,
    indent: &'a str,
}

impl<'a> MultilineStringParts<'a> {
    fn new(src: &'a str, pos: usize) -> Result<Self, ParserError> {
        let end = pos + src.len();

        // Strip leading carriage return as part of first newline
        let (src, next_pos) = match src.strip_prefix('\r') {
            Some(src) => (src, pos + 2),
            None => (src, pos + 1),
        };

        let mut lines = src.split('\n');

        // Remove mandatory final line, using trailing spaces as indent
        let indent = lines.next_back().unwrap();
        if indent.contains(|ch| ch != ' ') {
            return Err(ParserError::with_detail(
                ParserErrorKind::InvalidMultilineString,
                end - 3..end,
                r#"closing """ must be on a line preceded only by spaces"#,
            ));
        }

        // Validate mandatory initial empty line
        if lines.next() != Some("") {
            return Err(ParserError::with_detail(
                ParserErrorKind::InvalidMultilineString,
                pos..pos + 1,
                r#"opening """ must be followed immediately by newline"#,
            ));
        }

        let mut parts = Self {
            curr: StringParts { src: "", pos: 0 },
            lines,
            next_pos,
            indent,
        };

        // Skip first newline
        if let Some(nl) = parts.next().transpose()? {
            debug_assert_eq!(nl, "\n");
        }

        Ok(parts)
    }
}

impl<'a> Iterator for MultilineStringParts<'a> {
    type Item = Result<Cow<'a, str>, ParserError>;

    fn next(&mut self) -> Option<Self::Item> {
        if self.curr.src.is_empty() {
            let next = self.lines.next()?;

            // Update next line position
            let pos = self.next_pos;
            self.next_pos += next.len() + 1;

            // Strip indent
            let Some(src) = next.strip_prefix(self.indent) else {
                return Some(Err(ParserError::with_detail(
                    ParserErrorKind::InvalidMultilineString,
                    pos..pos + 1,
                    r#"lines must be indented at least as much as closing """"#,
                )));
            };
            let pos = pos + self.indent.len();

            // Strip trailing carriage return for `\r\n` newlines
            let src = src.strip_suffix('\r').unwrap_or(src);

            self.curr = StringParts { src, pos };

            Some(Ok("\n".into()))
        } else {
            Some(self.curr.next_part())
        }
    }
}

// Given a substring starting with an escape sequence, returns the decoded char
// and length of the sequence, or None if the sequence is invalid.
pub fn unescape(src: &str) -> Option<(char, usize)> {
    let mut chars = src.chars();
    if chars.next() != Some('\\') {
        return None;
    }
    Some(match chars.next()? {
        '\\' => ('\\', 2),
        '\'' => ('\'', 2),
        '"' => ('"', 2),
        't' => ('\t', 2),
        'n' => ('\n', 2),
        'r' => ('\r', 2),
        'u' => {
            if chars.next()? != '{' {
                return None;
            }
            let mut val = 0;
            let mut digits = 0;
            loop {
                let ch = chars.next()?;
                if ch == '}' {
                    if digits == 0 {
                        return None;
                    }
                    break;
                }
                val = (val << 4) | ch.to_digit(16)?;
                digits += 1;
                if digits > 6 {
                    return None;
                }
            }
            (char::from_u32(val)?, digits + 4)
        }
        _ => return None,
    })
}
