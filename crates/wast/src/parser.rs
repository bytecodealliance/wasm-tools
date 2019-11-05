//! Traits for parsing the WebAssembly Text format
//!
//! This module contains the traits, abstractions, and utilities needed to
//! define custom parsers for WebAssembly text format items. This module exposes
//! a recursive descent parsing strategy and centers around the
//! [`Parse`](crate::parser::Parse) trait for defining new fragments of
//! WebAssembly text syntax.
//!
//! The top-level [`parse`](crate::parser::parse) function can be used to fully parse AST fragments:
//!
//! ```
//! use wast::Wat;
//! use wast::parser::{self, ParseBuffer};
//!
//! # fn foo() -> Result<(), wast::Error> {
//! let wat = "(module (func))";
//! let buf = ParseBuffer::new(wat)?;
//! let module = parser::parse::<Wat>(&buf)?;
//! # Ok(())
//! # }
//! ```
//!
//! and you can also define your own new syntax with the
//! [`Parse`](crate::parser::Parse) trait:
//!
//! ```
//! use wast::{kw, Import, Func};
//! use wast::parser::{Parser, Parse, Result};
//!
//! // Fields of a WebAssembly which only allow imports and functions, and all
//! // imports must come before all the functions
//! struct OnlyImportsAndFunctions<'a> {
//!     imports: Vec<Import<'a>>,
//!     functions: Vec<Func<'a>>,
//! }
//!
//! impl<'a> Parse<'a> for OnlyImportsAndFunctions<'a> {
//!     fn parse(parser: Parser<'a>) -> Result<Self> {
//!         // While the second token is `import` (the first is `(`, so we care
//!         // about the second) we parse an `ast::ModuleImport` inside of
//!         // parentheses. The `parens` function here ensures that what we
//!         // parse inside of it is surrounded by `(` and `)`.
//!         let mut imports = Vec::new();
//!         while parser.peek2::<kw::import>() {
//!             let import = parser.parens(|p| p.parse())?;
//!             imports.push(import);
//!         }
//!
//!         // Afterwards we assume everything else is a function. Note that
//!         // `parse` here is a generic function and type inference figures out
//!         // that we're parsing functions here and imports above.
//!         let mut functions = Vec::new();
//!         while !parser.is_empty() {
//!             let func = parser.parens(|p| p.parse())?;
//!             functions.push(func);
//!         }
//!
//!         Ok(OnlyImportsAndFunctions { imports, functions })
//!     }
//! }
//! ```
//!
//! This module is heavily inspired by [`syn`](https://docs.rs/syn) so you can
//! likely also draw inspiration from the excellent examples in the `syn` crate.

use crate::lexer::{Comment, Float, Integer, Lexer, Source, Token};
use crate::{Error, Span};
use std::cell::Cell;
use std::fmt;

/// A top-level convenience parseing function that parss a `T` from `buf` and
/// requires that all tokens in `buf` are consume.
///
/// This generic parsing function can be used to parse any `T` implementing the
/// [`Parse`] trait. It is not used from [`Parse`] trait implementations.
///
/// # Examples
///
/// ```
/// use wast::Wat;
/// use wast::parser::{self, ParseBuffer};
///
/// # fn foo() -> Result<(), wast::Error> {
/// let wat = "(module (func))";
/// let buf = ParseBuffer::new(wat)?;
/// let module = parser::parse::<Wat>(&buf)?;
/// # Ok(())
/// # }
/// ```
///
/// or parsing simply a fragment
///
/// ```
/// use wast::parser::{self, ParseBuffer};
///
/// # fn foo() -> Result<(), wast::Error> {
/// let wat = "12";
/// let buf = ParseBuffer::new(wat)?;
/// let val = parser::parse::<u32>(&buf)?;
/// assert_eq!(val, 12);
/// # Ok(())
/// # }
/// ```
pub fn parse<'a, T: Parse<'a>>(buf: &'a ParseBuffer<'a>) -> Result<T> {
    let parser = buf.parser();
    let result = parser.parse()?;
    if parser.cursor().advance_token().is_none() {
        Ok(result)
    } else {
        Err(parser.error("extra tokens remaining after parse"))
    }
}

/// A trait for parsing a fragment of syntax in a recursive descent fashion.
///
/// The [`Parse`] trait is main abstraction you'll be working with when defining
/// custom parser or custom syntax for your WebAssembly text format (or when
/// using the official format items). Almost all items in the
/// [`ast`](crate::ast) module implement the [`Parse`] trait, and you'll
/// commonly use this with:
///
/// * The top-level [`parse`] function to parse an entire input.
/// * The intermediate [`Parser::parse`] function to parse an item out of an
///   input stream and then parse remaining items.
///
/// Implementation of [`Parse`] take a [`Parser`] as input and will mutate the
/// parser as they parse syntax. Once a token is consume it cannot be
/// "un-consumed". Utilities such as [`Parser::peek`] and [`Parser::lookahead1`]
/// can be used to determine what to parse next.
///
/// ## When to parse `(` and `)`?
///
/// Conventionally types are not responsible for parsing their own `(` and `)`
/// tokens which surround the type. For example WebAssembly imports look like:
///
/// ```text
/// (import "foo" "bar" (func (type 0)))
/// ```
///
/// but the [`Import`](crate::ast::Import) type parser looks like:
///
/// ```
/// # use wast::kw;
/// # use wast::parser::{Parser, Parse, Result};
/// # struct Import<'a>(&'a str);
/// impl<'a> Parse<'a> for Import<'a> {
///     fn parse(parser: Parser<'a>) -> Result<Self> {
///         parser.parse::<kw::import>()?;
///         // ...
/// # panic!()
///     }
/// }
/// ```
///
/// It is assumed here that the `(` and `)` tokens which surround an `import`
/// statement in the WebAssembly text format are parsed by the parent item
/// parsing `Import`.
///
/// Note that this is just a a convention, so it's not necessarily required of
/// all types. It's recommended that your types stick to this convention where
/// possible to avoid nested calls to [`Parser::parens`] or accidentally trying
/// to parse too many parenthesis.
///
/// # Examples
///
/// Let's say you want to define your own WebAssembly text format which only
/// contains imports and functions. You also require all imports to be listed
/// before all functions. An example [`Parse`] implementation might look like:
///
/// ```
/// use wast::{Import, Func, kw};
/// use wast::parser::{Parser, Parse, Result};
///
/// // Fields of a WebAssembly which only allow imports and functions, and all
/// // imports must come before all the functions
/// struct OnlyImportsAndFunctions<'a> {
///     imports: Vec<Import<'a>>,
///     functions: Vec<Func<'a>>,
/// }
///
/// impl<'a> Parse<'a> for OnlyImportsAndFunctions<'a> {
///     fn parse(parser: Parser<'a>) -> Result<Self> {
///         // While the second token is `import` (the first is `(`, so we care
///         // about the second) we parse an `ast::ModuleImport` inside of
///         // parentheses. The `parens` function here ensures that what we
///         // parse inside of it is surrounded by `(` and `)`.
///         let mut imports = Vec::new();
///         while parser.peek2::<kw::import>() {
///             let import = parser.parens(|p| p.parse())?;
///             imports.push(import);
///         }
///
///         // Afterwards we assume everything else is a function. Note that
///         // `parse` here is a generic function and type inference figures out
///         // that we're parsing functions here and imports above.
///         let mut functions = Vec::new();
///         while !parser.is_empty() {
///             let func = parser.parens(|p| p.parse())?;
///             functions.push(func);
///         }
///
///         Ok(OnlyImportsAndFunctions { imports, functions })
///     }
/// }
/// ```
pub trait Parse<'a>: Sized {
    /// Attempts to parse `Self` from `parser`, returning an error if it could
    /// not be parsed.
    ///
    /// This method will mutate the state of `parser` after attempting to parse
    /// an instance of `Self`. If an error happens then it is likely fatal and
    /// there is no guarantee of how many tokens have been consumed from
    /// `parser`.
    ///
    /// As recommended in the documentation of [`Parse`], implementations of
    /// this function should not start out by parsing `(` and `)` tokens, but
    /// rather parents calling recursive parsers should parse the `(` and `)`
    /// tokens for their child item that's being parsed.
    ///
    /// # Errors
    ///
    /// This function will return an error if `Self` could not be parsed. Note
    /// that creating an [`Error`] is not exactly a cheap operation, so
    /// [`Error`] is typically fatal and propagated all the way back to the top
    /// parse call site.
    fn parse(parser: Parser<'a>) -> Result<Self>;
}

/// A trait for types which be used to "peek" to see if they're the next token
/// in an input stream of [`Parser`].
///
/// Often when implementing [`Parse`] you'll need to query what the next token
/// in the stream is to figure out what to parse next. This [`Peek`] trait
/// defines the set of types that can be tested whether they're the next token
/// in the input stream.
///
/// Implementations of [`Peek`] should only be present on types that consume
/// exactly one token (not zero, not more, exactly one). Types implementing
/// [`Peek`] should also typically implement [`Parse`] should also typically
/// implement [`Parse`].
///
/// See the documentation of [`Parser::peek`] for example usage.
pub trait Peek {
    /// Tests to see whether this token is the first token within the [`Cursor`]
    /// specified.
    ///
    /// Returns `true` if [`Parse`] for this type is highly likely to succeed
    /// failing no other error conditions happening (like an integer literal
    /// being too big).
    fn peek(cursor: Cursor<'_>) -> bool;

    /// Returns a human-readable name of this token to display when generating
    /// errors about this token missing.
    fn display() -> &'static str;
}

/// A convenience type definition for `Result` where the error is hardwired to
/// [`Error`].
pub type Result<T> = std::result::Result<T, Error>;

/// A low-level buffer of tokens which represents a completely lexed file.
///
/// A `ParseBuffer` will immediately lex an entire file and then store all
/// tokens internally. A `ParseBuffer` only used to pass to the top-level
/// [`parse`] function.
pub struct ParseBuffer<'a> {
    // list of tokens from the tokenized source (including whitespace and
    // comments), and the second element is the index of the next `Token` token,
    // if any.
    tokens: Box<[(Source<'a>, Option<usize>)]>,
    input: &'a str,
    cur: Cell<usize>,
}

/// An in-progress parser for the tokens of a WebAssembly text file.
///
/// A `Parser` is argument to the [`Parse`] trait and is now the input stream is
/// interacted with to parse new items. Cloning [`Parser`] or copying a parser
/// refers to the same stream of tokens to parse, you cannot clone a [`Parser`]
/// and clone two items.
///
/// For more information about a [`Parser`] see its methods.
#[derive(Copy, Clone)]
pub struct Parser<'a> {
    buf: &'a ParseBuffer<'a>,
}

/// A helpful structure to perform a lookahead of one token to determine what to
/// parse.
///
/// For more information see the [`Parser::lookahead1`] method.
pub struct Lookahead1<'a> {
    parser: Parser<'a>,
    attempts: Vec<&'static str>,
}

/// An immutable cursor into a list of tokens.
///
/// This cursor cannot be mutated but can be used to parse more tokens in a list
/// of tokens. Cursors are created from the [`Parser::step`] method. This is a
/// very low-level parsing structure and you likely won't use it much.
#[derive(Copy, Clone)]
pub struct Cursor<'a> {
    parser: Parser<'a>,
    cur: usize,
}

impl ParseBuffer<'_> {
    /// Creates a new [`ParseBuffer`] by lexing the given `input` completely.
    ///
    /// # Errors
    ///
    /// Returns an error if `input` fails to lex.
    pub fn new(input: &str) -> Result<ParseBuffer<'_>> {
        let mut tokens = Vec::new();
        for token in Lexer::new(input) {
            tokens.push((token?, None));
        }

        // Calculate the "next token" for each token in the list. This'll allow
        // us to advance quickly inside of `advance_token` below instead of
        // having to skip over all comments/whitespace, perhaps repeatedly.
        //
        // To calculate this we go back-to-front, and just keep track of where
        // the last token was seen and fill that in for all the `next_token`
        // blanks.
        let mut last_token = None;
        for (i, (token, next_token)) in tokens.iter_mut().enumerate().rev() {
            *next_token = last_token;
            if let Source::Token(_) = token {
                last_token = Some(i);
            }
        }

        Ok(ParseBuffer {
            tokens: tokens.into_boxed_slice(),
            cur: Cell::new(0),
            input,
        })
    }

    fn parser(&self) -> Parser<'_> {
        Parser { buf: self }
    }
}

impl<'a> Parser<'a> {
    /// Returns whether there are no more `Token` tokens to parse from this
    /// [`Parser`].
    ///
    /// This indicates that either we've reached the end of the input, or we're
    /// a sub-[`Parser`] inside of a parenthesized expression and we've hit the
    /// `)` token.
    ///
    /// Note that if `false` is returned there *may* be more comments. Comments
    /// and whitespace are not considered for whether this parser is empty.
    pub fn is_empty(self) -> bool {
        match self.cursor().advance_token() {
            Some(Token::RParen(_)) | None => true,
            Some(_) => false, // more tokens to parse!
        }
    }

    /// Parses a `T` from this [`Parser`].
    ///
    /// This method has a trivial definition (it simply calls
    /// [`T::parse`](Parse::parse)) but is here for syntactic purposes. This is
    /// what you'll call 99% of the time in a [`Parse`] implementation in order
    /// to parse sub-items.
    ///
    /// Typically you always want to use `?` with the result of this method, you
    /// should not handle errors and decide what else to parse. To handle
    /// branches in parsing, ue [`Parser::peek`].
    ///
    /// # Examples
    ///
    /// A good example of using `parse` is to see how the [`TableType`] type is
    /// parsed in this crate. A [`TableType`] is defined in the official
    /// specification as [`tabletype`][spec] and is defined as:
    ///
    /// [spec]: https://webassembly.github.io/spec/core/text/types.html#table-types
    ///
    /// ```text
    /// tabletype ::= lim:limits et:elemtype
    /// ```
    ///
    /// so to parse a [`TableType`] we recursively need to parse a [`Limits`]
    /// and a [`TableElemType`]
    ///
    /// ```
    /// # use wast::*;
    /// # use wast::parser::*;
    /// struct TableType {
    ///     limits: Limits,
    ///     elem: TableElemType,
    /// }
    ///
    /// impl<'a> Parse<'a> for TableType {
    ///     fn parse(parser: Parser<'a>) -> Result<Self> {
    ///         // parse the `lim` then `et` in sequence
    ///         Ok(TableType {
    ///             limits: parser.parse()?,
    ///             elem: parser.parse()?,
    ///         })
    ///     }
    /// }
    /// ```
    ///
    /// [`Limits`]: crate::ast::Limits
    /// [`TableType`]: crate::ast::TableType
    /// [`TableElemType`]: crate::ast::TableElemType
    pub fn parse<T: Parse<'a>>(self) -> Result<T> {
        T::parse(self)
    }

    /// Performs a cheap test to see whether the current token in this stream is
    /// `T`.
    ///
    /// This method can be used to efficiently determine what next to parse. The
    /// [`Peek`] trait is defined for types which can be used to test if they're
    /// the next item in the input stream.
    ///
    /// Nothing is actually parsed in this method, nor does this mutate the
    /// state of this [`Parser`]. Instead, this simply performs a check.
    ///
    /// This method is frequently combined with the [`Parser::lookahead1`]
    /// method to automatically produce nice error messages if some tokens
    /// aren't found.
    ///
    /// # Examples
    ///
    /// For an example of using the `peek` method let's take a look at parsing
    /// the [`Limits`] type. This is [defined in the official spec][spec] as:
    ///
    /// ```text
    /// limits ::= n:u32
    ///          | n:u32 m:u32
    /// ```
    ///
    /// which means that it's either one `u32` token or two, so we need to know
    /// whether to consume two tokens or one:
    ///
    /// ```
    /// # use wast::parser::*;
    /// struct Limits {
    ///     min: u32,
    ///     max: Option<u32>,
    /// }
    ///
    /// impl<'a> Parse<'a> for Limits {
    ///     fn parse(parser: Parser<'a>) -> Result<Self> {
    ///         // Always parse the first number...
    ///         let min = parser.parse()?;
    ///
    ///         // ... and then test if there's a second number before parsing
    ///         let max = if parser.peek::<u32>() {
    ///             Some(parser.parse()?)
    ///         } else {
    ///             None
    ///         };
    ///
    ///         Ok(Limits { min, max })
    ///     }
    /// }
    /// ```
    ///
    /// [spec]: https://webassembly.github.io/spec/core/text/types.html#limits
    /// [`Limits`]: crate::ast::Limits
    pub fn peek<T: Peek>(self) -> bool {
        T::peek(self.cursor())
    }

    /// Same as the [`Parser::peek`] method, except checks the next token, not
    /// the current token.
    pub fn peek2<T: Peek>(self) -> bool {
        let mut cursor = self.cursor();
        if cursor.advance_token().is_some() {
            T::peek(cursor)
        } else {
            false
        }
    }

    /// A helper structure to perform a sequence of `peek` operations and if
    /// they all fail produce a nice error message.
    ///
    /// This method purely exists for conveniently producing error messages and
    /// provides no functionality that [`Parser::peek`] doesn't already give.
    /// The [`Lookahead1`] structure has one main method [`Lookahead1::peek`],
    /// which is the same method as [`Parser::peek`]. The difference is that the
    /// [`Lookahead1::error`] method needs no arguments.
    ///
    /// # Examples
    ///
    /// Let's look at the parsing of [`Index`]. This type is either a `u32` or
    /// an [`Id`] and is used in name resolution primarily. The [official
    /// grammar for an index][spec] is:
    ///
    /// ```text
    /// idx ::= x:u32
    ///       | v:id
    /// ```
    ///
    /// Which is to say that an index is either a `u32` or an [`Id`]. When
    /// parsing an [`Index`] we can do:
    ///
    /// ```
    /// # use wast::*;
    /// # use wast::parser::*;
    /// enum Index<'a> {
    ///     Num(u32),
    ///     Id(Id<'a>),
    /// }
    ///
    /// impl<'a> Parse<'a> for Index<'a> {
    ///     fn parse(parser: Parser<'a>) -> Result<Self> {
    ///         let mut l = parser.lookahead1();
    ///         if l.peek::<Id>() {
    ///             Ok(Index::Id(parser.parse()?))
    ///         } else if l.peek::<u32>() {
    ///             Ok(Index::Num(parser.parse()?))
    ///         } else {
    ///             // produces error message of `expected identifier or u32`
    ///             Err(l.error())
    ///         }
    ///     }
    /// }
    /// ```
    ///
    /// [spec]: https://webassembly.github.io/spec/core/text/modules.html#indices
    /// [`Index`]: crate::ast::Index
    /// [`Id`]: crate::ast::Id
    pub fn lookahead1(self) -> Lookahead1<'a> {
        Lookahead1 {
            attempts: Vec::new(),
            parser: self,
        }
    }

    /// Parsea an item surrounded by parentheses.
    ///
    /// WebAssembly's text format is all based on s-expressions, so naturally
    /// you're going to want to parse a lot of parenthesized things! As noted in
    /// the documentation of [`Parse`] you typically don't parse your own
    /// surrounding `(` and `)` tokens, but the parser above you parsed them for
    /// you. This is method method the parser above you uses.
    ///
    /// This method will parse a `(` token, and then call `f` on a sub-parser
    /// which when finished asserts that a `)` token is the next token. This
    /// requires that `f` consumes all tokens leading up to the paired `)`.
    ///
    /// Usage will often simply be `parser.parens(|p| p.parse())?` to
    /// automatically parse a type within parentheses, but you can, as always,
    /// go crazy and do whatever you'd like too.
    ///
    /// # Examples
    ///
    /// A good example of this is to see how a `Module` is parsed. This isn't
    /// the exact definition, but it's close enough!
    ///
    /// ```
    /// # use wast::*;
    /// # use wast::parser::*;
    /// struct Module<'a> {
    ///     fields: Vec<ModuleField<'a>>,
    /// }
    ///
    /// impl<'a> Parse<'a> for Module<'a> {
    ///     fn parse(parser: Parser<'a>) -> Result<Self> {
    ///         // Modules start out with a `module` keyword
    ///         parser.parse::<kw::module>()?;
    ///
    ///         // And then everything else is `(field ...)`, so while we've got
    ///         // items left we continuously parse parenthesized items.
    ///         let mut fields = Vec::new();
    ///         while !parser.is_empty() {
    ///             fields.push(parser.parens(|p| p.parse())?);
    ///         }
    ///         Ok(Module { fields })
    ///     }
    /// }
    /// ```
    pub fn parens<T>(self, f: impl FnOnce(Parser<'a>) -> Result<T>) -> Result<T> {
        let before = self.buf.cur.get();
        let res = self.step(|cursor| {
            let mut cursor = match cursor.lparen() {
                Some(rest) => rest,
                None => return Err(cursor.error("expected `(`")),
            };
            cursor.parser.buf.cur.set(cursor.cur);
            let result = f(cursor.parser)?;
            cursor.cur = cursor.parser.buf.cur.get();
            match cursor.rparen() {
                Some(rest) => Ok((result, rest)),
                None => Err(cursor.error("expected `)`")),
            }
        });
        if res.is_err() {
            self.buf.cur.set(before);
        }
        return res;
    }

    fn cursor(self) -> Cursor<'a> {
        Cursor {
            parser: self,
            cur: self.buf.cur.get(),
        }
    }

    /// A low-level parsing method you probably won't use.
    ///
    /// This is used to implement parsing of the most primitive types in the
    /// [`ast`](crate::ast) module. You probably don't want to use this, but
    /// probably want to use something like [`Parser::parse`] or
    /// [`Parser::parens`].
    pub fn step<F, T>(self, f: F) -> Result<T>
    where
        F: FnOnce(Cursor<'a>) -> Result<(T, Cursor<'a>)>,
    {
        let (result, cursor) = f(self.cursor())?;
        self.buf.cur.set(cursor.cur);
        Ok(result)
    }

    /// Creates an error whose line/column information is pointing at the
    /// current token.
    ///
    /// This is used to produce human-readable error messages which point to the
    /// right location in the input stream, and the `msg` here is arbitrary text
    /// used to associate with the error and indicate why it was generated.
    pub fn error(self, msg: impl fmt::Display) -> Error {
        self.error_at(self.cursor().cur_span(), &msg)
    }

    fn error_at(self, span: Span, msg: &dyn fmt::Display) -> Error {
        Error::parse(span, self.buf.input, msg.to_string())
    }

    fn input_pos(self, src: &'a str) -> usize {
        src.as_ptr() as usize - self.buf.input.as_ptr() as usize
    }

    /// Returns the span of the current token
    pub fn cur_span(&self) -> Span {
        self.cursor().cur_span()
    }
}

impl<'a> Cursor<'a> {
    /// Returns the span of the next `Token` token.
    ///
    /// Does not take into account whitespace or comments.
    pub fn cur_span(&self) -> Span {
        let offset = match self.clone().advance_token() {
            Some(t) => self.parser.input_pos(t.src()),
            None => self.parser.buf.input.len(),
        };
        Span { offset }
    }

    /// Same as [`Parser::error`], but works with the current token in this
    /// [`Cursor`] instead.
    pub fn error(&self, msg: impl fmt::Display) -> Error {
        self.parser.error_at(self.cur_span(), &msg)
    }

    /// Attempts to advance this cursor if the current token is a `(`.
    ///
    /// If the current token is `(`, returns a new [`Cursor`] pointing at the
    /// rest of the tokens in the stream. Otherwise returns `None`.
    ///
    /// This function will automatically skip over any comment or whitespace
    /// tokens.
    pub fn lparen(mut self) -> Option<Self> {
        match self.advance_token()? {
            Token::LParen(_) => Some(self),
            _ => None,
        }
    }

    /// Attempts to advance this cursor if the current token is a `)`.
    ///
    /// If the current token is `)`, returns a new [`Cursor`] pointing at the
    /// rest of the tokens in the stream. Otherwise returns `None`.
    ///
    /// This function will automatically skip over any comment or whitespace
    /// tokens.
    pub fn rparen(mut self) -> Option<Self> {
        match self.advance_token()? {
            Token::RParen(_) => Some(self),
            _ => None,
        }
    }

    /// Attempts to advance this cursor if the current token is a
    /// [`Token::Id`](crate::lexer::Token)
    ///
    /// If the current token is `Id`, returns the identifier minus the leading
    /// `$` character as well as a new [`Cursor`] pointing at the rest of the
    /// tokens in the stream. Otherwise returns `None`.
    ///
    /// This function will automatically skip over any comment or whitespace
    /// tokens.
    pub fn id(mut self) -> Option<(&'a str, Self)> {
        match self.advance_token()? {
            Token::Id(id) => Some((&id[1..], self)),
            _ => None,
        }
    }

    /// Attempts to advance this cursor if the current token is a
    /// [`Token::Keyword`](crate::lexer::Token)
    ///
    /// If the current token is `Keyword`, returns the keyword as well as a new
    /// [`Cursor`] pointing at the rest of the tokens in the stream. Otherwise
    /// returns `None`.
    ///
    /// This function will automatically skip over any comment or whitespace
    /// tokens.
    pub fn keyword(mut self) -> Option<(&'a str, Self)> {
        match self.advance_token()? {
            Token::Keyword(id) => Some((id, self)),
            _ => None,
        }
    }

    /// Attempts to advance this cursor if the current token is a
    /// [`Token::Reserved`](crate::lexer::Token)
    ///
    /// If the current token is `Reserved`, returns the reserved token as well
    /// as a new [`Cursor`] pointing at the rest of the tokens in the stream.
    /// Otherwise returns `None`.
    ///
    /// This function will automatically skip over any comment or whitespace
    /// tokens.
    pub fn reserved(mut self) -> Option<(&'a str, Self)> {
        match self.advance_token()? {
            Token::Reserved(id) => Some((id, self)),
            _ => None,
        }
    }

    /// Attempts to advance this cursor if the current token is a
    /// [`Token::Integer`](crate::lexer::Token)
    ///
    /// If the current token is `Integer`, returns the integer as well as a new
    /// [`Cursor`] pointing at the rest of the tokens in the stream. Otherwise
    /// returns `None`.
    ///
    /// This function will automatically skip over any comment or whitespace
    /// tokens.
    pub fn integer(mut self) -> Option<(&'a Integer<'a>, Self)> {
        match self.advance_token()? {
            Token::Integer(i) => Some((i, self)),
            _ => None,
        }
    }

    /// Attempts to advance this cursor if the current token is a
    /// [`Token::Float`](crate::lexer::Token)
    ///
    /// If the current token is `Float`, returns the float as well as a new
    /// [`Cursor`] pointing at the rest of the tokens in the stream. Otherwise
    /// returns `None`.
    ///
    /// This function will automatically skip over any comment or whitespace
    /// tokens.
    pub fn float(mut self) -> Option<(&'a Float<'a>, Self)> {
        match self.advance_token()? {
            Token::Float(f) => Some((f, self)),
            _ => None,
        }
    }

    /// Attempts to advance this cursor if the current token is a
    /// [`Token::String`](crate::lexer::Token)
    ///
    /// If the current token is `String`, returns the byte value of the string
    /// as well as a new [`Cursor`] pointing at the rest of the tokens in the
    /// stream. Otherwise returns `None`.
    ///
    /// This function will automatically skip over any comment or whitespace
    /// tokens.
    pub fn string(mut self) -> Option<(&'a [u8], Self)> {
        match self.advance_token()? {
            Token::String { val, .. } => Some((&**val, self)),
            _ => None,
        }
    }

    /// Attempts to advance this cursor if the current token is a
    /// [`Source::Comment`](crate::lexer::Comment)
    ///
    /// This function will skip any whitespace tokens, but it will not skip any
    /// other tokens.
    pub fn comment(mut self) -> Option<(&'a Comment<'a>, Self)> {
        let comment = loop {
            match &self.parser.buf.tokens.get(self.cur)?.0 {
                Source::Token(_) => return None,
                Source::Comment(c) => {
                    self.cur += 1;
                    break c;
                }
                Source::Whitespace(_) => {
                    self.cur += 1;
                }
            }
        };
        Some((comment, self))
    }

    fn advance_token(&mut self) -> Option<&'a Token<'a>> {
        let (token, next) = self.parser.buf.tokens.get(self.cur)?;
        match token {
            // If the current token is a `Token`, then only advance ourselves
            // one token further. That way if after this `Token` you want
            // comments, we can find the comments!
            Source::Token(t) => {
                self.cur += 1;
                Some(t)
            }

            // Otherwise this is a comment or whitespace token. In that case
            // `next` points to the next `Token` token if `next` was listed.
            _ => {
                let next = (*next)?;
                let (token, _) = &self.parser.buf.tokens[next];
                match token {
                    Source::Token(t) => {
                        self.cur = next + 1;
                        Some(t)
                    }
                    _ => unreachable!(),
                }
            }
        }
    }
}

impl Lookahead1<'_> {
    /// Attempts to see if `T` is the next token in the [`Parser`] this
    /// [`Lookahead1`] references.
    ///
    /// For more information see [`Parser::lookahead1`] and [`Parser::peek`]
    pub fn peek<T: Peek>(&mut self) -> bool {
        if self.parser.peek::<T>() {
            true
        } else {
            self.attempts.push(T::display());
            false
        }
    }

    /// Generates an error message saying that one of the tokens passed to
    /// [`Lookahead1::peek`] method was expected.
    ///
    /// Before calling this method you should call [`Lookahead1::peek`] for all
    /// possible tokens you'd like to parse.
    pub fn error(self) -> Error {
        match self.attempts.len() {
            0 => {
                if self.parser.is_empty() {
                    self.parser.error("unexpected end of input")
                } else {
                    self.parser.error("unexpected token")
                }
            }
            1 => {
                let message = format!("unexpected token, expected {}", self.attempts[0]);
                self.parser.error(&message)
            }
            2 => {
                let message = format!(
                    "unexpected token, expected {} or {}",
                    self.attempts[0], self.attempts[1]
                );
                self.parser.error(&message)
            }
            _ => {
                let join = self.attempts.join(", ");
                let message = format!("unexpected token, expected one of: {}", join);
                self.parser.error(&message)
            }
        }
    }
}

impl<'a, T: Peek + Parse<'a>> Parse<'a> for Option<T> {
    fn parse(parser: Parser<'a>) -> Result<Option<T>> {
        if parser.peek::<T>() {
            Ok(Some(parser.parse()?))
        } else {
            Ok(None)
        }
    }
}
