use crate::annotation;
use crate::core::*;
use crate::kw;
use crate::lexer::{Lexer, Token, TokenKind};
use crate::parser::{Parse, Parser, Result};
use crate::token::*;
use std::mem;

/// An expression, or a list of instructions, in the WebAssembly text format.
///
/// This expression type will parse s-expression-folded instructions into a flat
/// list of instructions for emission later on. The implicit `end` instruction
/// at the end of an expression is not included in the `instrs` field.
#[derive(Debug)]
#[allow(missing_docs)]
pub struct Expression<'a> {
    /// Instructions in this expression.
    pub instrs: Box<[Instruction<'a>]>,

    /// Branch hints, if any, found while parsing instructions.
    pub branch_hints: Box<[BranchHint]>,

    /// Optionally parsed spans of all instructions in `instrs`.
    ///
    /// This value is `None` as it's disabled by default. This can be enabled
    /// through the
    /// [`ParseBuffer::track_instr_spans`](crate::parser::ParseBuffer::track_instr_spans)
    /// function.
    ///
    /// This is not tracked by default due to the memory overhead and limited
    /// use of this field.
    pub instr_spans: Option<Box<[Span]>>,
}

/// A `@metadata.code.branch_hint` in the code, associated with a If or BrIf
/// This instruction is a placeholder and won't produce anything. Its purpose
/// is to store the offset of the following instruction and check that
/// it's followed by `br_if` or `if`.
#[derive(Debug)]
pub struct BranchHint {
    /// Index of instructions in `instrs` field of `Expression` that this hint
    /// applies to.
    pub instr_index: usize,
    /// The value of this branch hint
    pub value: u32,
}

impl<'a> Parse<'a> for Expression<'a> {
    fn parse(parser: Parser<'a>) -> Result<Self> {
        let mut exprs = ExpressionParser::new(parser);
        exprs.parse(parser)?;
        Ok(Expression {
            instrs: exprs.raw_instrs.into(),
            branch_hints: exprs.branch_hints.into(),
            instr_spans: exprs.spans.map(|s| s.into()),
        })
    }
}

impl<'a> Expression<'a> {
    /// Creates an expression from the single `instr` specified.
    pub fn one(instr: Instruction<'a>) -> Expression<'a> {
        Expression {
            instrs: [instr].into(),
            branch_hints: Box::new([]),
            instr_spans: None,
        }
    }

    /// Parse an expression formed from a single folded instruction.
    ///
    /// Attempts to parse an expression formed from a single folded instruction.
    ///
    /// This method will mutate the state of `parser` after attempting to parse
    /// the expression. If an error happens then it is likely fatal and
    /// there is no guarantee of how many tokens have been consumed from
    /// `parser`.
    ///
    /// # Errors
    ///
    /// This function will return an error if the expression could not be
    /// parsed. Note that creating an [`crate::Error`] is not exactly a cheap
    /// operation, so [`crate::Error`] is typically fatal and propagated all the
    /// way back to the top parse call site.
    pub fn parse_folded_instruction(parser: Parser<'a>) -> Result<Self> {
        let mut exprs = ExpressionParser::new(parser);
        exprs.parse_folded_instruction(parser)?;
        Ok(Expression {
            instrs: exprs.raw_instrs.into(),
            branch_hints: exprs.branch_hints.into(),
            instr_spans: exprs.spans.map(|s| s.into()),
        })
    }
}

/// Helper struct used to parse an `Expression` with helper methods and such.
///
/// The primary purpose of this is to avoid defining expression parsing as a
/// call-thread-stack recursive function. Since we're parsing user input that
/// runs the risk of blowing the call stack, so we want to be sure to use a heap
/// stack structure wherever possible.
struct ExpressionParser<'a> {
    /// The flat list of instructions that we've parsed so far, and will
    /// eventually become the final `Expression`.
    ///
    /// Appended to with `push_instr` to ensure that this is the same length of
    /// `spans` if `spans` is used.
    raw_instrs: Vec<Instruction<'a>>,

    /// Descriptor of all our nested s-expr blocks. This only happens when
    /// instructions themselves are nested.
    stack: Vec<Level<'a>>,

    /// Related to the branch hints proposal.
    /// Will be used later to collect the offsets in the final binary.
    /// <(index of branch instructions, BranchHintAnnotation)>
    branch_hints: Vec<BranchHint>,

    /// A branch hint annotation that has been parsed but not yet attached to an
    /// instruction. The annotation applies to the instruction (or folded
    /// instruction) that immediately follows it. For a folded instruction the
    /// head instruction (e.g. the `if` or `br_if`) is pushed after its
    /// operands, so the hint's index cannot be known until that instruction is
    /// actually pushed. See `push_instr_with_hint`.
    pending_hint: Option<u32>,

    /// Storage for all span information in `raw_instrs`. Optionally disabled to
    /// reduce memory consumption of parsing expressions.
    spans: Option<Vec<Span>>,
}

enum Paren {
    None,
    Left,
    Right(Span),
}

/// A "kind" of nested block that we can be parsing inside of.
enum Level<'a> {
    /// This is a normal `block` or `loop` or similar, where the instruction
    /// payload here is pushed when the block is exited.
    ///
    /// The final field is a pending branch hint that applies to the payload
    /// instruction, if the folded instruction was preceded by a branch hint
    /// annotation. It's recorded when the instruction is finally pushed.
    EndWith(Instruction<'a>, Option<Span>, Option<u32>),

    /// This is a pretty special variant which means that we're parsing an `if`
    /// statement, and the state of the `if` parsing is tracked internally in
    /// the payload.
    ///
    /// The final field is a pending branch hint that applies to the `if`
    /// instruction, if it was preceded by a branch hint annotation. It's
    /// recorded when the `if` instruction is finally pushed (see
    /// `handle_if_lparen`).
    If(If<'a>, Option<u32>),

    /// This means we're either parsing inside of `(then ...)` or `(else ...)`
    /// which don't correspond to terminating instructions, we're just in a
    /// nested block.
    IfArm,

    /// This means we are finishing the parsing of a branch hint annotation.
    BranchHint,
}

/// Possible states of "what is currently being parsed?" in an `if` expression.
enum If<'a> {
    /// Only the `if` instruction has been parsed, next thing to parse is the
    /// clause, if any, of the `if` instruction.
    ///
    /// This parse ends when `(then ...)` is encountered.
    Clause(Instruction<'a>, Span),
    /// Currently parsing the `then` block, and afterwards a closing paren is
    /// required or an `(else ...)` expression.
    Then,
    /// Parsing the `else` expression, nothing can come after.
    Else,
}

impl<'a> ExpressionParser<'a> {
    fn new(parser: Parser<'a>) -> ExpressionParser<'a> {
        ExpressionParser {
            raw_instrs: Vec::new(),
            stack: Vec::new(),
            branch_hints: Vec::new(),
            pending_hint: None,
            spans: if parser.track_instr_spans() {
                Some(Vec::new())
            } else {
                None
            },
        }
    }

    fn parse(&mut self, parser: Parser<'a>) -> Result<()> {
        // Here we parse instructions in a loop, and we do not recursively
        // invoke this parse function to avoid blowing the stack on
        // deeply-recursive parses.
        //
        // Our loop generally only finishes once there's no more input left int
        // the `parser`. If there's some unclosed delimiters though (on our
        // `stack`), then we also keep parsing to generate error messages if
        // there's no input left.
        while !parser.is_empty() || !self.stack.is_empty() {
            // As a small ease-of-life adjustment here, if we're parsing inside
            // of an `if block then we require that all sub-components are
            // s-expressions surrounded by `(` and `)`, so verify that here.
            if let Some(Level::If(..)) = self.stack.last() {
                if !parser.is_empty() && !parser.peek::<LParen>()? {
                    return Err(parser.error("expected `(`"));
                }
            }

            match self.paren(parser)? {
                // No parenthesis seen? Then we just parse the next instruction
                // and move on.
                Paren::None => {
                    let span = parser.cur_span();
                    // A flat instruction is pushed immediately, so any pending
                    // branch hint applies directly to it.
                    let hint = self.pending_hint.take();
                    self.push_instr_with_hint(parser.parse()?, span, hint);
                }

                // If we see a left-parenthesis then things are a little
                // special. We handle block-like instructions specially
                // (`block`, `loop`, and `if`), and otherwise all other
                // instructions simply get appended once we reach the end of the
                // s-expression.
                //
                // In all cases here we push something onto the `stack` to get
                // popped when the `)` character is seen.
                Paren::Left => {
                    // First up is handling `if` parsing, which is funky in a
                    // whole bunch of ways. See the method internally for more
                    // information.
                    if self.handle_if_lparen(parser)? {
                        continue;
                    }

                    // Handle the case of a branch hint annotation
                    if parser.peek::<annotation::metadata_code_branch_hint>()? {
                        self.parse_branch_hint(parser)?;
                        self.stack.push(Level::BranchHint);
                        continue;
                    }

                    let span = parser.cur_span();
                    // Any pending branch hint applies to the head of this folded
                    // instruction, so take it here and route it to wherever the
                    // head instruction is pushed.
                    let hint = self.pending_hint.take();
                    match parser.parse()? {
                        // If block/loop show up then we just need to be sure to
                        // push an `end` instruction whenever the `)` token is
                        // seen. The head instruction is pushed immediately, so
                        // the hint is recorded now.
                        i @ Instruction::block(_)
                        | i @ Instruction::loop_(_)
                        | i @ Instruction::try_table(_) => {
                            self.push_instr_with_hint(i, span, hint);
                            self.stack
                                .push(Level::EndWith(Instruction::end(None), None, None));
                        }

                        // Parsing an `if` instruction is super tricky, so we
                        // push an `If` scope and we let all our scope-based
                        // parsing handle the remaining items. The `if`
                        // instruction is pushed only once `(then` is reached, so
                        // stash the pending hint until then.
                        i @ Instruction::if_(_) => {
                            self.stack.push(Level::If(If::Clause(i, span), hint));
                        }

                        // Anything else means that we're parsing a nested form
                        // such as `(i32.add ...)` which means that the
                        // instruction we parsed will be coming at the end, so
                        // stash the pending hint until the closing `)`.
                        other => self.stack.push(Level::EndWith(other, Some(span), hint)),
                    }
                }

                // If we registered a `)` token as being seen, then we're
                // guaranteed there's an item in the `stack` stack for us to
                // pop. We peel that off and take a look at what it says to do.
                Paren::Right(span) => {
                    let level = self.stack.pop().unwrap();
                    // A pending hint at a closing `)` had no instruction after
                    // it (e.g. `(br_if ... (@...))` or a dangling annotation), so
                    // it's misplaced — except for the `)` closing the annotation.
                    if !matches!(level, Level::BranchHint) {
                        self.check_hint_consumed(parser)?;
                    }
                    match level {
                        Level::EndWith(i, s, hint) => {
                            self.push_instr_with_hint(i, s.unwrap_or(span), hint)
                        }
                        Level::IfArm => {}
                        Level::BranchHint => {}

                        // If an `if` statement hasn't parsed the clause or `then`
                        // block, then that's an error because there weren't enough
                        // items in the `if` statement. Otherwise we're just careful
                        // to terminate with an `end` instruction.
                        Level::If(If::Clause(..), _) => {
                            return Err(parser.error("previous `if` had no `then`"));
                        }
                        Level::If(_, _) => {
                            self.push_instr(Instruction::end(None), span);
                        }
                    }
                }
            }
        }
        // A trailing annotation with no following instruction is likewise
        // misplaced.
        self.check_hint_consumed(parser)?;
        Ok(())
    }

    fn parse_folded_instruction(&mut self, parser: Parser<'a>) -> Result<()> {
        let mut done = false;
        while !done {
            match self.paren(parser)? {
                Paren::Left => {
                    let span = parser.cur_span();
                    self.stack
                        .push(Level::EndWith(parser.parse()?, Some(span), None));
                }
                Paren::Right(span) => {
                    let (top_instr, span) = match self.stack.pop().unwrap() {
                        Level::EndWith(i, s, _) => (i, s.unwrap_or(span)),
                        _ => panic!("unknown level type"),
                    };
                    self.push_instr(top_instr, span);
                    if self.stack.is_empty() {
                        done = true;
                    }
                }
                Paren::None => {
                    return Err(parser.error("expected to continue a folded instruction"));
                }
            }
        }
        Ok(())
    }

    /// Parses either `(`, `)`, or nothing.
    fn paren(&self, parser: Parser<'a>) -> Result<Paren> {
        parser.step(|cursor| {
            Ok(match cursor.lparen()? {
                Some(rest) => (Paren::Left, rest),
                None if self.stack.is_empty() => (Paren::None, cursor),
                None => match cursor.rparen()? {
                    Some(rest) => (Paren::Right(cursor.cur_span()), rest),
                    None => (Paren::None, cursor),
                },
            })
        })
    }

    /// State transitions with parsing an `if` statement.
    ///
    /// The syntactical form of an `if` statement looks like:
    ///
    /// ```wat
    /// (if ($clause)... (then $then) (else $else))
    /// ```
    ///
    /// THis method is called after a `(` is parsed within the `(if ...` block.
    /// This determines what to do next.
    ///
    /// Returns `true` if the rest of the arm above should be skipped, or
    /// `false` if we should parse the next item as an instruction (because we
    /// didn't handle the lparen here).
    fn handle_if_lparen(&mut self, parser: Parser<'a>) -> Result<bool> {
        // Only execute the code below if there's an `If` listed last.
        let (i, pending) = match self.stack.last_mut() {
            Some(Level::If(i, pending)) => (i, pending),
            _ => return Ok(false),
        };

        match i {
            // If the clause is still being parsed then interpret this `(` as a
            // folded instruction unless it starts with `then`, in which case
            // this transitions to the `Then` state and a new level has been
            // reached.
            If::Clause(if_instr, if_instr_span) => {
                if !parser.peek::<kw::then>()? {
                    return Ok(false);
                }
                // A pending hint here isn't followed by an instruction (the
                // next token is `(then`), so it's misplaced.
                if self.pending_hint.is_some() {
                    return Err(Self::hint_placement_error(parser));
                }
                parser.parse::<kw::then>()?;
                let instr = mem::replace(if_instr, Instruction::end(None));
                let span = *if_instr_span;
                let hint = pending.take();
                *i = If::Then;
                self.push_instr_with_hint(instr, span, hint);
                self.stack.push(Level::IfArm);
                Ok(true)
            }

            // Previously we were parsing the `(then ...)` clause so this next
            // `(` must be followed by `else`.
            If::Then => {
                let span = parser.parse::<kw::r#else>()?.0;
                *i = If::Else;
                self.push_instr(Instruction::else_(None), span);
                self.stack.push(Level::IfArm);
                Ok(true)
            }

            // If after a `(else ...` clause is parsed there's another `(` then
            // that's not syntactically allowed.
            If::Else => Err(parser.error("unexpected token: too many payloads inside of `(if)`")),
        }
    }

    fn parse_branch_hint(&mut self, parser: Parser<'a>) -> Result<()> {
        parser.parse::<annotation::metadata_code_branch_hint>()?;

        let hint = parser.parse::<String>()?;

        let value = match hint.as_bytes() {
            [0] => 0,
            [1] => 1,
            _ => return Err(parser.error("invalid value for branch hint")),
        };

        // A pending hint that hasn't yet been attached to an instruction means
        // two annotations are targeting the same instruction, which is a
        // duplicate.
        if self.pending_hint.is_some() {
            return Err(parser.error("@metadata.code.branch_hint annotation: duplicate annotation"));
        }
        self.pending_hint = Some(value);
        Ok(())
    }

    fn push_instr(&mut self, instr: Instruction<'a>, span: Span) {
        self.raw_instrs.push(instr);
        if let Some(spans) = &mut self.spans {
            spans.push(span);
        }
    }

    /// Errors if a branch hint annotation has been parsed but not attached to an
    /// instruction. A branch hint must immediately precede the instruction it
    /// applies to, so an unconsumed hint means the annotation was misplaced
    /// (inside a folded form or with no following instruction).
    fn check_hint_consumed(&self, parser: Parser<'a>) -> Result<()> {
        if self.pending_hint.is_some() {
            return Err(Self::hint_placement_error(parser));
        }
        Ok(())
    }

    fn hint_placement_error(parser: Parser<'a>) -> crate::Error {
        parser.error("@metadata.code.branch_hint annotation: must precede an instruction")
    }

    /// Pushes an instruction, recording a branch hint for it if `hint` is a
    /// pending branch hint value. The hint's `instr_index` is the index this
    /// instruction is pushed at, so branch hints end up sorted by increasing
    /// index (instructions are only ever appended) as the encoder requires.
    fn push_instr_with_hint(&mut self, instr: Instruction<'a>, span: Span, hint: Option<u32>) {
        if let Some(value) = hint {
            self.branch_hints.push(BranchHint {
                instr_index: self.raw_instrs.len(),
                value,
            });
        }
        self.push_instr(instr, span);
    }
}

// TODO: document this obscenity
macro_rules! instructions {
    (pub enum Instruction<'a> {
        $(
            $(#[$custom:ident])?
            $name:ident $(($($arg:tt)*))? : $instr:tt $( | $deprecated:tt )?,
        )*
    }) => (
        /// A listing of all WebAssembly instructions that can be in a module
        /// that this crate currently parses.
        #[derive(Debug, Clone)]
        #[allow(missing_docs, non_camel_case_types)]
        pub enum Instruction<'a> {
            $(
                $name $(( instructions!(@ty $($arg)*) ))?,
            )*
        }

        impl<'a> Parse<'a> for Instruction<'a> {
            fn parse(parser: Parser<'a>) -> Result<Self> {
                $(
                    fn $name<'a>(_parser: Parser<'a>) -> Result<Instruction<'a>> {
                        Ok(Instruction::$name $((
                            instructions!(@parse _parser $($arg)*)?
                        ))?)
                    }
                )*
                let parse_remainder = parser.step(|c| {
                    let (kw, rest) = match c.keyword() ?{
                        Some(pair) => pair,
                        None => return Err(c.error("expected an instruction")),
                    };
                    match kw {
                        $($instr $( | $deprecated )?=> Ok(($name as fn(_) -> _, rest)),)*
                        _ => return Err(c.error("unknown operator or unexpected token")),
                    }
                })?;
                parse_remainder(parser)
            }
        }

        impl<'a> Instruction<'a> {
            pub(crate) fn encode(&self, sink: &mut wasm_encoder::InstructionSink<'_>) {
                match self {
                    $(
                        Instruction::$name $((instructions!(@first x $($arg)*)))? => {
                            instructions!(@encode sink x $name $(($($arg)*))? $(#[$custom])?)
                        }
                    )*
                }
            }

            /// Returns the associated [`MemArg`] if one is available for this
            /// instruction.
            #[allow(unused_variables)]
            pub fn memarg_mut(&mut self) -> Option<&mut MemArg<'a>> {
                match self {
                    $(
                        Instruction::$name $((instructions!(@memarg_binding a $($arg)*)))? => {
                            instructions!(@get_memarg a $($($arg)*)?)
                        }
                    )*
                }
            }
        }
    );

    (@ty MemArg<$amt:tt>) => (MemArg<'a>);
    (@ty LoadOrStoreLane<$amt:tt>) => (LoadOrStoreLane<'a>);
    (@ty $other:ty) => ($other);

    (@first $first:ident $($t:tt)*) => ($first);

    (@parse $parser:ident MemArg<$amt:tt>) => (MemArg::parse($parser, $amt));
    (@parse $parser:ident MemArg) => (compile_error!("must specify `MemArg` default"));
    (@parse $parser:ident LoadOrStoreLane<$amt:tt>) => (LoadOrStoreLane::parse($parser, $amt));
    (@parse $parser:ident LoadOrStoreLane) => (compile_error!("must specify `LoadOrStoreLane` default"));
    (@parse $parser:ident $other:ty) => ($parser.parse::<$other>());

    // Instructions call the `InstructionSink` method of the same name, with
    // the payload, if any, converted via `Into` into the single argument of
    // the method. Instructions annotated with `#[custom_encode]` are instead
    // encoded by the function of the same name in
    // `crate::core::binary::custom_encoders`.
    (@encode $sink:ident $x:ident $method:ident $(($($arg:tt)*))?) => ({
        $sink.$method($(instructions!(@first $x $($arg)*).into())?);
    });
    (@encode $sink:ident $x:ident $method:ident ($($arg:tt)*) #[custom_encode]) => ({
        crate::core::binary::custom_encoders::$method($sink, $x);
    });

    (@get_memarg $name:ident MemArg<$amt:tt>) => (Some($name));
    (@get_memarg $name:ident LoadOrStoreLane<$amt:tt>) => (Some(&mut $name.memarg));
    (@get_memarg $($other:tt)*) => (None);

    (@memarg_binding $name:ident MemArg<$amt:tt>) => ($name);
    (@memarg_binding $name:ident LoadOrStoreLane<$amt:tt>) => ($name);
    (@memarg_binding $name:ident $other:ty) => (_);
}

instructions! {
    pub enum Instruction<'a> {
        block(Box<BlockType<'a>>) : "block",
        if_(Box<BlockType<'a>>) : "if",
        #[custom_encode]
        else_(Option<Id<'a>>) : "else",
        loop_(Box<BlockType<'a>>) : "loop",
        #[custom_encode]
        end(Option<Id<'a>>) : "end",

        unreachable : "unreachable",
        nop : "nop",
        br(Index<'a>) : "br",
        br_if(Index<'a>) : "br_if",
        #[custom_encode]
        br_table(BrTableIndices<'a>) : "br_table",
        return_ : "return",
        call(Index<'a>) : "call",
        #[custom_encode]
        call_indirect(Box<CallIndirect<'a>>) : "call_indirect",

        // tail-call proposal
        return_call(Index<'a>) : "return_call",
        #[custom_encode]
        return_call_indirect(Box<CallIndirect<'a>>) : "return_call_indirect",

        // function-references proposal
        call_ref(Index<'a>) : "call_ref",
        return_call_ref(Index<'a>) : "return_call_ref",

        drop : "drop",
        #[custom_encode]
        select(SelectTypes<'a>) : "select",
        local_get(Index<'a>) : "local.get",
        local_set(Index<'a>) : "local.set",
        local_tee(Index<'a>) : "local.tee",
        global_get(Index<'a>) : "global.get",
        global_set(Index<'a>) : "global.set",

        table_get(TableArg<'a>) : "table.get",
        table_set(TableArg<'a>) : "table.set",

        i32_load(MemArg<4>) : "i32.load",
        i64_load(MemArg<8>) : "i64.load",
        f32_load(MemArg<4>) : "f32.load",
        f64_load(MemArg<8>) : "f64.load",
        i32_load8_s(MemArg<1>) : "i32.load8_s",
        i32_load8_u(MemArg<1>) : "i32.load8_u",
        i32_load16_s(MemArg<2>) : "i32.load16_s",
        i32_load16_u(MemArg<2>) : "i32.load16_u",
        i64_load8_s(MemArg<1>) : "i64.load8_s",
        i64_load8_u(MemArg<1>) : "i64.load8_u",
        i64_load16_s(MemArg<2>) : "i64.load16_s",
        i64_load16_u(MemArg<2>) : "i64.load16_u",
        i64_load32_s(MemArg<4>) : "i64.load32_s",
        i64_load32_u(MemArg<4>) : "i64.load32_u",
        i32_store(MemArg<4>) : "i32.store",
        i64_store(MemArg<8>) : "i64.store",
        f32_store(MemArg<4>) : "f32.store",
        f64_store(MemArg<8>) : "f64.store",
        i32_store8(MemArg<1>) : "i32.store8",
        i32_store16(MemArg<2>) : "i32.store16",
        i64_store8(MemArg<1>) : "i64.store8",
        i64_store16(MemArg<2>) : "i64.store16",
        i64_store32(MemArg<4>) : "i64.store32",

        // Lots of bulk memory proposal here as well
        memory_size(MemoryArg<'a>) : "memory.size",
        memory_grow(MemoryArg<'a>) : "memory.grow",
        #[custom_encode]
        memory_init(MemoryInit<'a>) : "memory.init",
        #[custom_encode]
        memory_copy(MemoryCopy<'a>) : "memory.copy",
        memory_fill(MemoryArg<'a>) : "memory.fill",
        memory_discard(MemoryArg<'a>) : "memory.discard",
        data_drop(Index<'a>) : "data.drop",
        elem_drop(Index<'a>) : "elem.drop",
        #[custom_encode]
        table_init(TableInit<'a>) : "table.init",
        #[custom_encode]
        table_copy(TableCopy<'a>) : "table.copy",
        table_fill(TableArg<'a>) : "table.fill",
        table_size(TableArg<'a>) : "table.size",
        table_grow(TableArg<'a>) : "table.grow",

        ref_null(HeapType<'a>) : "ref.null",
        ref_is_null : "ref.is_null",
        ref_func(Index<'a>) : "ref.func",

        // function-references proposal
        ref_as_non_null : "ref.as_non_null",
        br_on_null(Index<'a>) : "br_on_null",
        br_on_non_null(Index<'a>) : "br_on_non_null",

        // gc proposal: eqref
        ref_eq : "ref.eq",

        // gc proposal: struct
        struct_new(Index<'a>) : "struct.new",
        struct_new_default(Index<'a>) : "struct.new_default",
        #[custom_encode]
        struct_get(StructAccess<'a>) : "struct.get",
        #[custom_encode]
        struct_get_s(StructAccess<'a>) : "struct.get_s",
        #[custom_encode]
        struct_get_u(StructAccess<'a>) : "struct.get_u",
        #[custom_encode]
        struct_set(StructAccess<'a>) : "struct.set",

        // gc proposal: array
        array_new(Index<'a>) : "array.new",
        array_new_default(Index<'a>) : "array.new_default",
        #[custom_encode]
        array_new_fixed(ArrayNewFixed<'a>) : "array.new_fixed",
        #[custom_encode]
        array_new_data(ArrayNewData<'a>) : "array.new_data",
        #[custom_encode]
        array_new_elem(ArrayNewElem<'a>) : "array.new_elem",
        array_get(Index<'a>) : "array.get",
        array_get_s(Index<'a>) : "array.get_s",
        array_get_u(Index<'a>) : "array.get_u",
        array_set(Index<'a>) : "array.set",
        array_len : "array.len",
        array_fill(ArrayFill<'a>) : "array.fill",
        #[custom_encode]
        array_copy(ArrayCopy<'a>) : "array.copy",
        #[custom_encode]
        array_init_data(ArrayInit<'a>) : "array.init_data",
        #[custom_encode]
        array_init_elem(ArrayInit<'a>) : "array.init_elem",

        // gc proposal, i31
        ref_i31 : "ref.i31",
        i31_get_s : "i31.get_s",
        i31_get_u : "i31.get_u",

        // gc proposal, concrete casting
        #[custom_encode]
        ref_test(RefTest<'a>) : "ref.test",
        #[custom_encode]
        ref_cast(RefCast<'a>) : "ref.cast",
        #[custom_encode]
        br_on_cast(Box<BrOnCast<'a>>) : "br_on_cast",
        #[custom_encode]
        br_on_cast_fail(Box<BrOnCastFail<'a>>) : "br_on_cast_fail",

        // gc proposal extern/any coercion operations
        any_convert_extern : "any.convert_extern",
        extern_convert_any : "extern.convert_any",

        #[custom_encode]
        i32_const(i32) : "i32.const",
        #[custom_encode]
        i64_const(i64) : "i64.const",
        f32_const(F32) : "f32.const",
        f64_const(F64) : "f64.const",

        i32_clz : "i32.clz",
        i32_ctz : "i32.ctz",
        i32_popcnt : "i32.popcnt",
        i32_add : "i32.add",
        i32_sub : "i32.sub",
        i32_mul : "i32.mul",
        i32_div_s : "i32.div_s",
        i32_div_u : "i32.div_u",
        i32_rem_s : "i32.rem_s",
        i32_rem_u : "i32.rem_u",
        i32_and : "i32.and",
        i32_or : "i32.or",
        i32_xor : "i32.xor",
        i32_shl : "i32.shl",
        i32_shr_s : "i32.shr_s",
        i32_shr_u : "i32.shr_u",
        i32_rotl : "i32.rotl",
        i32_rotr : "i32.rotr",

        i64_clz : "i64.clz",
        i64_ctz : "i64.ctz",
        i64_popcnt : "i64.popcnt",
        i64_add : "i64.add",
        i64_sub : "i64.sub",
        i64_mul : "i64.mul",
        i64_div_s : "i64.div_s",
        i64_div_u : "i64.div_u",
        i64_rem_s : "i64.rem_s",
        i64_rem_u : "i64.rem_u",
        i64_and : "i64.and",
        i64_or : "i64.or",
        i64_xor : "i64.xor",
        i64_shl : "i64.shl",
        i64_shr_s : "i64.shr_s",
        i64_shr_u : "i64.shr_u",
        i64_rotl : "i64.rotl",
        i64_rotr : "i64.rotr",

        f32_abs : "f32.abs",
        f32_neg : "f32.neg",
        f32_ceil : "f32.ceil",
        f32_floor : "f32.floor",
        f32_trunc : "f32.trunc",
        f32_nearest : "f32.nearest",
        f32_sqrt : "f32.sqrt",
        f32_add : "f32.add",
        f32_sub : "f32.sub",
        f32_mul : "f32.mul",
        f32_div : "f32.div",
        f32_min : "f32.min",
        f32_max : "f32.max",
        f32_copysign : "f32.copysign",

        f64_abs : "f64.abs",
        f64_neg : "f64.neg",
        f64_ceil : "f64.ceil",
        f64_floor : "f64.floor",
        f64_trunc : "f64.trunc",
        f64_nearest : "f64.nearest",
        f64_sqrt : "f64.sqrt",
        f64_add : "f64.add",
        f64_sub : "f64.sub",
        f64_mul : "f64.mul",
        f64_div : "f64.div",
        f64_min : "f64.min",
        f64_max : "f64.max",
        f64_copysign : "f64.copysign",

        i32_eqz : "i32.eqz",
        i32_eq : "i32.eq",
        i32_ne : "i32.ne",
        i32_lt_s : "i32.lt_s",
        i32_lt_u : "i32.lt_u",
        i32_gt_s : "i32.gt_s",
        i32_gt_u : "i32.gt_u",
        i32_le_s : "i32.le_s",
        i32_le_u : "i32.le_u",
        i32_ge_s : "i32.ge_s",
        i32_ge_u : "i32.ge_u",

        i64_eqz : "i64.eqz",
        i64_eq : "i64.eq",
        i64_ne : "i64.ne",
        i64_lt_s : "i64.lt_s",
        i64_lt_u : "i64.lt_u",
        i64_gt_s : "i64.gt_s",
        i64_gt_u : "i64.gt_u",
        i64_le_s : "i64.le_s",
        i64_le_u : "i64.le_u",
        i64_ge_s : "i64.ge_s",
        i64_ge_u : "i64.ge_u",

        f32_eq : "f32.eq",
        f32_ne : "f32.ne",
        f32_lt : "f32.lt",
        f32_gt : "f32.gt",
        f32_le : "f32.le",
        f32_ge : "f32.ge",

        f64_eq : "f64.eq",
        f64_ne : "f64.ne",
        f64_lt : "f64.lt",
        f64_gt : "f64.gt",
        f64_le : "f64.le",
        f64_ge : "f64.ge",

        i32_wrap_i64 : "i32.wrap_i64",
        i32_trunc_f32_s : "i32.trunc_f32_s",
        i32_trunc_f32_u : "i32.trunc_f32_u",
        i32_trunc_f64_s : "i32.trunc_f64_s",
        i32_trunc_f64_u : "i32.trunc_f64_u",
        i64_extend_i32_s : "i64.extend_i32_s",
        i64_extend_i32_u : "i64.extend_i32_u",
        i64_trunc_f32_s : "i64.trunc_f32_s",
        i64_trunc_f32_u : "i64.trunc_f32_u",
        i64_trunc_f64_s : "i64.trunc_f64_s",
        i64_trunc_f64_u : "i64.trunc_f64_u",
        f32_convert_i32_s : "f32.convert_i32_s",
        f32_convert_i32_u : "f32.convert_i32_u",
        f32_convert_i64_s : "f32.convert_i64_s",
        f32_convert_i64_u : "f32.convert_i64_u",
        f32_demote_f64 : "f32.demote_f64",
        f64_convert_i32_s : "f64.convert_i32_s",
        f64_convert_i32_u : "f64.convert_i32_u",
        f64_convert_i64_s : "f64.convert_i64_s",
        f64_convert_i64_u : "f64.convert_i64_u",
        f64_promote_f32 : "f64.promote_f32",
        i32_reinterpret_f32 : "i32.reinterpret_f32",
        i64_reinterpret_f64 : "i64.reinterpret_f64",
        f32_reinterpret_i32 : "f32.reinterpret_i32",
        f64_reinterpret_i64 : "f64.reinterpret_i64",

        // non-trapping float to int
        i32_trunc_sat_f32_s : "i32.trunc_sat_f32_s",
        i32_trunc_sat_f32_u : "i32.trunc_sat_f32_u",
        i32_trunc_sat_f64_s : "i32.trunc_sat_f64_s",
        i32_trunc_sat_f64_u : "i32.trunc_sat_f64_u",
        i64_trunc_sat_f32_s : "i64.trunc_sat_f32_s",
        i64_trunc_sat_f32_u : "i64.trunc_sat_f32_u",
        i64_trunc_sat_f64_s : "i64.trunc_sat_f64_s",
        i64_trunc_sat_f64_u : "i64.trunc_sat_f64_u",

        // sign extension proposal
        i32_extend8_s : "i32.extend8_s",
        i32_extend16_s : "i32.extend16_s",
        i64_extend8_s : "i64.extend8_s",
        i64_extend16_s : "i64.extend16_s",
        i64_extend32_s : "i64.extend32_s",

        // atomics proposal
        memory_atomic_notify(MemArg<4>) : "memory.atomic.notify",
        memory_atomic_wait32(MemArg<4>) : "memory.atomic.wait32",
        memory_atomic_wait64(MemArg<8>) : "memory.atomic.wait64",
        atomic_fence : "atomic.fence",

        i32_atomic_load(MemArg<4>) : "i32.atomic.load",
        i64_atomic_load(MemArg<8>) : "i64.atomic.load",
        i32_atomic_load8_u(MemArg<1>) : "i32.atomic.load8_u",
        i32_atomic_load16_u(MemArg<2>) : "i32.atomic.load16_u",
        i64_atomic_load8_u(MemArg<1>) : "i64.atomic.load8_u",
        i64_atomic_load16_u(MemArg<2>) : "i64.atomic.load16_u",
        i64_atomic_load32_u(MemArg<4>) : "i64.atomic.load32_u",
        i32_atomic_store(MemArg<4>) : "i32.atomic.store",
        i64_atomic_store(MemArg<8>) : "i64.atomic.store",
        i32_atomic_store8(MemArg<1>) : "i32.atomic.store8",
        i32_atomic_store16(MemArg<2>) : "i32.atomic.store16",
        i64_atomic_store8(MemArg<1>) : "i64.atomic.store8",
        i64_atomic_store16(MemArg<2>) : "i64.atomic.store16",
        i64_atomic_store32(MemArg<4>) : "i64.atomic.store32",

        i32_atomic_rmw_add(MemArg<4>) : "i32.atomic.rmw.add",
        i64_atomic_rmw_add(MemArg<8>) : "i64.atomic.rmw.add",
        i32_atomic_rmw8_add_u(MemArg<1>) : "i32.atomic.rmw8.add_u",
        i32_atomic_rmw16_add_u(MemArg<2>) : "i32.atomic.rmw16.add_u",
        i64_atomic_rmw8_add_u(MemArg<1>) : "i64.atomic.rmw8.add_u",
        i64_atomic_rmw16_add_u(MemArg<2>) : "i64.atomic.rmw16.add_u",
        i64_atomic_rmw32_add_u(MemArg<4>) : "i64.atomic.rmw32.add_u",

        i32_atomic_rmw_sub(MemArg<4>) : "i32.atomic.rmw.sub",
        i64_atomic_rmw_sub(MemArg<8>) : "i64.atomic.rmw.sub",
        i32_atomic_rmw8_sub_u(MemArg<1>) : "i32.atomic.rmw8.sub_u",
        i32_atomic_rmw16_sub_u(MemArg<2>) : "i32.atomic.rmw16.sub_u",
        i64_atomic_rmw8_sub_u(MemArg<1>) : "i64.atomic.rmw8.sub_u",
        i64_atomic_rmw16_sub_u(MemArg<2>) : "i64.atomic.rmw16.sub_u",
        i64_atomic_rmw32_sub_u(MemArg<4>) : "i64.atomic.rmw32.sub_u",

        i32_atomic_rmw_and(MemArg<4>) : "i32.atomic.rmw.and",
        i64_atomic_rmw_and(MemArg<8>) : "i64.atomic.rmw.and",
        i32_atomic_rmw8_and_u(MemArg<1>) : "i32.atomic.rmw8.and_u",
        i32_atomic_rmw16_and_u(MemArg<2>) : "i32.atomic.rmw16.and_u",
        i64_atomic_rmw8_and_u(MemArg<1>) : "i64.atomic.rmw8.and_u",
        i64_atomic_rmw16_and_u(MemArg<2>) : "i64.atomic.rmw16.and_u",
        i64_atomic_rmw32_and_u(MemArg<4>) : "i64.atomic.rmw32.and_u",

        i32_atomic_rmw_or(MemArg<4>) : "i32.atomic.rmw.or",
        i64_atomic_rmw_or(MemArg<8>) : "i64.atomic.rmw.or",
        i32_atomic_rmw8_or_u(MemArg<1>) : "i32.atomic.rmw8.or_u",
        i32_atomic_rmw16_or_u(MemArg<2>) : "i32.atomic.rmw16.or_u",
        i64_atomic_rmw8_or_u(MemArg<1>) : "i64.atomic.rmw8.or_u",
        i64_atomic_rmw16_or_u(MemArg<2>) : "i64.atomic.rmw16.or_u",
        i64_atomic_rmw32_or_u(MemArg<4>) : "i64.atomic.rmw32.or_u",

        i32_atomic_rmw_xor(MemArg<4>) : "i32.atomic.rmw.xor",
        i64_atomic_rmw_xor(MemArg<8>) : "i64.atomic.rmw.xor",
        i32_atomic_rmw8_xor_u(MemArg<1>) : "i32.atomic.rmw8.xor_u",
        i32_atomic_rmw16_xor_u(MemArg<2>) : "i32.atomic.rmw16.xor_u",
        i64_atomic_rmw8_xor_u(MemArg<1>) : "i64.atomic.rmw8.xor_u",
        i64_atomic_rmw16_xor_u(MemArg<2>) : "i64.atomic.rmw16.xor_u",
        i64_atomic_rmw32_xor_u(MemArg<4>) : "i64.atomic.rmw32.xor_u",

        i32_atomic_rmw_xchg(MemArg<4>) : "i32.atomic.rmw.xchg",
        i64_atomic_rmw_xchg(MemArg<8>) : "i64.atomic.rmw.xchg",
        i32_atomic_rmw8_xchg_u(MemArg<1>) : "i32.atomic.rmw8.xchg_u",
        i32_atomic_rmw16_xchg_u(MemArg<2>) : "i32.atomic.rmw16.xchg_u",
        i64_atomic_rmw8_xchg_u(MemArg<1>) : "i64.atomic.rmw8.xchg_u",
        i64_atomic_rmw16_xchg_u(MemArg<2>) : "i64.atomic.rmw16.xchg_u",
        i64_atomic_rmw32_xchg_u(MemArg<4>) : "i64.atomic.rmw32.xchg_u",

        i32_atomic_rmw_cmpxchg(MemArg<4>) : "i32.atomic.rmw.cmpxchg",
        i64_atomic_rmw_cmpxchg(MemArg<8>) : "i64.atomic.rmw.cmpxchg",
        i32_atomic_rmw8_cmpxchg_u(MemArg<1>) : "i32.atomic.rmw8.cmpxchg_u",
        i32_atomic_rmw16_cmpxchg_u(MemArg<2>) : "i32.atomic.rmw16.cmpxchg_u",
        i64_atomic_rmw8_cmpxchg_u(MemArg<1>) : "i64.atomic.rmw8.cmpxchg_u",
        i64_atomic_rmw16_cmpxchg_u(MemArg<2>) : "i64.atomic.rmw16.cmpxchg_u",
        i64_atomic_rmw32_cmpxchg_u(MemArg<4>) : "i64.atomic.rmw32.cmpxchg_u",

        // proposal: shared-everything-threads
        #[custom_encode]
        global_atomic_get(Ordered<Index<'a>>) : "global.atomic.get",
        #[custom_encode]
        global_atomic_set(Ordered<Index<'a>>) : "global.atomic.set",
        #[custom_encode]
        global_atomic_rmw_add(Ordered<Index<'a>>) : "global.atomic.rmw.add",
        #[custom_encode]
        global_atomic_rmw_sub(Ordered<Index<'a>>) : "global.atomic.rmw.sub",
        #[custom_encode]
        global_atomic_rmw_and(Ordered<Index<'a>>) : "global.atomic.rmw.and",
        #[custom_encode]
        global_atomic_rmw_or(Ordered<Index<'a>>) : "global.atomic.rmw.or",
        #[custom_encode]
        global_atomic_rmw_xor(Ordered<Index<'a>>) : "global.atomic.rmw.xor",
        #[custom_encode]
        global_atomic_rmw_xchg(Ordered<Index<'a>>) : "global.atomic.rmw.xchg",
        #[custom_encode]
        global_atomic_rmw_cmpxchg(Ordered<Index<'a>>) : "global.atomic.rmw.cmpxchg",
        #[custom_encode]
        table_atomic_get(Ordered<TableArg<'a>>) : "table.atomic.get",
        #[custom_encode]
        table_atomic_set(Ordered<TableArg<'a>>) : "table.atomic.set",
        #[custom_encode]
        table_atomic_rmw_xchg(Ordered<TableArg<'a>>) : "table.atomic.rmw.xchg",
        #[custom_encode]
        table_atomic_rmw_cmpxchg(Ordered<TableArg<'a>>) : "table.atomic.rmw.cmpxchg",
        #[custom_encode]
        struct_atomic_get(Ordered<StructAccess<'a>>) : "struct.atomic.get",
        #[custom_encode]
        struct_atomic_get_s(Ordered<StructAccess<'a>>) : "struct.atomic.get_s",
        #[custom_encode]
        struct_atomic_get_u(Ordered<StructAccess<'a>>) : "struct.atomic.get_u",
        #[custom_encode]
        struct_atomic_set(Ordered<StructAccess<'a>>) : "struct.atomic.set",
        #[custom_encode]
        struct_atomic_rmw_add(Ordered<StructAccess<'a>>) : "struct.atomic.rmw.add",
        #[custom_encode]
        struct_atomic_rmw_sub(Ordered<StructAccess<'a>>) : "struct.atomic.rmw.sub",
        #[custom_encode]
        struct_atomic_rmw_and(Ordered<StructAccess<'a>>) : "struct.atomic.rmw.and",
        #[custom_encode]
        struct_atomic_rmw_or(Ordered<StructAccess<'a>>) : "struct.atomic.rmw.or",
        #[custom_encode]
        struct_atomic_rmw_xor(Ordered<StructAccess<'a>>) : "struct.atomic.rmw.xor",
        #[custom_encode]
        struct_atomic_rmw_xchg(Ordered<StructAccess<'a>>) : "struct.atomic.rmw.xchg",
        #[custom_encode]
        struct_atomic_rmw_cmpxchg(Ordered<StructAccess<'a>>) : "struct.atomic.rmw.cmpxchg",
        #[custom_encode]
        array_atomic_get(Ordered<Index<'a>>) : "array.atomic.get",
        #[custom_encode]
        array_atomic_get_s(Ordered<Index<'a>>) : "array.atomic.get_s",
        #[custom_encode]
        array_atomic_get_u(Ordered<Index<'a>>) : "array.atomic.get_u",
        #[custom_encode]
        array_atomic_set(Ordered<Index<'a>>) : "array.atomic.set",
        #[custom_encode]
        array_atomic_rmw_add(Ordered<Index<'a>>) : "array.atomic.rmw.add",
        #[custom_encode]
        array_atomic_rmw_sub(Ordered<Index<'a>>) : "array.atomic.rmw.sub",
        #[custom_encode]
        array_atomic_rmw_and(Ordered<Index<'a>>) : "array.atomic.rmw.and",
        #[custom_encode]
        array_atomic_rmw_or(Ordered<Index<'a>>) : "array.atomic.rmw.or",
        #[custom_encode]
        array_atomic_rmw_xor(Ordered<Index<'a>>) : "array.atomic.rmw.xor",
        #[custom_encode]
        array_atomic_rmw_xchg(Ordered<Index<'a>>) : "array.atomic.rmw.xchg",
        #[custom_encode]
        array_atomic_rmw_cmpxchg(Ordered<Index<'a>>) : "array.atomic.rmw.cmpxchg",
        ref_i31_shared : "ref.i31_shared",

        // proposal: simd
        //
        // https://webassembly.github.io/simd/core/binary/instructions.html
        v128_load(MemArg<16>) : "v128.load",
        v128_load8x8_s(MemArg<8>) : "v128.load8x8_s",
        v128_load8x8_u(MemArg<8>) : "v128.load8x8_u",
        v128_load16x4_s(MemArg<8>) : "v128.load16x4_s",
        v128_load16x4_u(MemArg<8>) : "v128.load16x4_u",
        v128_load32x2_s(MemArg<8>) : "v128.load32x2_s",
        v128_load32x2_u(MemArg<8>) : "v128.load32x2_u",
        v128_load8_splat(MemArg<1>) : "v128.load8_splat",
        v128_load16_splat(MemArg<2>) : "v128.load16_splat",
        v128_load32_splat(MemArg<4>) : "v128.load32_splat",
        v128_load64_splat(MemArg<8>) : "v128.load64_splat",
        v128_load32_zero(MemArg<4>) : "v128.load32_zero",
        v128_load64_zero(MemArg<8>) : "v128.load64_zero",
        v128_store(MemArg<16>) : "v128.store",

        #[custom_encode]
        v128_load8_lane(LoadOrStoreLane<1>) : "v128.load8_lane",
        #[custom_encode]
        v128_load16_lane(LoadOrStoreLane<2>) : "v128.load16_lane",
        #[custom_encode]
        v128_load32_lane(LoadOrStoreLane<4>) : "v128.load32_lane",
        #[custom_encode]
        v128_load64_lane(LoadOrStoreLane<8>) : "v128.load64_lane",
        #[custom_encode]
        v128_store8_lane(LoadOrStoreLane<1>) : "v128.store8_lane",
        #[custom_encode]
        v128_store16_lane(LoadOrStoreLane<2>) : "v128.store16_lane",
        #[custom_encode]
        v128_store32_lane(LoadOrStoreLane<4>) : "v128.store32_lane",
        #[custom_encode]
        v128_store64_lane(LoadOrStoreLane<8>) : "v128.store64_lane",

        v128_const(V128Const) : "v128.const",
        i8x16_shuffle(I8x16Shuffle) : "i8x16.shuffle",

        i8x16_extract_lane_s(LaneArg) : "i8x16.extract_lane_s",
        i8x16_extract_lane_u(LaneArg) : "i8x16.extract_lane_u",
        i8x16_replace_lane(LaneArg) : "i8x16.replace_lane",
        i16x8_extract_lane_s(LaneArg) : "i16x8.extract_lane_s",
        i16x8_extract_lane_u(LaneArg) : "i16x8.extract_lane_u",
        i16x8_replace_lane(LaneArg) : "i16x8.replace_lane",
        i32x4_extract_lane(LaneArg) : "i32x4.extract_lane",
        i32x4_replace_lane(LaneArg) : "i32x4.replace_lane",
        i64x2_extract_lane(LaneArg) : "i64x2.extract_lane",
        i64x2_replace_lane(LaneArg) : "i64x2.replace_lane",
        f32x4_extract_lane(LaneArg) : "f32x4.extract_lane",
        f32x4_replace_lane(LaneArg) : "f32x4.replace_lane",
        f64x2_extract_lane(LaneArg) : "f64x2.extract_lane",
        f64x2_replace_lane(LaneArg) : "f64x2.replace_lane",

        i8x16_swizzle : "i8x16.swizzle",
        i8x16_splat : "i8x16.splat",
        i16x8_splat : "i16x8.splat",
        i32x4_splat : "i32x4.splat",
        i64x2_splat : "i64x2.splat",
        f32x4_splat : "f32x4.splat",
        f64x2_splat : "f64x2.splat",

        i8x16_eq : "i8x16.eq",
        i8x16_ne : "i8x16.ne",
        i8x16_lt_s : "i8x16.lt_s",
        i8x16_lt_u : "i8x16.lt_u",
        i8x16_gt_s : "i8x16.gt_s",
        i8x16_gt_u : "i8x16.gt_u",
        i8x16_le_s : "i8x16.le_s",
        i8x16_le_u : "i8x16.le_u",
        i8x16_ge_s : "i8x16.ge_s",
        i8x16_ge_u : "i8x16.ge_u",

        i16x8_eq : "i16x8.eq",
        i16x8_ne : "i16x8.ne",
        i16x8_lt_s : "i16x8.lt_s",
        i16x8_lt_u : "i16x8.lt_u",
        i16x8_gt_s : "i16x8.gt_s",
        i16x8_gt_u : "i16x8.gt_u",
        i16x8_le_s : "i16x8.le_s",
        i16x8_le_u : "i16x8.le_u",
        i16x8_ge_s : "i16x8.ge_s",
        i16x8_ge_u : "i16x8.ge_u",

        i32x4_eq : "i32x4.eq",
        i32x4_ne : "i32x4.ne",
        i32x4_lt_s : "i32x4.lt_s",
        i32x4_lt_u : "i32x4.lt_u",
        i32x4_gt_s : "i32x4.gt_s",
        i32x4_gt_u : "i32x4.gt_u",
        i32x4_le_s : "i32x4.le_s",
        i32x4_le_u : "i32x4.le_u",
        i32x4_ge_s : "i32x4.ge_s",
        i32x4_ge_u : "i32x4.ge_u",

        i64x2_eq : "i64x2.eq",
        i64x2_ne : "i64x2.ne",
        i64x2_lt_s : "i64x2.lt_s",
        i64x2_gt_s : "i64x2.gt_s",
        i64x2_le_s : "i64x2.le_s",
        i64x2_ge_s : "i64x2.ge_s",

        f32x4_eq : "f32x4.eq",
        f32x4_ne : "f32x4.ne",
        f32x4_lt : "f32x4.lt",
        f32x4_gt : "f32x4.gt",
        f32x4_le : "f32x4.le",
        f32x4_ge : "f32x4.ge",

        f64x2_eq : "f64x2.eq",
        f64x2_ne : "f64x2.ne",
        f64x2_lt : "f64x2.lt",
        f64x2_gt : "f64x2.gt",
        f64x2_le : "f64x2.le",
        f64x2_ge : "f64x2.ge",

        v128_not : "v128.not",
        v128_and : "v128.and",
        v128_andnot : "v128.andnot",
        v128_or : "v128.or",
        v128_xor : "v128.xor",
        v128_bitselect : "v128.bitselect",
        v128_any_true : "v128.any_true",

        i8x16_abs : "i8x16.abs",
        i8x16_neg : "i8x16.neg",
        i8x16_popcnt : "i8x16.popcnt",
        i8x16_all_true : "i8x16.all_true",
        i8x16_bitmask : "i8x16.bitmask",
        i8x16_narrow_i16x8_s : "i8x16.narrow_i16x8_s",
        i8x16_narrow_i16x8_u : "i8x16.narrow_i16x8_u",
        i8x16_shl : "i8x16.shl",
        i8x16_shr_s : "i8x16.shr_s",
        i8x16_shr_u : "i8x16.shr_u",
        i8x16_add : "i8x16.add",
        i8x16_add_sat_s : "i8x16.add_sat_s",
        i8x16_add_sat_u : "i8x16.add_sat_u",
        i8x16_sub : "i8x16.sub",
        i8x16_sub_sat_s : "i8x16.sub_sat_s",
        i8x16_sub_sat_u : "i8x16.sub_sat_u",
        i8x16_min_s : "i8x16.min_s",
        i8x16_min_u : "i8x16.min_u",
        i8x16_max_s : "i8x16.max_s",
        i8x16_max_u : "i8x16.max_u",
        i8x16_avgr_u : "i8x16.avgr_u",

        i16x8_extadd_pairwise_i8x16_s : "i16x8.extadd_pairwise_i8x16_s",
        i16x8_extadd_pairwise_i8x16_u : "i16x8.extadd_pairwise_i8x16_u",
        i16x8_abs : "i16x8.abs",
        i16x8_neg : "i16x8.neg",
        i16x8_q15mulr_sat_s : "i16x8.q15mulr_sat_s",
        i16x8_all_true : "i16x8.all_true",
        i16x8_bitmask : "i16x8.bitmask",
        i16x8_narrow_i32x4_s : "i16x8.narrow_i32x4_s",
        i16x8_narrow_i32x4_u : "i16x8.narrow_i32x4_u",
        i16x8_extend_low_i8x16_s : "i16x8.extend_low_i8x16_s",
        i16x8_extend_high_i8x16_s : "i16x8.extend_high_i8x16_s",
        i16x8_extend_low_i8x16_u : "i16x8.extend_low_i8x16_u",
        i16x8_extend_high_i8x16_u : "i16x8.extend_high_i8x16_u",
        i16x8_shl : "i16x8.shl",
        i16x8_shr_s : "i16x8.shr_s",
        i16x8_shr_u : "i16x8.shr_u",
        i16x8_add : "i16x8.add",
        i16x8_add_sat_s : "i16x8.add_sat_s",
        i16x8_add_sat_u : "i16x8.add_sat_u",
        i16x8_sub : "i16x8.sub",
        i16x8_sub_sat_s : "i16x8.sub_sat_s",
        i16x8_sub_sat_u : "i16x8.sub_sat_u",
        i16x8_mul : "i16x8.mul",
        i16x8_min_s : "i16x8.min_s",
        i16x8_min_u : "i16x8.min_u",
        i16x8_max_s : "i16x8.max_s",
        i16x8_max_u : "i16x8.max_u",
        i16x8_avgr_u : "i16x8.avgr_u",
        i16x8_extmul_low_i8x16_s : "i16x8.extmul_low_i8x16_s",
        i16x8_extmul_high_i8x16_s : "i16x8.extmul_high_i8x16_s",
        i16x8_extmul_low_i8x16_u : "i16x8.extmul_low_i8x16_u",
        i16x8_extmul_high_i8x16_u : "i16x8.extmul_high_i8x16_u",

        i32x4_extadd_pairwise_i16x8_s : "i32x4.extadd_pairwise_i16x8_s",
        i32x4_extadd_pairwise_i16x8_u : "i32x4.extadd_pairwise_i16x8_u",
        i32x4_abs : "i32x4.abs",
        i32x4_neg : "i32x4.neg",
        i32x4_all_true : "i32x4.all_true",
        i32x4_bitmask : "i32x4.bitmask",
        i32x4_extend_low_i16x8_s : "i32x4.extend_low_i16x8_s",
        i32x4_extend_high_i16x8_s : "i32x4.extend_high_i16x8_s",
        i32x4_extend_low_i16x8_u : "i32x4.extend_low_i16x8_u",
        i32x4_extend_high_i16x8_u : "i32x4.extend_high_i16x8_u",
        i32x4_shl : "i32x4.shl",
        i32x4_shr_s : "i32x4.shr_s",
        i32x4_shr_u : "i32x4.shr_u",
        i32x4_add : "i32x4.add",
        i32x4_sub : "i32x4.sub",
        i32x4_mul : "i32x4.mul",
        i32x4_min_s : "i32x4.min_s",
        i32x4_min_u : "i32x4.min_u",
        i32x4_max_s : "i32x4.max_s",
        i32x4_max_u : "i32x4.max_u",
        i32x4_dot_i16x8_s : "i32x4.dot_i16x8_s",
        i32x4_extmul_low_i16x8_s : "i32x4.extmul_low_i16x8_s",
        i32x4_extmul_high_i16x8_s : "i32x4.extmul_high_i16x8_s",
        i32x4_extmul_low_i16x8_u : "i32x4.extmul_low_i16x8_u",
        i32x4_extmul_high_i16x8_u : "i32x4.extmul_high_i16x8_u",

        i64x2_abs : "i64x2.abs",
        i64x2_neg : "i64x2.neg",
        i64x2_all_true : "i64x2.all_true",
        i64x2_bitmask : "i64x2.bitmask",
        i64x2_extend_low_i32x4_s : "i64x2.extend_low_i32x4_s",
        i64x2_extend_high_i32x4_s : "i64x2.extend_high_i32x4_s",
        i64x2_extend_low_i32x4_u : "i64x2.extend_low_i32x4_u",
        i64x2_extend_high_i32x4_u : "i64x2.extend_high_i32x4_u",
        i64x2_shl : "i64x2.shl",
        i64x2_shr_s : "i64x2.shr_s",
        i64x2_shr_u : "i64x2.shr_u",
        i64x2_add : "i64x2.add",
        i64x2_sub : "i64x2.sub",
        i64x2_mul : "i64x2.mul",
        i64x2_extmul_low_i32x4_s : "i64x2.extmul_low_i32x4_s",
        i64x2_extmul_high_i32x4_s : "i64x2.extmul_high_i32x4_s",
        i64x2_extmul_low_i32x4_u : "i64x2.extmul_low_i32x4_u",
        i64x2_extmul_high_i32x4_u : "i64x2.extmul_high_i32x4_u",

        f32x4_ceil : "f32x4.ceil",
        f32x4_floor : "f32x4.floor",
        f32x4_trunc : "f32x4.trunc",
        f32x4_nearest : "f32x4.nearest",
        f32x4_abs : "f32x4.abs",
        f32x4_neg : "f32x4.neg",
        f32x4_sqrt : "f32x4.sqrt",
        f32x4_add : "f32x4.add",
        f32x4_sub : "f32x4.sub",
        f32x4_mul : "f32x4.mul",
        f32x4_div : "f32x4.div",
        f32x4_min : "f32x4.min",
        f32x4_max : "f32x4.max",
        f32x4_pmin : "f32x4.pmin",
        f32x4_pmax : "f32x4.pmax",

        f64x2_ceil : "f64x2.ceil",
        f64x2_floor : "f64x2.floor",
        f64x2_trunc : "f64x2.trunc",
        f64x2_nearest : "f64x2.nearest",
        f64x2_abs : "f64x2.abs",
        f64x2_neg : "f64x2.neg",
        f64x2_sqrt : "f64x2.sqrt",
        f64x2_add : "f64x2.add",
        f64x2_sub : "f64x2.sub",
        f64x2_mul : "f64x2.mul",
        f64x2_div : "f64x2.div",
        f64x2_min : "f64x2.min",
        f64x2_max : "f64x2.max",
        f64x2_pmin : "f64x2.pmin",
        f64x2_pmax : "f64x2.pmax",

        i32x4_trunc_sat_f32x4_s : "i32x4.trunc_sat_f32x4_s",
        i32x4_trunc_sat_f32x4_u : "i32x4.trunc_sat_f32x4_u",
        f32x4_convert_i32x4_s : "f32x4.convert_i32x4_s",
        f32x4_convert_i32x4_u : "f32x4.convert_i32x4_u",
        i32x4_trunc_sat_f64x2_s_zero : "i32x4.trunc_sat_f64x2_s_zero",
        i32x4_trunc_sat_f64x2_u_zero : "i32x4.trunc_sat_f64x2_u_zero",
        f64x2_convert_low_i32x4_s : "f64x2.convert_low_i32x4_s",
        f64x2_convert_low_i32x4_u : "f64x2.convert_low_i32x4_u",
        f32x4_demote_f64x2_zero : "f32x4.demote_f64x2_zero",
        f64x2_promote_low_f32x4 : "f64x2.promote_low_f32x4",

        // Exception handling proposal
        throw_ref : "throw_ref",
        #[custom_encode]
        try_table(TryTable<'a>) : "try_table",
        throw(Index<'a>) : "throw",

        // Deprecated exception handling opcodes
        try_(Box<BlockType<'a>>) : "try",
        catch(Index<'a>) : "catch",
        rethrow(Index<'a>) : "rethrow",
        delegate(Index<'a>) : "delegate",
        catch_all : "catch_all",

        // Relaxed SIMD proposal
        i8x16_relaxed_swizzle : "i8x16.relaxed_swizzle",
        i32x4_relaxed_trunc_f32x4_s : "i32x4.relaxed_trunc_f32x4_s",
        i32x4_relaxed_trunc_f32x4_u : "i32x4.relaxed_trunc_f32x4_u",
        i32x4_relaxed_trunc_f64x2_s_zero : "i32x4.relaxed_trunc_f64x2_s_zero",
        i32x4_relaxed_trunc_f64x2_u_zero : "i32x4.relaxed_trunc_f64x2_u_zero",
        f32x4_relaxed_madd : "f32x4.relaxed_madd",
        f32x4_relaxed_nmadd : "f32x4.relaxed_nmadd",
        f64x2_relaxed_madd : "f64x2.relaxed_madd",
        f64x2_relaxed_nmadd : "f64x2.relaxed_nmadd",
        i8x16_relaxed_laneselect : "i8x16.relaxed_laneselect",
        i16x8_relaxed_laneselect : "i16x8.relaxed_laneselect",
        i32x4_relaxed_laneselect : "i32x4.relaxed_laneselect",
        i64x2_relaxed_laneselect : "i64x2.relaxed_laneselect",
        f32x4_relaxed_min : "f32x4.relaxed_min",
        f32x4_relaxed_max : "f32x4.relaxed_max",
        f64x2_relaxed_min : "f64x2.relaxed_min",
        f64x2_relaxed_max : "f64x2.relaxed_max",
        i16x8_relaxed_q15mulr_s : "i16x8.relaxed_q15mulr_s",
        i16x8_relaxed_dot_i8x16_i7x16_s : "i16x8.relaxed_dot_i8x16_i7x16_s",
        i32x4_relaxed_dot_i8x16_i7x16_add_s : "i32x4.relaxed_dot_i8x16_i7x16_add_s",

        // Stack switching proposal
        cont_new(Index<'a>) : "cont.new",
        #[custom_encode]
        cont_bind(ContBind<'a>) : "cont.bind",
        suspend(Index<'a>) : "suspend",
        #[custom_encode]
        resume(Resume<'a>) : "resume",
        #[custom_encode]
        resume_throw(ResumeThrow<'a>) : "resume_throw",
        #[custom_encode]
        resume_throw_ref(ResumeThrowRef<'a>) : "resume_throw_ref",
        #[custom_encode]
        switch(Switch<'a>) : "switch",

        // Wide arithmetic proposal
        i64_add128 : "i64.add128",
        i64_sub128 : "i64.sub128",
        i64_mul_wide_s : "i64.mul_wide_s",
        i64_mul_wide_u : "i64.mul_wide_u",

        // Custom descriptors
        struct_new_desc(Index<'a>) : "struct.new_desc",
        struct_new_default_desc(Index<'a>) : "struct.new_default_desc",
        ref_get_desc(Index<'a>) : "ref.get_desc",
        #[custom_encode]
        ref_cast_desc_eq(RefCastDescEq<'a>) : "ref.cast_desc_eq",
        #[custom_encode]
        br_on_cast_desc_eq(Box<BrOnCastDescEq<'a>>) : "br_on_cast_desc_eq",
        #[custom_encode]
        br_on_cast_desc_eq_fail(Box<BrOnCastDescEqFail<'a>>) : "br_on_cast_desc_eq_fail",
    }
}

// As shown in #1095 the size of this variant is somewhat performance-sensitive
// since big `*.wat` files will have a lot of these. This is a small ratchet to
// make sure that this enum doesn't become larger than it already is, although
// ideally it also wouldn't be as large as it is now.
#[test]
fn assert_instruction_not_too_large() {
    let size = std::mem::size_of::<Instruction<'_>>();
    let pointer = std::mem::size_of::<u64>();
    assert!(size <= pointer * 11);
}

impl<'a> Instruction<'a> {
    pub(crate) fn needs_data_count(&self) -> bool {
        match self {
            Instruction::memory_init(_)
            | Instruction::data_drop(_)
            | Instruction::array_new_data(_)
            | Instruction::array_init_data(_) => true,
            _ => false,
        }
    }
}

/// Extra information associated with block-related instructions.
///
/// This is used to label blocks and also annotate what types are expected for
/// the block.
#[derive(Debug, Clone)]
#[allow(missing_docs)]
pub struct BlockType<'a> {
    pub label: Option<Id<'a>>,
    pub label_name: Option<NameAnnotation<'a>>,
    pub ty: TypeUse<'a, FunctionType<'a>>,
}

impl<'a> Parse<'a> for BlockType<'a> {
    fn parse(parser: Parser<'a>) -> Result<Self> {
        Ok(BlockType {
            label: parser.parse()?,
            label_name: parser.parse()?,
            ty: parser
                .parse::<TypeUse<'a, FunctionTypeNoNames<'a>>>()?
                .into(),
        })
    }
}

/// Extra information associated with the cont.bind instruction
#[derive(Debug, Clone)]
#[allow(missing_docs)]
pub struct ContBind<'a> {
    pub argument_index: Index<'a>,
    pub result_index: Index<'a>,
}

impl<'a> Parse<'a> for ContBind<'a> {
    fn parse(parser: Parser<'a>) -> Result<Self> {
        Ok(ContBind {
            argument_index: parser.parse()?,
            result_index: parser.parse()?,
        })
    }
}

/// Extra information associated with the resume instruction
#[derive(Debug, Clone)]
#[allow(missing_docs)]
pub struct Resume<'a> {
    pub type_index: Index<'a>,
    pub table: ResumeTable<'a>,
}

impl<'a> Parse<'a> for Resume<'a> {
    fn parse(parser: Parser<'a>) -> Result<Self> {
        Ok(Resume {
            type_index: parser.parse()?,
            table: parser.parse()?,
        })
    }
}

/// Extra information associated with the resume_throw instruction
#[derive(Debug, Clone)]
#[allow(missing_docs)]
pub struct ResumeThrow<'a> {
    pub type_index: Index<'a>,
    pub tag_index: Index<'a>,
    pub table: ResumeTable<'a>,
}

impl<'a> Parse<'a> for ResumeThrow<'a> {
    fn parse(parser: Parser<'a>) -> Result<Self> {
        Ok(ResumeThrow {
            type_index: parser.parse()?,
            tag_index: parser.parse()?,
            table: parser.parse()?,
        })
    }
}

/// Extra information associated with the resume_throw_ref instruction
#[derive(Debug, Clone)]
#[allow(missing_docs)]
pub struct ResumeThrowRef<'a> {
    pub type_index: Index<'a>,
    pub table: ResumeTable<'a>,
}

impl<'a> Parse<'a> for ResumeThrowRef<'a> {
    fn parse(parser: Parser<'a>) -> Result<Self> {
        Ok(ResumeThrowRef {
            type_index: parser.parse()?,
            table: parser.parse()?,
        })
    }
}

/// Extra information associated with the switch instruction
#[derive(Debug, Clone)]
#[allow(missing_docs)]
pub struct Switch<'a> {
    pub type_index: Index<'a>,
    pub tag_index: Index<'a>,
}

impl<'a> Parse<'a> for Switch<'a> {
    fn parse(parser: Parser<'a>) -> Result<Self> {
        Ok(Switch {
            type_index: parser.parse()?,
            tag_index: parser.parse()?,
        })
    }
}

/// A representation of resume tables
#[derive(Debug, Clone)]
#[allow(missing_docs)]
pub struct ResumeTable<'a> {
    pub handlers: Vec<Handle<'a>>,
}

/// A representation of resume table entries
#[derive(Debug, Clone)]
#[allow(missing_docs)]
pub enum Handle<'a> {
    OnLabel { tag: Index<'a>, label: Index<'a> },
    OnSwitch { tag: Index<'a> },
}

impl<'a> Parse<'a> for ResumeTable<'a> {
    fn parse(parser: Parser<'a>) -> Result<Self> {
        let mut handlers = Vec::new();
        while parser.peek::<LParen>()? && parser.peek2::<kw::on>()? {
            handlers.push(parser.parens(|p| {
                p.parse::<kw::on>()?;
                let tag: Index<'a> = p.parse()?;
                if p.peek::<kw::switch>()? {
                    p.parse::<kw::switch>()?;
                    Ok(Handle::OnSwitch { tag })
                } else {
                    Ok(Handle::OnLabel {
                        tag,
                        label: p.parse()?,
                    })
                }
            })?);
        }
        Ok(ResumeTable { handlers })
    }
}

#[derive(Debug, Clone)]
#[allow(missing_docs)]
pub struct TryTable<'a> {
    pub block: Box<BlockType<'a>>,
    pub catches: Vec<TryTableCatch<'a>>,
}

impl<'a> Parse<'a> for TryTable<'a> {
    fn parse(parser: Parser<'a>) -> Result<Self> {
        let block = parser.parse()?;

        let mut catches = Vec::new();
        while parser.peek::<LParen>()?
            && (parser.peek2::<kw::catch>()?
                || parser.peek2::<kw::catch_ref>()?
                || parser.peek2::<kw::catch_all>()?
                || parser.peek2::<kw::catch_all_ref>()?)
        {
            catches.push(parser.parens(|p| {
                let kind = if parser.peek::<kw::catch_ref>()? {
                    p.parse::<kw::catch_ref>()?;
                    TryTableCatchKind::CatchRef(p.parse()?)
                } else if parser.peek::<kw::catch>()? {
                    p.parse::<kw::catch>()?;
                    TryTableCatchKind::Catch(p.parse()?)
                } else if parser.peek::<kw::catch_all>()? {
                    p.parse::<kw::catch_all>()?;
                    TryTableCatchKind::CatchAll
                } else {
                    p.parse::<kw::catch_all_ref>()?;
                    TryTableCatchKind::CatchAllRef
                };

                Ok(TryTableCatch {
                    kind,
                    label: p.parse()?,
                })
            })?);
        }

        Ok(TryTable { block, catches })
    }
}

#[derive(Debug, Clone)]
#[allow(missing_docs)]
pub enum TryTableCatchKind<'a> {
    // Catch a tagged exception, do not capture an exnref.
    Catch(Index<'a>),
    // Catch a tagged exception, and capture the exnref.
    CatchRef(Index<'a>),
    // Catch any exception, do not capture an exnref.
    CatchAll,
    // Catch any exception, and capture the exnref.
    CatchAllRef,
}

impl<'a> TryTableCatchKind<'a> {
    #[allow(missing_docs)]
    pub fn tag_index_mut(&mut self) -> Option<&mut Index<'a>> {
        match self {
            TryTableCatchKind::Catch(tag) | TryTableCatchKind::CatchRef(tag) => Some(tag),
            TryTableCatchKind::CatchAll | TryTableCatchKind::CatchAllRef => None,
        }
    }
}

#[derive(Debug, Clone)]
#[allow(missing_docs)]
pub struct TryTableCatch<'a> {
    pub kind: TryTableCatchKind<'a>,
    pub label: Index<'a>,
}

/// Extra information associated with the `br_table` instruction.
#[allow(missing_docs)]
#[derive(Debug, Clone)]
pub struct BrTableIndices<'a> {
    pub labels: Vec<Index<'a>>,
    pub default: Index<'a>,
}

impl<'a> Parse<'a> for BrTableIndices<'a> {
    fn parse(parser: Parser<'a>) -> Result<Self> {
        let mut labels = vec![parser.parse()?];
        while parser.peek::<Index>()? {
            labels.push(parser.parse()?);
        }
        let default = labels.pop().unwrap();
        Ok(BrTableIndices { labels, default })
    }
}

/// Payload for lane-related instructions. Unsigned with no + prefix.
#[derive(Debug, Clone)]
pub struct LaneArg {
    /// The lane argument.
    pub lane: u8,
}

impl<'a> Parse<'a> for LaneArg {
    fn parse(parser: Parser<'a>) -> Result<Self> {
        let lane = parser.step(|c| {
            if let Some((i, rest)) = c.integer()? {
                if i.sign() == None {
                    let (src, radix) = i.val();
                    let val = u8::from_str_radix(src, radix)
                        .map_err(|_| c.error("malformed lane index"))?;
                    Ok((val, rest))
                } else {
                    Err(c.error("unexpected token"))
                }
            } else {
                Err(c.error("expected a lane index"))
            }
        })?;
        Ok(LaneArg { lane })
    }
}

/// Payload for memory-related instructions indicating offset/alignment of
/// memory accesses.
#[derive(Debug, Clone)]
pub struct MemArg<'a> {
    /// The alignment of this access.
    ///
    /// This is not stored as a log, this is the actual alignment (e.g. 1, 2, 4,
    /// 8, etc).
    pub align: u64,
    /// The offset, in bytes of this access.
    pub offset: u64,
    /// The memory index we're accessing
    pub memory: Index<'a>,
}

impl<'a> MemArg<'a> {
    fn parse(parser: Parser<'a>, default_align: u64) -> Result<Self> {
        fn parse_field(name: &str, parser: Parser<'_>) -> Result<Option<u64>> {
            parser.step(|c| {
                let (kw, rest) = match c.keyword()? {
                    Some(p) => p,
                    None => return Ok((None, c)),
                };
                if !kw.starts_with(name) {
                    return Ok((None, c));
                }
                let kw = &kw[name.len()..];
                if !kw.starts_with('=') {
                    return Ok((None, c));
                }
                let num = &kw[1..];
                let lexer = Lexer::new(num);
                let mut pos = 0;
                if let Ok(Some(
                    token @ Token {
                        kind: TokenKind::Integer(integer_kind),
                        ..
                    },
                )) = lexer.parse(&mut pos)
                {
                    let int = token.integer(lexer.input(), integer_kind);
                    let (s, base) = int.val();
                    let value = u64::from_str_radix(s, base);
                    return match value {
                        Ok(n) => Ok((Some(n), rest)),
                        Err(_) => Err(c.error("u64 constant out of range")),
                    };
                }
                Err(c.error("expected u64 integer constant"))
            })
        }

        let memory = parser
            .parse::<Option<_>>()?
            .unwrap_or_else(|| Index::Num(0, parser.prev_span()));
        let offset = parse_field("offset", parser)?.unwrap_or(0);
        let align = match parse_field("align", parser)? {
            Some(n) if !n.is_power_of_two() => {
                return Err(parser.error("alignment must be a power of two"));
            }
            n => n.unwrap_or(default_align),
        };

        Ok(MemArg {
            offset,
            align,
            memory,
        })
    }
}

/// Extra data associated with the `loadN_lane` and `storeN_lane` instructions.
#[derive(Debug, Clone)]
pub struct LoadOrStoreLane<'a> {
    /// The memory argument for this instruction.
    pub memarg: MemArg<'a>,
    /// The lane argument for this instruction.
    pub lane: LaneArg,
}

impl<'a> LoadOrStoreLane<'a> {
    fn parse(parser: Parser<'a>, default_align: u64) -> Result<Self> {
        // This is sort of funky. The first integer we see could be the lane
        // index, but it could also be the memory index. To determine what it is
        // then if we see a second integer we need to look further.
        let has_memarg = parser.step(|c| match c.integer()? {
            Some((_, after_int)) => {
                // Two integers in a row? That means that the first one is the
                // memory index and the second must be the lane index.
                if after_int.integer()?.is_some() {
                    return Ok((true, c));
                }

                // If the first integer is trailed by `offset=...` or
                // `align=...` then this is definitely a memarg.
                if let Some((kw, _)) = after_int.keyword()? {
                    if kw.starts_with("offset=") || kw.starts_with("align=") {
                        return Ok((true, c));
                    }
                }

                // Otherwise the first integer was trailed by something that
                // didn't look like a memarg, so this must be the lane index.
                Ok((false, c))
            }

            // Not an integer here? That must mean that this must be the memarg
            // first followed by the trailing index.
            None => Ok((true, c)),
        })?;
        Ok(LoadOrStoreLane {
            memarg: if has_memarg {
                MemArg::parse(parser, default_align)?
            } else {
                MemArg {
                    align: default_align,
                    offset: 0,
                    memory: Index::Num(0, parser.prev_span()),
                }
            },
            lane: LaneArg::parse(parser)?,
        })
    }
}

/// Extra data associated with the `call_indirect` instruction.
#[derive(Debug, Clone)]
pub struct CallIndirect<'a> {
    /// The table that this call is going to be indexing.
    pub table: Index<'a>,
    /// Type type signature that this `call_indirect` instruction is using.
    pub ty: TypeUse<'a, FunctionType<'a>>,
}

impl<'a> Parse<'a> for CallIndirect<'a> {
    fn parse(parser: Parser<'a>) -> Result<Self> {
        let prev_span = parser.prev_span();
        let table: Option<_> = parser.parse()?;
        let ty = parser.parse::<TypeUse<'a, FunctionTypeNoNames<'a>>>()?;
        Ok(CallIndirect {
            table: table.unwrap_or(Index::Num(0, prev_span)),
            ty: ty.into(),
        })
    }
}

/// Extra data associated with the `table.init` instruction
#[derive(Debug, Clone)]
pub struct TableInit<'a> {
    /// The index of the table we're copying into.
    pub table: Index<'a>,
    /// The index of the element segment we're copying into a table.
    pub elem: Index<'a>,
}

impl<'a> Parse<'a> for TableInit<'a> {
    fn parse(parser: Parser<'a>) -> Result<Self> {
        let prev_span = parser.prev_span();
        let (elem, table) = if parser.peek2::<Index>()? {
            let table = parser.parse()?;
            (parser.parse()?, table)
        } else {
            (parser.parse()?, Index::Num(0, prev_span))
        };
        Ok(TableInit { table, elem })
    }
}

/// Extra data associated with the `table.copy` instruction.
#[derive(Debug, Clone)]
pub struct TableCopy<'a> {
    /// The index of the destination table to copy into.
    pub dst: Index<'a>,
    /// The index of the source table to copy from.
    pub src: Index<'a>,
}

impl<'a> Parse<'a> for TableCopy<'a> {
    fn parse(parser: Parser<'a>) -> Result<Self> {
        let (dst, src) = match parser.parse::<Option<_>>()? {
            Some(dst) => (dst, parser.parse()?),
            None => (
                Index::Num(0, parser.prev_span()),
                Index::Num(0, parser.prev_span()),
            ),
        };
        Ok(TableCopy { dst, src })
    }
}

/// Extra data associated with unary table instructions.
#[derive(Debug, Clone)]
pub struct TableArg<'a> {
    /// The index of the table argument.
    pub dst: Index<'a>,
}

// `TableArg` could be an unwrapped as an `Index` if not for this custom parse
// behavior: if we cannot parse a table index, we default to table `0`.
impl<'a> Parse<'a> for TableArg<'a> {
    fn parse(parser: Parser<'a>) -> Result<Self> {
        let dst = if let Some(dst) = parser.parse()? {
            dst
        } else {
            Index::Num(0, parser.prev_span())
        };
        Ok(TableArg { dst })
    }
}

/// Extra data associated with unary memory instructions.
#[derive(Debug, Clone)]
pub struct MemoryArg<'a> {
    /// The index of the memory space.
    pub mem: Index<'a>,
}

impl<'a> Parse<'a> for MemoryArg<'a> {
    fn parse(parser: Parser<'a>) -> Result<Self> {
        let mem = if let Some(mem) = parser.parse()? {
            mem
        } else {
            Index::Num(0, parser.prev_span())
        };
        Ok(MemoryArg { mem })
    }
}

/// Extra data associated with the `memory.init` instruction
#[derive(Debug, Clone)]
pub struct MemoryInit<'a> {
    /// The index of the data segment we're copying into memory.
    pub data: Index<'a>,
    /// The index of the memory we're copying into,
    pub mem: Index<'a>,
}

impl<'a> Parse<'a> for MemoryInit<'a> {
    fn parse(parser: Parser<'a>) -> Result<Self> {
        let prev_span = parser.prev_span();
        let (data, mem) = if parser.peek2::<Index>()? {
            let memory = parser.parse()?;
            (parser.parse()?, memory)
        } else {
            (parser.parse()?, Index::Num(0, prev_span))
        };
        Ok(MemoryInit { data, mem })
    }
}

/// Extra data associated with the `memory.copy` instruction
#[derive(Debug, Clone)]
pub struct MemoryCopy<'a> {
    /// The index of the memory we're copying from.
    pub src: Index<'a>,
    /// The index of the memory we're copying to.
    pub dst: Index<'a>,
}

impl<'a> Parse<'a> for MemoryCopy<'a> {
    fn parse(parser: Parser<'a>) -> Result<Self> {
        let (src, dst) = match parser.parse()? {
            Some(dst) => (parser.parse()?, dst),
            None => (
                Index::Num(0, parser.prev_span()),
                Index::Num(0, parser.prev_span()),
            ),
        };
        Ok(MemoryCopy { src, dst })
    }
}

/// Extra data associated with the `struct.get/set` instructions
#[derive(Debug, Clone)]
pub struct StructAccess<'a> {
    /// The index of the struct type we're accessing.
    pub r#struct: Index<'a>,
    /// The index of the field of the struct we're accessing
    pub field: Index<'a>,
}

impl<'a> Parse<'a> for StructAccess<'a> {
    fn parse(parser: Parser<'a>) -> Result<Self> {
        Ok(StructAccess {
            r#struct: parser.parse()?,
            field: parser.parse()?,
        })
    }
}

/// Extra data associated with the `array.fill` instruction
#[derive(Debug, Clone)]
pub struct ArrayFill<'a> {
    /// The index of the array type we're filling.
    pub array: Index<'a>,
}

impl<'a> Parse<'a> for ArrayFill<'a> {
    fn parse(parser: Parser<'a>) -> Result<Self> {
        Ok(ArrayFill {
            array: parser.parse()?,
        })
    }
}

/// Extra data associated with the `array.copy` instruction
#[derive(Debug, Clone)]
pub struct ArrayCopy<'a> {
    /// The index of the array type we're copying to.
    pub dest_array: Index<'a>,
    /// The index of the array type we're copying from.
    pub src_array: Index<'a>,
}

impl<'a> Parse<'a> for ArrayCopy<'a> {
    fn parse(parser: Parser<'a>) -> Result<Self> {
        Ok(ArrayCopy {
            dest_array: parser.parse()?,
            src_array: parser.parse()?,
        })
    }
}

/// Extra data associated with the `array.init_[data/elem]` instruction
#[derive(Debug, Clone)]
pub struct ArrayInit<'a> {
    /// The index of the array type we're initializing.
    pub array: Index<'a>,
    /// The index of the data or elem segment we're reading from.
    pub segment: Index<'a>,
}

impl<'a> Parse<'a> for ArrayInit<'a> {
    fn parse(parser: Parser<'a>) -> Result<Self> {
        Ok(ArrayInit {
            array: parser.parse()?,
            segment: parser.parse()?,
        })
    }
}

/// Extra data associated with the `array.new_fixed` instruction
#[derive(Debug, Clone)]
pub struct ArrayNewFixed<'a> {
    /// The index of the array type we're accessing.
    pub array: Index<'a>,
    /// The amount of values to initialize the array with.
    pub length: u32,
}

impl<'a> Parse<'a> for ArrayNewFixed<'a> {
    fn parse(parser: Parser<'a>) -> Result<Self> {
        Ok(ArrayNewFixed {
            array: parser.parse()?,
            length: parser.parse()?,
        })
    }
}

/// Extra data associated with the `array.new_data` instruction
#[derive(Debug, Clone)]
pub struct ArrayNewData<'a> {
    /// The index of the array type we're accessing.
    pub array: Index<'a>,
    /// The data segment to initialize from.
    pub data_idx: Index<'a>,
}

impl<'a> Parse<'a> for ArrayNewData<'a> {
    fn parse(parser: Parser<'a>) -> Result<Self> {
        Ok(ArrayNewData {
            array: parser.parse()?,
            data_idx: parser.parse()?,
        })
    }
}

/// Extra data associated with the `array.new_elem` instruction
#[derive(Debug, Clone)]
pub struct ArrayNewElem<'a> {
    /// The index of the array type we're accessing.
    pub array: Index<'a>,
    /// The elem segment to initialize from.
    pub elem_idx: Index<'a>,
}

impl<'a> Parse<'a> for ArrayNewElem<'a> {
    fn parse(parser: Parser<'a>) -> Result<Self> {
        Ok(ArrayNewElem {
            array: parser.parse()?,
            elem_idx: parser.parse()?,
        })
    }
}

/// Extra data associated with the `ref.cast` instruction
#[derive(Debug, Clone)]
pub struct RefCast<'a> {
    /// The type to cast to.
    pub r#type: RefType<'a>,
}

impl<'a> Parse<'a> for RefCast<'a> {
    fn parse(parser: Parser<'a>) -> Result<Self> {
        Ok(RefCast {
            r#type: parser.parse()?,
        })
    }
}

/// Extra data associated with the `ref.test` instruction
#[derive(Debug, Clone)]
pub struct RefTest<'a> {
    /// The type to test for.
    pub r#type: RefType<'a>,
}

impl<'a> Parse<'a> for RefTest<'a> {
    fn parse(parser: Parser<'a>) -> Result<Self> {
        Ok(RefTest {
            r#type: parser.parse()?,
        })
    }
}

/// Extra data associated with the `br_on_cast` instruction
#[derive(Debug, Clone)]
pub struct BrOnCast<'a> {
    /// The label to branch to.
    pub label: Index<'a>,
    /// The type we're casting from.
    pub from_type: RefType<'a>,
    /// The type we're casting to.
    pub to_type: RefType<'a>,
}

impl<'a> Parse<'a> for BrOnCast<'a> {
    fn parse(parser: Parser<'a>) -> Result<Self> {
        Ok(BrOnCast {
            label: parser.parse()?,
            from_type: parser.parse()?,
            to_type: parser.parse()?,
        })
    }
}

/// Extra data associated with the `br_on_cast_fail` instruction
#[derive(Debug, Clone)]
pub struct BrOnCastFail<'a> {
    /// The label to branch to.
    pub label: Index<'a>,
    /// The type we're casting from.
    pub from_type: RefType<'a>,
    /// The type we're casting to.
    pub to_type: RefType<'a>,
}

impl<'a> Parse<'a> for BrOnCastFail<'a> {
    fn parse(parser: Parser<'a>) -> Result<Self> {
        Ok(BrOnCastFail {
            label: parser.parse()?,
            from_type: parser.parse()?,
            to_type: parser.parse()?,
        })
    }
}

/// Extra data associated with the `ref.cast_desc` instruction
#[derive(Debug, Clone)]
pub struct RefCastDescEq<'a> {
    /// The type to cast to.
    pub r#type: RefType<'a>,
}

impl<'a> Parse<'a> for RefCastDescEq<'a> {
    fn parse(parser: Parser<'a>) -> Result<Self> {
        Ok(RefCastDescEq {
            r#type: parser.parse()?,
        })
    }
}

/// Extra data associated with the `br_on_cast_desc_eq` instruction
#[derive(Debug, Clone)]
pub struct BrOnCastDescEq<'a> {
    /// The label to branch to.
    pub label: Index<'a>,
    /// The type we're casting from.
    pub from_type: RefType<'a>,
    /// The type we're casting to.
    pub to_type: RefType<'a>,
}

impl<'a> Parse<'a> for BrOnCastDescEq<'a> {
    fn parse(parser: Parser<'a>) -> Result<Self> {
        Ok(BrOnCastDescEq {
            label: parser.parse()?,
            from_type: parser.parse()?,
            to_type: parser.parse()?,
        })
    }
}

/// Extra data associated with the `br_on_cast_desc_fail` instruction
#[derive(Debug, Clone)]
pub struct BrOnCastDescEqFail<'a> {
    /// The label to branch to.
    pub label: Index<'a>,
    /// The type we're casting from.
    pub from_type: RefType<'a>,
    /// The type we're casting to.
    pub to_type: RefType<'a>,
}

impl<'a> Parse<'a> for BrOnCastDescEqFail<'a> {
    fn parse(parser: Parser<'a>) -> Result<Self> {
        Ok(BrOnCastDescEqFail {
            label: parser.parse()?,
            from_type: parser.parse()?,
            to_type: parser.parse()?,
        })
    }
}

/// The memory ordering for atomic instructions.
///
/// For an in-depth explanation of memory orderings, see the C++ documentation
/// for [`memory_order`] or the Rust documentation for [`atomic::Ordering`].
///
/// [`memory_order`]: https://en.cppreference.com/w/cpp/atomic/memory_order
/// [`atomic::Ordering`]: https://doc.rust-lang.org/std/sync/atomic/enum.Ordering.html
#[derive(Clone, Debug)]
pub enum Ordering {
    /// Like `AcqRel` but all threads see all sequentially consistent operations
    /// in the same order.
    AcqRel,
    /// For a load, it acquires; this orders all operations before the last
    /// "releasing" store. For a store, it releases; this orders all operations
    /// before it at the next "acquiring" load.
    SeqCst,
}

impl<'a> Parse<'a> for Ordering {
    fn parse(parser: Parser<'a>) -> Result<Self> {
        if parser.peek::<kw::seqcst>()? {
            parser.parse::<kw::seqcst>()?;
            Ok(Ordering::SeqCst)
        } else if parser.peek::<kw::acqrel>()? {
            parser.parse::<kw::acqrel>()?;
            Ok(Ordering::AcqRel)
        } else {
            Err(parser.error("expected a memory ordering: `seqcst` or `acqrel`"))
        }
    }
}

/// Add a memory [`Ordering`] to the argument `T` of some instruction.
///
/// This is helpful for many kinds of `*.atomic.*` instructions introduced by
/// the shared-everything-threads proposal. Many of these instructions "build
/// on" existing instructions by simply adding a memory order to them.
#[derive(Clone, Debug)]
pub struct Ordered<T> {
    /// The memory ordering for this atomic instruction.
    pub ordering: Ordering,
    /// The original argument type.
    pub inner: T,
}

impl<'a, T> Parse<'a> for Ordered<T>
where
    T: Parse<'a>,
{
    fn parse(parser: Parser<'a>) -> Result<Self> {
        let ordering = parser.parse()?;
        let inner = parser.parse()?;
        Ok(Ordered { ordering, inner })
    }
}

/// Different ways to specify a `v128.const` instruction
#[derive(Clone, Debug)]
#[allow(missing_docs)]
pub enum V128Const {
    I8x16([i8; 16]),
    I16x8([i16; 8]),
    I32x4([i32; 4]),
    I64x2([i64; 2]),
    F32x4([F32; 4]),
    F64x2([F64; 2]),
}

impl V128Const {
    /// Returns the raw little-ended byte sequence used to represent this
    /// `v128` constant`
    ///
    /// This is typically suitable for encoding as the payload of the
    /// `v128.const` instruction.
    #[rustfmt::skip]
    pub fn to_le_bytes(&self) -> [u8; 16] {
        match self {
            V128Const::I8x16(arr) => [
                arr[0] as u8,
                arr[1] as u8,
                arr[2] as u8,
                arr[3] as u8,
                arr[4] as u8,
                arr[5] as u8,
                arr[6] as u8,
                arr[7] as u8,
                arr[8] as u8,
                arr[9] as u8,
                arr[10] as u8,
                arr[11] as u8,
                arr[12] as u8,
                arr[13] as u8,
                arr[14] as u8,
                arr[15] as u8,
            ],
            V128Const::I16x8(arr) => {
                let a1 = arr[0].to_le_bytes();
                let a2 = arr[1].to_le_bytes();
                let a3 = arr[2].to_le_bytes();
                let a4 = arr[3].to_le_bytes();
                let a5 = arr[4].to_le_bytes();
                let a6 = arr[5].to_le_bytes();
                let a7 = arr[6].to_le_bytes();
                let a8 = arr[7].to_le_bytes();
                [
                    a1[0], a1[1],
                    a2[0], a2[1],
                    a3[0], a3[1],
                    a4[0], a4[1],
                    a5[0], a5[1],
                    a6[0], a6[1],
                    a7[0], a7[1],
                    a8[0], a8[1],
                ]
            }
            V128Const::I32x4(arr) => {
                let a1 = arr[0].to_le_bytes();
                let a2 = arr[1].to_le_bytes();
                let a3 = arr[2].to_le_bytes();
                let a4 = arr[3].to_le_bytes();
                [
                    a1[0], a1[1], a1[2], a1[3],
                    a2[0], a2[1], a2[2], a2[3],
                    a3[0], a3[1], a3[2], a3[3],
                    a4[0], a4[1], a4[2], a4[3],
                ]
            }
            V128Const::I64x2(arr) => {
                let a1 = arr[0].to_le_bytes();
                let a2 = arr[1].to_le_bytes();
                [
                    a1[0], a1[1], a1[2], a1[3], a1[4], a1[5], a1[6], a1[7],
                    a2[0], a2[1], a2[2], a2[3], a2[4], a2[5], a2[6], a2[7],
                ]
            }
            V128Const::F32x4(arr) => {
                let a1 = arr[0].bits.to_le_bytes();
                let a2 = arr[1].bits.to_le_bytes();
                let a3 = arr[2].bits.to_le_bytes();
                let a4 = arr[3].bits.to_le_bytes();
                [
                    a1[0], a1[1], a1[2], a1[3],
                    a2[0], a2[1], a2[2], a2[3],
                    a3[0], a3[1], a3[2], a3[3],
                    a4[0], a4[1], a4[2], a4[3],
                ]
            }
            V128Const::F64x2(arr) => {
                let a1 = arr[0].bits.to_le_bytes();
                let a2 = arr[1].bits.to_le_bytes();
                [
                    a1[0], a1[1], a1[2], a1[3], a1[4], a1[5], a1[6], a1[7],
                    a2[0], a2[1], a2[2], a2[3], a2[4], a2[5], a2[6], a2[7],
                ]
            }
        }
    }
}

impl<'a> Parse<'a> for V128Const {
    fn parse(parser: Parser<'a>) -> Result<Self> {
        let mut l = parser.lookahead1();
        if l.peek::<kw::i8x16>()? {
            parser.parse::<kw::i8x16>()?;
            Ok(V128Const::I8x16([
                parser.parse()?,
                parser.parse()?,
                parser.parse()?,
                parser.parse()?,
                parser.parse()?,
                parser.parse()?,
                parser.parse()?,
                parser.parse()?,
                parser.parse()?,
                parser.parse()?,
                parser.parse()?,
                parser.parse()?,
                parser.parse()?,
                parser.parse()?,
                parser.parse()?,
                parser.parse()?,
            ]))
        } else if l.peek::<kw::i16x8>()? {
            parser.parse::<kw::i16x8>()?;
            Ok(V128Const::I16x8([
                parser.parse()?,
                parser.parse()?,
                parser.parse()?,
                parser.parse()?,
                parser.parse()?,
                parser.parse()?,
                parser.parse()?,
                parser.parse()?,
            ]))
        } else if l.peek::<kw::i32x4>()? {
            parser.parse::<kw::i32x4>()?;
            Ok(V128Const::I32x4([
                parser.parse()?,
                parser.parse()?,
                parser.parse()?,
                parser.parse()?,
            ]))
        } else if l.peek::<kw::i64x2>()? {
            parser.parse::<kw::i64x2>()?;
            Ok(V128Const::I64x2([parser.parse()?, parser.parse()?]))
        } else if l.peek::<kw::f32x4>()? {
            parser.parse::<kw::f32x4>()?;
            Ok(V128Const::F32x4([
                parser.parse()?,
                parser.parse()?,
                parser.parse()?,
                parser.parse()?,
            ]))
        } else if l.peek::<kw::f64x2>()? {
            parser.parse::<kw::f64x2>()?;
            Ok(V128Const::F64x2([parser.parse()?, parser.parse()?]))
        } else {
            Err(l.error())
        }
    }
}

/// Lanes being shuffled in the `i8x16.shuffle` instruction
#[derive(Debug, Clone)]
pub struct I8x16Shuffle {
    #[allow(missing_docs)]
    pub lanes: [u8; 16],
}

impl<'a> Parse<'a> for I8x16Shuffle {
    fn parse(parser: Parser<'a>) -> Result<Self> {
        Ok(I8x16Shuffle {
            lanes: [
                parser.parse()?,
                parser.parse()?,
                parser.parse()?,
                parser.parse()?,
                parser.parse()?,
                parser.parse()?,
                parser.parse()?,
                parser.parse()?,
                parser.parse()?,
                parser.parse()?,
                parser.parse()?,
                parser.parse()?,
                parser.parse()?,
                parser.parse()?,
                parser.parse()?,
                parser.parse()?,
            ],
        })
    }
}

/// Payload of the `select` instructions
#[derive(Debug, Clone)]
pub struct SelectTypes<'a> {
    #[allow(missing_docs)]
    pub tys: Option<Vec<ValType<'a>>>,
}

impl<'a> Parse<'a> for SelectTypes<'a> {
    fn parse(parser: Parser<'a>) -> Result<Self> {
        let mut found = false;
        let mut list = Vec::new();
        while parser.peek2::<kw::result>()? {
            found = true;
            parser.parens(|p| {
                p.parse::<kw::result>()?;
                while !p.is_empty() {
                    list.push(p.parse()?);
                }
                Ok(())
            })?;
        }
        Ok(SelectTypes {
            tys: if found { Some(list) } else { None },
        })
    }
}
