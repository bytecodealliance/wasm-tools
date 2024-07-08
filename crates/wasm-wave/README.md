# WAVE: Web Assembly Value Encoding

WAVE is a human-oriented text encoding of WebAssembly Component Model values. It is designed to be consistent with the
[WIT IDL format](https://github.com/WebAssembly/component-model/blob/main/design/mvp/WIT.md).

|Type|Example Values
|---|---
|Bools|`true`, `false`
|Integers|`123`, `-9`
|Floats|`3.14`, `6.022e+23`, `nan`, `-inf`
|Chars|`'x'`, `'☃︎'`, `'\''`, `'\u{0}'`
|Strings|`"abc\t123"`
|Tuples|`("abc", 123)`
|Lists|`[1, 2, 3]`
|Records|`{field-a: 1, field-b: "two"}`
|Variants|`days(30)`, `forever`
|Enums|`south`, `west`
|Options|`"flat some"`, `some("explicit some")`, `none`
|Results|`"flat ok"`, `ok("explicit ok")`, `err("oops")`
|Flags|`{read, write}`, `{}`

## Usage

```rust
use wasmtime::component::{Type, Val};

let val: Val = wasm_wave::from_str(&Type::String, "\"👋 Hello, world! 👋\"").unwrap();
println!("{}", wasm_wave::to_string(&val).unwrap());
```

→ `"👋 Hello, world! 👋"`

## Encoding

Values are encoded as Unicode text. UTF-8 should be used wherever an interoperable binary encoding is required.

### Whitespace

Whitespace is _insignificant between tokens_ and _significant within tokens_: keywords, labels, chars, and strings.

### Comments

Comments start with `//` and run to the end of the line.

### Keywords

Several tokens are reserved WAVE keywords: `true`, `false`, `inf`, `nan`, `some`, `none`, `ok`, `err`. Variant or enum cases that match one of these keywords must be prefixed with `%`.

### Labels

Kebab-case labels are used for record fields, variant cases, enum cases, and flags. Labels use ASCII alphanumeric characters and hyphens, following the [Wit identifier syntax](https://github.com/WebAssembly/component-model/blob/main/design/mvp/WIT.md#identifiers):

- Labels consist of one or more hypen-separated words.
  - `one`, `two-words`
- Words consist of one ASCII letter followed by any number of ASCII alphanumeric characters.
  - `q`, `abc123`
- Each word can contain lowercase or uppercase characters but not both; each word in a label can use a different (single) case.
  - `HTTP3`, `method-GET`
- Any label may be prefixed with `%`; this is not part of the label itself but allows for representing labels that would otherwise be parsed as keywords.
  - `%err`, `%non-keyword`

### Bools

Bools are encoded as one of the keywords `false` or `true`.

### Integers

Integers are encoded as base-10 numbers.

> TBD: hex/bin repr? e.g. `0xab`, `0b1011`

### Floats

Floats are encoded as JSON numbers or one of the keywords `nan`, (not a number) `inf` (infinity), or `-inf` (negative infinity).

### Chars

Chars are encoded as `'<char>'`, where `<char>` is one of:

- a single [Unicode Scalar Value](https://unicode.org/glossary/#unicode_scalar_value)
- one of these escapes:
  - `\'` → `'`
  - `\"` → `"`
  - `\\` → `\`
  - `\t` → U+9 (HT)
  - `\n` → U+A (LF)
  - `\r` → U+D (CR)
  - `\u{···}` → U+··· (where `···` is a hexadecimal Unicode Scalar Value)

Escaping newline (`\n`), `\`, and `'` is mandatory for chars.

### Strings

Strings are encoded as a double-quote-delimited sequence of `<char>`s (as for [Chars](#chars)).

Escaping newline (`\n`), `\`, and `"` is mandatory for strings.

### Multiline Strings

A multiline string begins with `"""` followed immediately by a line break (`\n` or `\r\n`) and ends with a line break, zero or more spaces, and `"""`. The number of spaces immediately preceding the ending `"""` determines the indent level of the entire multiline string. Every other line break in the string must be followed by at least this many spaces which are then omitted ("dedented") from the decoded string.

Each line break in the encoded string except for the first and last is decoded as a newline character (`\n`).

Escaping `\` is mandatory for multiline strings. Escaping carriage return (`\r`) is mandatory immediately before a literal newline character (`\n`) if it is to be retained. Escaping `"` is mandatory where necessary to break up any sequence of `"""` within a string, even if the first `"` is escaped (i.e. `\"""` is prohibited).

```python
"""
A single line
"""
```
→ `"A single line"`

```python
"""
    Indentation determined
      by ending delimiter
  """
```
→
```clike
"  Indentation determined\n    by ending delimiter"
```

```python
"""
  Must escape carriage return at end of line: \r
  Must break up double quote triplets: ""\""
  """
```
→
```clike
"Must escape carriage return at end of line: \r\nMust break up double quote triplets: \"\"\"\""
```

### Tuples

Tuples are encoded as a parenthesized sequence of comma-separated values. Trailing commas are permitted.

`tuple<u8, string>` → `(123, "abc")`

### Lists

Lists are encoded as a square-braketed sequence of comma-separated values. Trailing commas are permitted.

`list<char>` → `[]`, `['a', 'b', 'c']`

### Records

Records are encoded as curly-braced set of comma-separated record entries. Trailing commas are permitted. Each record entry consists of a field label, a colon, and a value. Fields may be present in any order. Record entries with the `option`-typed value `none` may be omitted; if all of a record's fields are omitted in this way the "empty" record must be encoded as `{:}` (to disambiguate from an empty `flags` value).

```clike
record example {
  must-have: u8,
  optional: option<u8>,
}
```

→ `{must-have: 123}` = `{must-have: 123, optional: none,}`

```clike
record all-optional {
  optional: option<u8>,
}
```

→ `{:}` = `{optional: none}`

> Note: Field labels _may_ be prefixed with `%` but this is never required.

### Variants

Variants are encoded as a case label. If the case has a payload, the label is followed by the parenthesized case payload value.

If a variant case matches a WAVE keyword it must be prefixed with `%`.

```clike
variant response {
  empty,
  body(list<u8>),
  err(string),
}
```

→ `empty`, `body([79, 75])`, `%err("oops")`

### Enums

Enums are encoded as a case label.

If an enum case matches a WAVE keyword it must be prefixed with `%`.

`enum status { ok, not-found }` → `%ok`, `not-found`

### Options

Options may be encoded in their variant form (e.g. `some(...)` or `none`). A `some` value may also be encoded as the "flat" payload value itself, but only if the payload is not an option or result type.

- `option<u8>` → `123` = `some(123)`

### Results

Results may be encoded in their variant form (e.g. `ok(...)`, `err("oops")`). An `ok` value may also be encoded as the "flat" payload value itself, but only if it has a payload which is not an option or result type.

- `result<u8>` → `123` = `ok(123)`
- `result<_, string>` → `ok`, `err("oops")`
- `result` → `ok`, `err`

### Flags

Flags are encoded as a curly-braced set of comma-separated flag labels in any order. Trailing commas are permitted.

`flags perms { read, write, exec }` → `{write, read,}`

> Note: Flags _may_ be prefixed with `%` but this is never required.

> TBD: Allow record form? `{read: true, write: true, exec: false}`

### Resources

> TBD (`<named-type>(<idx>)`?)

## Appendix: Function calls

Some applications may benefit from a standard way to encode function calls and/or results, described here.

Function calls can be encoded as some application-dependent function identifier (such as a kebab-case label) followed by parenthesized function arguments.

If function results need to be encoded along with the call, they can be separated from the call by `->`.

```clike
my-func("param")
    
with-result() -> ok("result")
```

### Function arguments

Arguments are encoded as a sequence of comma-separated values.

Any number of `option` `none` values at the end of the sequence may be omitted.

```clike
// f: func(a: option<u8>, b: option<u8>, c: option<u8>)
// all equivalent:
f(some(1))
f(some(1), none)
f(some(1), none, none)
```

> TBD: Named-parameter encoding? e.g.
> `my-func(named-param: 1)`
> Could allow omitting "middle" `none` params.

### Function results

Results are encoded in one of several ways depending on the number of result values:

- Any number of result values may be encoded as a parenthesized sequence of comma-separated result entries. Each result entry consists of a label (for named results) or zero-based index number, a colon, and the result value. Result entry ordering must match the function definition.
- Zero result values are encoded as `()` or omitted entirely.
- A single result value may be encoded as the "flat" result value itself.

```clike
-> ()

-> some("single result")
// or
-> (0: some("single result"))
    
-> (result-a: "abc", result-b: 123)
```

---

:ocean:
