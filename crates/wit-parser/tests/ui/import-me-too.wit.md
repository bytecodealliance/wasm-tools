# Title

This file is like import-me.wit, but it's a Markdown file with embedded wit
code blocks.

## `foo`
```wit
/// This is foo.
type foo = u32
```

## `x`
```wit
/// This is x.
resource x
```

## `handle`
```wit
/// This is handle.
type %handle = handle x
```

## `some-record`
```wit
/// This is some-record.
type some-record = tuple<u32, u64, float32>
```
