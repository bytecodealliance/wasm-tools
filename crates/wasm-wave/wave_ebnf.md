# WAVE EBNF

A WAVE value is defined by the `value` rule below. Many applications may allow
whitespace around the value, equivalent to the `value-ws` rule.

> Note that Bool, Variant, Enum, Option and Result values are combined under
> the `variant-case` rule because these cannot be distinguished without type
> information. Likewise, Record and Map values are combined under the
> `record-or-map` rule, and `empty-braces` (`{}`) may be empty Flags or an
> empty Map (`{:}` may also be an empty Map).

```ebnf
value ::= number
        | char
        | string
        | multiline-string
        | variant-case
        | tuple
        | list
        | empty-braces
        | flags
        | record-or-map

value-ws ::= ws value ws
ws ::= ([ \t\n\r] | comment)*
comment ::= '//' [^\n]*

number ::= number_finite
         | 'nan'
         | 'inf'
         | '-inf'
number_finite ::= integer number-fraction? number-exponent?
integer ::= unsigned-integer
          | '-' unsigned-integer
unsigned-integer ::= '0'
                   | [1-9] [0-9]*
number-fraction ::= '.' [0-9]+
number-exponent ::= [eE] [+-]? [0-9]+

char ::= ['] char-char [']
char-char ::= common-char | '"'

string ::= '"' string-char* '"'
string-char ::= common-char | [']

multiline-string ::= '"""' line-break multiline-string-line* [ ]* '"""'
multiline-string-line ::= [ ]* multiline-string-char* line-break
multiline-string-char ::= common-char | ['"]

line-break ::= '\r\n' | '\n'

common-char ::= <any Unicode Scalar Value except ['"\n\\]>
              | '\' escape
escape ::= ['"tnr\\] | escape-unicode
escape-unicode ::= 'u{' [0-9a-fA-F]+ '}'

variant-case ::= label ws variant-case-payload?
variant-case-payload ::= '(' value-ws ')'

tuple ::= '(' values-seq ','? ws ')'

list ::= '[' ws ']'
       | '[' values-seq ','? ws ']'

values-seq ::= value-ws
             | values-seq ',' value-ws

empty-braces ::= '{' ws '}'

flags ::= '{' flags-seq ','? ws '}'
flags-seq ::= ws label ws
            | flags-seq ',' ws label ws

record-or-map ::= '{' ws ':' ws '}'
                | '{' entries ','? ws '}'
entries ::= ws entry ws
          | entries ',' ws entry ws
entry ::= entry-key ws ':' ws value
entry-key ::= label
            | number
            | char
            | string
            | multiline-string

label ::= '%'? first-word ('-' word)*
first-word ::= [a-z][a-z0-9]*
             | [A-Z][A-Z0-9]*
word ::= [a-z0-9]+
       | [A-Z0-9]+
```

* "`Unicode scalar value`" is defined by Unicode
* `escape-unicode` must identify a valid Unicode scalar value, using 1 to 6 hexadecimal digits.
* `multiline-string-line` must not contain `"""`
* See [README.md#multiline-strings](./README.md#multiline-strings) for the indentation and decoding rules that constrain `multiline-string`.
