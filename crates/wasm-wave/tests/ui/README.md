# UI ("golden files") Tests

Each `.waves` (WAVE Script) file in this directory contains a set of test inputs; corresponding outputs are in the matching `.out` file (e.g. `test.wave` &rarr; `test.out`).

Each test input looks like `<func-name>(<WAVE value>, ...);\n` where `<func-name>` is a function defined in the `ui.wit` file. For each input the WAVE value arguments are parsed and type-checked against the function parameter types.  The result of this parsing - either a re-encoded copy of the input or an error - is written to the output.

Note that each test input must end with `;\n`. This is not part of WAVE itself; inputs are split on this exact substring so it may not appear anywhere else in inputs.

Comments at the start of a test case are copied into the output.

## Updating Outputs

By default, running the `ui` tests will check outputs against the contents of the existing `.out` files. When updating tests, the `.out` files can be overwritten by setting the environment variable `BLESS=1` when running the tests. Updated outputs should be committed and reviewed.