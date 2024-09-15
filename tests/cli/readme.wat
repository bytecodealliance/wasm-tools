;; This is intended to be a self-documenting test which explains what's
;; possible in directives for this test suite in `tests/cli/*`. The purpose of
;; this test suite is to make it as easy as dropping a file in this directory to
;; test the `wasm-tools` CLI tool and its subcommands. The test file itself is
;; generally the input to the test and what's being tested will be present in
;; comments at the top of the file with directives.
;;
;; All test directives must come in comments at the start of the file:
;;
;; RUN: validate %
;;
;; The `RUN` prefix indicates that the specified `wasm-tools` subcommand should
;; be executed. It's possible to have more than one test in a file by having
;; named directives such as:
;;
;; RUN[validate-again]: validate %
;;
;; Directive names must be unique, so using `validate-again` would not be valid.
;; Additionally you can't use an unprefixed directive more than once so using
;; `RUN: ...` here again would not be allowed for example.
;;
;; You can also use the `FAIL` directive to indicate that the subcommand should
;; fail rather than succeed.
;;
;; FAIL[should-fail]: validate % --features=-simd
;;
;; As you can see directives can have comments around them. Directives are
;; identified as comment lines starting with `RUN` or `FAIL`.
;;
;; Within directives there are a few feature. First as you've seen the `%` value
;; will be substituted with the current filename which means:
;;
;; RUN[subst]: validate %
;;
;; means to run `wasm-tools validate tests/cli/readme.wat` and test the result
;; is successful.
;;
;; You can additionally use `|` to pipe commands together by feeding the stdout
;; of the previous command into the stdin of the next command.
;;
;; RUN[pipe]: print % | validate
;;
;; Note that when piping commands the intermediate commands before the final
;; one, in this case `print` being the intermediate, must all succeed.
;;
;; Tests also assert the stdout/stderr of the command being tested. For example
;; if printing is tested:
;;
;; RUN[print]: print %
;;
;; then this tests that `tests/cli/readme.wat.print.stdout` is the result of
;; `wasm-tools print tests/cli/readme.wat`. Note that this can be tedious to
;; update so you can use the environment variable `BLESS=1` to automatically
;; update all test assertions. This can then be reviewed after the test is
;; passing for accuracy.
;;
;; Each test additionally can have a temporary directory available to it which
;; is accessible with the `%tmpdir` substitution. For example:
;;
;; RUN[tmpdir]: print % -o %tmpdir/foo.wat | validate %tmpdir/foo.wat
;;
;; Note that temporary directories are persisted across tests in the same file,
;; but different files all get different temporary directories.
;;
;; You can also split commands across multiple lines:
;;
;; RUN[multiline]: print % | \
;;                 validate
;;
;; here the `\` character is deleted and the next line is concatenated.


;; this is the contents of the test, mostly empty in this case.
(module
  (type (func (result v128)))
)
