# S

S is a small S-expression based language which nevertheless is capable of
evaluating itself.

To learn its syntax study [the tests].

The main interpreter is written in [Rust]. Here is [the interpreter loop]. There
is also an [interpreter written in s].

To run the tests, use `cargo test --release`. They are quite slow, because each
test is executed three times with different layering of interpreters.


[the tests]: https://github.com/matklad/s/blob/5614c3dd6b8e585b5e30baa6d7b677811aa69a93/src/eval.rs#L469
[Rust]: https://www.rust-lang.org/
[the interpreter loop]: https://github.com/matklad/s/blob/5614c3dd6b8e585b5e30baa6d7b677811aa69a93/src/eval.rs#L414-L427
[interpreter written in s]: https://github.com/matklad/s/blob/5614c3dd6b8e585b5e30baa6d7b677811aa69a93/src/eval.s