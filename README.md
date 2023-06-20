# zig-xml

zig-xml is an XML library for Zig. It is intended to be as correct and efficient
as possible, providing APIs at varying levels of abstraction to best suit the
needs of different users.

**Warning:** this library is still in early development. Bugs and breaking
changes are highly likely. If you need a stable and well-tested XML library,
[zig-libxml2](https://github.com/mitchellh/zig-libxml2) is probably your best
bet (build setup for the popular libxml2 C library).

## Reading XML

**TODO:** see the doc comments in the code; I keep redesigning the API too often
and don't want to keep the README updated yet.

## Writing XML

**TODO:** https://github.com/ianprime0509/zig-xml/issues/10

## Examples

See the `examples` directory (these examples are not very good right now but
they do show how to use most of the library).

## Fuzzing

This library has some basic support for fuzz testing, taking its basic method
from the article
[Fuzzing Zig Code Using AFL++](https://www.ryanliptak.com/blog/fuzzing-zig-code/).
To start fuzzing, you will need
[AFL++](https://github.com/AFLplusplus/AFLplusplus), specifically
`afl-clang-lto` and `afl-fuzz`, in your path. Then, you can run
`zig build fuzz`. To resume a prior fuzzing session, pass `-Dresume=true`.

You can also run `zig build install-fuzz` to just build the fuzz executable and
then run it with `afl-fuzz` separately.

Finally, if any crashes are identified during fuzzing, they can be replayed by
feeding the crash input back to `zig build fuzz-reproduce`, which will yield an
error trace for further debugging.

## License

zig-xml is free and open source software, released under the
[MIT license](https://opensource.org/license/MIT/) as found in the `LICENSE`
file of this repository.
