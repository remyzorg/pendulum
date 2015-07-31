

# Readme

# Description

**pendulum** is a language dedicated to the programming of reactive systems on the
 Web. It has a powerful expressivity to describe synchronous concurrent systems
 communicating with broadcasted signals. Its constructions and implementation
 are mostly based on Esterel. The language is embedded in OCaml as a syntax
 extension. It allows the programmer to describe **reactive machines** as OCaml
 values, and run them on signals arguments. The compilation of concurrency in pendulum is
 completely sequential and static.
 ([similar work and inspiration](https://www.github.com/remyzorg/pendulum/wiki/Documentation))

The core language is completely compatible with vanilla OCaml compiler.

**Please do not use pendulum for now, as it is an early prototype**

# Documentation

see a slight [documentation](https://www.github.com/remyzorg/pendulum/wiki/Documentation) and [examples](examples/)


# Install

### with opam (easier)

opam `1.2` with `switch 4.02.2` (minimum) is required

* `opam pin add pendulum git://github.com/remyzorg/pendulum.git`

* an example to use pendulum as syntax extension with ocamlbuild

`ocamlbuild -package pendulum -cflags "-ppx ppx_pendulum" <file-without-ext>.byte`

### without opam

1. `git clone git://github.com/remyzorg/pendulum.git`
2. `cd pendulum`
3. `./configure`
4. `make`
5. `make install `
