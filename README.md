

# Readme

# Description

**pendulum** is a language dedicated to the programming of reactive systems on the
 Web. It has a powerful expressivity to describe synchronous concurrent systems
 communicating with broadcasted signals. Its constructions and implementation
 are mostly based on Esterel. The language is embedded in OCaml as a syntax
 extension. It allows the programmer to describe **reactive machines** as OCaml
 values, and run them on signals arguments. The compilation of concurrency in pendulum is
 completely sequential and static.

The core language is completely compatible with vanilla OCaml compiler >4.02.1.


**Please do not use pendulum for now, as it is an early prototype**


# TODO :

* Code generation
  * handle value for local signals !!
  * check if then else generation
  * weird error when forgotting `;` after the last signal decl
  * remove initialization syntax on global signals


* Documentation :
  * write examples
  * comment surf / depth
  * other comments

* API
  * output takes a callback triggered when they are emited
  * input takes a call which triggers the machine or
  better which update the signal value and presence
  So the presence must not be reset each beggining of a step ? (maybe at the end ?)

* Language
  * add first class machines (necessary)
  * maintain signals values or presence informations between steps
  * gathering operators at signal definitions (several emission of the same signal at the same instant
  * `pre`
  * n-ary parallel operator and/or gathering (at signals definition)
* DOM
  * try setTimeout and requestAnimationFrames as global clocks


# Install

### with opam (easier)

opam `1.2` with `switch 4.02.1` (minimum) is required

* `opam pin add pendulum git://github.com/remyzorg/pendulum.git`

(Bonus)
* an example to use pendulum as syntax extension with ocamlbuild

`ocamlbuild -package pendulum -cflags "-ppx ppx_pendulum" main.byte`

### without opam

1. `git clone git://github.com/remyzorg/pendulum.git`
2. `cd pendulum`
3. `./configure`
4. `make`
5. `make install `
