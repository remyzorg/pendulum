
# Pendulum's syntax

## Concrete syntax of the extension

Pendulum's keywords are based on Esterel's keywords

```

sname := <ocaml-expr-ident>
label := <ocaml-expr-ident>

expr :=
  | nothing
  | pause
  | emit <sname> <ocaml-expr>
  | present <sname> <expr> <expr>
  | loop <expr>
  | <expr> || <expr>
  | <expr> ; <expr>
  | signal <sname> <ocaml-expr> <expr>
  | trap <label> <expr>
  | exit <label>
  | suspend <sname> <expr>
  | atom <ocaml-expr>
  | present <sname> <expr> (* not kernel *)

inout_decls :=
  | input sname `;` 
  | input sname `;` inout_decls

program := inout_decls expr

```

## Syntax's precedence rules

The syntax's precedence rules follow those of OCaml corresponding expressions :
* keywords have the function call precedence. example : 
```ocaml
loop
  atom
    print_string "hello";
  pause
```
is incorrect. The expression inside `loop `must be parenthezised with `begin...end` or `(...)` and
so does the `atom`
```ocaml
loop begin
  atom (print_string "hello");
  pause
end 
```

* `||` and `;` has the same precedence than in OCaml, particularly :
 * Both are weaker than function calls so `loop pause || loop pause` and `loop pause ; loop pause` are syntactically corrects and does what you expect
 * `;` is weaker than `||` so :
```ocaml
loop
  pause
|| 
atom (print_string "hello");
pause

```
is equivalent to
```ocaml
(
  (loop pause)
  ||
  (atom (print_string "hello"))
)
;
(
  pause
)

```

# Pendulum's API

## The syntax extension code entry

The syntax extension in called from a standard OCaml code by tagging a `let` with `%sync`

```ocaml
let%sync my_machine = (* pendulum program *)

```

It generates a value `my_machine` of type `Pendulum.Machine.t`.
This value only has a field `instantiate : 'a -> (unit -> Machine.state)`. 

```ocaml
let step = my_machine.instantiate ( ... )
```

Calling `my_machine.instatiate ( ... )` will create one particular instantiation
of this reactive program description, represented by its *step* or *moving
forward* function.

## Quick example

```ocaml

open Pendulum
open Machine

let%sync hw_loop =
  loop begin
    atom (print_string "Hello world\n");
    pause
  end
```

defines a reactive program which is printing a string and waiting for the next instant, and so on.
Now I want to execute it :

```ocaml
let () = 
  let step = hw_loop.instantiate () in
  for i = 0 to 9 do
    ignore (step ())
  done
```

We get the `step` function from the instantiation. Then we call it in a *for
loop* to trigger the instant 10 times. Now you would like to add input signals
to your machine. So you can give it parameters at each instants.

First, modify your reactive program this way, by defining inputs signals : 

```ocaml
let%sync hw_loop =
  input s1;
  input s2;

  (* ... *)

```

Then you must create the signal in the OCaml world.

```ocaml
let () = 
  let s1 = make_signal () in (* signals values have the type unit here *)
  let s2 = make_signal () in
  let step = hw_loop.instantiate (s1, s2) in

  (* ... *)

```

You may want to play with it in the reactive program. We use the reactive
expression `present` which tests if the signal has been emited (or set present
in the OCaml world) during this instant. If so, it executes the corresponding
alternative. As you can see, you only have to write one alternative is the
second does `nothing`.

```ocaml
let%sync hw_loop =
  input s1;
  input s2;
  loop begin
    present s1 
      (atom (print_string "Hello"));
    present s2 
      (atom (print_string "World\n"))
      (atom (print_string "\n"));
    pause
  end
```

And eventually call the `step` in the for loop :

```ocaml
let () = 
  let s1 = make_signal () in
  let s2 = make_signal () in
  let step = hw_loop.instantiate (s1, s2) in
  for i = 0 to 9 do
    if i mod 2 = then set_present s1    (* s1 is present only on even i *)
    else set_present s2;                (* s2 is present only on odd i *)
    if i mod 5 = 0 then set_present s1; (* if i is a multiple of 5 then both are presents *)
    ignore (step ())
  done
```
