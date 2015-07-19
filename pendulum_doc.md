






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
 * Both are weaker than function calls so `loop pause || loop pause` and `loop pause ; loop pause` are syntactically corrects and it does what you expect
 * `;` is weaker than `||` so :
```
loop
  pause
|| 
atom (print_string "hello");
pause

```
is equivalent to
```
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

The syntax extension in called from a standard OCaml code by tagging a `let` with `%sync`

```ocaml
let%sync my_machine = (* pendulum program *)

```

It generates a value `my_machine` of type `Pendulum.Machine.t`.
This value only has a field `instantiate : 'a -> (unit -> Machine.state)`. 

Calling `my_machine.instatiate ( ... )` will create one particular instantiation
of this synchronous program description, represented by its *step* or *moving
forward* function.




