


type error = Noerr

exception Error of Location.t * error
val print_error : Format.formatter -> error -> unit

type ml_test_expr =
  | MLsig of Ast.signal
  | MLselect of int
  | MLor of ml_test_expr * ml_test_expr
  | MLfinished

type ml_sequence =
  | Seqlist of ml_ast list
  | Seq of ml_sequence * ml_sequence
and ml_ast =
  | MLemit of Parsetree.expression Ast.valued_signal
  | MLif of ml_test_expr * ml_sequence * ml_sequence
  | MLenter of int
  | MLexit of int
  | MLexpr of Ast.atom
  | MLpause
  | MLfinish

val pp_ml_sequence : int -> Format.formatter -> ml_sequence -> unit

val grc2ml : int list array -> Grc.Flowgraph.t -> ml_sequence

val generate:
  ?sigs:(
    Ast.signal list *
    Parsetree.expression Ast.valued_signal list) ->
  Ast.Tagged.t -> Parsetree.expression
