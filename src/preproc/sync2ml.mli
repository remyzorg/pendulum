


type error = Noerr

exception Error of Location.t * error
val print_error : Format.formatter -> error -> unit

val generate: Ast.Tagged.t -> unit
