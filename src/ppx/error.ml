open Migrate_parsetree
open Ast_405
open Parsetree


type syntax =
  | Forgot_sem
  | Keyword
  | Signal_name
  | Signal_test
  | Signal_expr
  | Run_params
  | Signal_decl
  | Signal_decl_at_start
  | Event_name
  | Unknown_arg_option of string
  | Argument_syntax_error

type 'a t =
  | Value_missing of string
  | Syntax of syntax
  | Non_recursive_let
  | Only_on_let
  | Wrong_argument_values
  | Other_err of 'a * (Format.formatter -> 'a -> unit)


let print_syntax_rsn fmt rsn =
  let open Format in
  match rsn with
  | Forgot_sem -> fprintf fmt "maybe you forgot a `;`"
  | Keyword -> fprintf fmt "reactive statement expected"
  | Signal_name -> fprintf fmt "signal name expected"
  | Signal_test -> fprintf fmt "signal test expected"
  | Event_name -> fprintf fmt "event name expected"
  | Signal_expr -> fprintf fmt "signal expression expected"
  | Signal_decl -> fprintf fmt "signal declaration exprected"
  | Run_params -> fprintf fmt "run parameters expected"
  | Signal_decl_at_start -> fprintf fmt "signal declarations must be on the top"
  | Unknown_arg_option s -> fprintf fmt "unknown option %s" s
  | Argument_syntax_error -> fprintf fmt "argument expected"


let print_error fmt e =
  let open Format in
  fprintf fmt "[pendulum] ";
  match e with
  | Value_missing s -> fprintf fmt "Signal value of %s is missing" s
  | Syntax rsn -> fprintf fmt "Syntax error : %a" print_syntax_rsn rsn
  | Non_recursive_let -> fprintf fmt "recursive `let` not allowed"
  | Only_on_let -> fprintf fmt "only allowed on let"
  | Other_err (e, f) -> fprintf fmt "%a" f e
  | Wrong_argument_values -> fprintf fmt "unexpected value for this argument"

let error ~loc rsn =
  raise (Location.Error (
      Location.error ~loc (Format.asprintf "%a" print_error rsn)))

let syntax_error ~loc r = error ~loc (Syntax r)

let signal_value_missing e s =
  error ~loc:e.pexp_loc (Value_missing s)
