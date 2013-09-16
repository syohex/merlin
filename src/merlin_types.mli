open Misc

val raise_error: exn -> unit
val catch_errors: (unit -> 'a) -> exn list * (exn, 'a) sum

(* Keep track of type variables generated by error recovery *)
val erroneous_type_register: Types.type_expr -> unit
val erroneous_type_check: Types.type_expr -> bool

val relax_typer: bool fluid

include module type of Merlin_types_custom