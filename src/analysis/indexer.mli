open Std

type db
val fresh: unit -> db
val update_cmis: db -> string list -> unit
val update_path: db -> string List.Lazy.t -> unit

(* Returns [] for unknown digest *)
val rdeps: db -> Digest.t -> Digest.t list
(* Raises Not_found for unknown digest *)
val path: db -> Digest.t -> string
