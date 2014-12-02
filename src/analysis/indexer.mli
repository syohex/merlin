open Std

type db
val fresh: unit -> db
val update_cmis: db -> string list -> unit
val update_path: db -> string List.Lazy.t -> unit

(* Returns [] for unknown digest *)
val rdeps: db -> Digest.t -> Digest.t list

type cmi = {
  name: string;
  path: string;
  mtime: float;
  digest: Digest.t;
  deps: Digest.t list;
}

(* Raises Not_found for unknown digest *)
val find_digest: db -> Digest.t -> cmi
(* Raises Not_found for unknown path *)
val find_path: db -> string -> cmi
