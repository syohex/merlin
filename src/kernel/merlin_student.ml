open Std

type hash = int64

(* Compute a rolling hash *)
module Rollinghash : sig
  val empty : hash
  val push : item:hash -> hash -> hash
  val remove : size:int ->item:hash -> hash -> hash
end = struct
  type t = int64

  let empty = 0L

  let rotate l = function
    | 0 -> l
    | n -> Int64.(logor (shift_left l n) (shift_right_logical l (64 - n)))

  let push ~item t = Int64.logxor (rotate t 1) item
  let remove ~size ~item t = Int64.logxor (rotate item size) t
end

(* Maintain a rolling window, with a custom payload *)
module Rollingwindow : sig
  type 'a t
  val fresh : size:int -> 'a t
  val push : hash -> 'a option -> 'a t -> hash * 'a option
end = struct
  type 'a t = {
    mutable cursor: int;
    mutable hash : hash;
    items: hash array;
    payloads: 'a option array;
  }

  let fresh ~size =
    assert (size > 0);
    {
      cursor = 0;
      hash = Rollinghash.empty;
      items = Array.make size 0L;
      payloads = Array.make size None;
    }

  let push item payload t =
    let size = Array.length t.items in
    let cursor = t.cursor in
    let hash = t.hash in
    let hash = Rollinghash.remove ~size ~item:t.items.(cursor) hash in
    let hash = Rollinghash.push ~item hash in
    t.hash <- hash;
    let payload' = t.payloads.(cursor) in
    t.items.(cursor) <- item;
    t.payloads.(cursor) <- payload;
    let cursor = cursor + 1 in
    if cursor = size then
      t.cursor <- 0
    else
      t.cursor <- cursor;
    hash, payload'
end

(* Incremental hashing of a menhir parser *)
module Parserhasher : sig
  type t
  val empty : t
  val hash : Merlin_parser.t -> t -> t
  val get : t -> int64
end = struct
  open Merlin_parser

  let push_state hash ~lr1 =
    Rollinghash.push ~item:(Hashlut.small_int lr1) hash

  (* hashed stack *)
  type hstack = (frame * int64) list
  let hstack_hash = function
    | [] -> 0L
    | (_,h) :: _ -> h

  let rec hstack_push stack h = function
    | [] -> stack
    | f :: fs ->
      let h = push_state h ~lr1:(Frame.lr1_state f) in
      hstack_push ((f,h) :: stack) h fs

  let hstack_push stack frames =
    hstack_push stack (hstack_hash stack) frames

  let rec hstack_unroll ~from ~root = match from with
    | [] -> raise Not_found
    | (x, _) :: _ when Frame.eq x root -> from
    | _ :: from -> hstack_unroll ~from ~root

  let hstack_fresh frame =
    hstack_push [] (List.rev_unfold [frame] ~f:Frame.next frame)

  let hstack_update stack frame =
    match stack with
    | [] -> hstack_fresh frame
    | (top,_) :: _ ->
      try
        let root = Merlin_parser.root_frame frame top in
        let frames = Merlin_parser.unroll_stack ~from:top ~root in
        let stack = hstack_unroll ~from:stack ~root in
        hstack_push stack frames
      with Not_found -> hstack_fresh frame

  (* hashed parser = hashed stack + current state *)
  type t = int64 * hstack

  let empty = 0L, []
  let get = fst
  let hash parser (_,hstack) =
    let hstack = hstack_update hstack (stack parser) in
    push_state (hstack_hash hstack) ~lr1:(get_lr1_state parser), hstack
end

(* Linehasher, hash tokens on the same line *)
module Linehasher : sig
  type t
  val empty : t
  val push : Lexing.position -> _ Raw_parser.token_class -> t -> t * int64 option
  val current : t -> int64
  val flush : t -> t * int64
end = struct
  type t = int * int64
  let empty = -1, 0L
  let current = snd

  let push startp token (line',hash') =
    let line, col = Lexing.split_pos startp in
    let result, hash =
      if line = line' || line' = -1 then
        None, hash'
      else
        Some hash', 0L
    in
    let token = Obj.repr token in
    assert (Obj.is_int token);
    let token : int = Obj.obj token in
    let hash = Rollinghash.push ~item:(Hashlut.small_int token) hash in
    let hash = Rollinghash.push ~item:(Hashlut.small_int col) hash in
    let t = (line, hash) in
    t, result

  let flush (_, h) = empty, h
end

(* Rolling window over consecutive lines of the lexer *)
module Linewindow : sig
  type t
  val fresh : size:int -> t
  val push : t -> Merlin_lexer.item -> (hash * Merlin_lexer.item list) option
  val flush : t -> hash * Merlin_lexer.item list
end = struct
  type t = {
    mutable line: Linehasher.t;
    mutable items: Merlin_lexer.item list;
    window: Merlin_lexer.item list Rollingwindow.t;
  }

  let fresh ~size =
    { line = Linehasher.empty;
      items = [];
      window = Rollingwindow.fresh ~size }

  let token_class token =
    Raw_parser_values.(class_of_symbol (symbol_of_token token))

  let push t item =
    t.items <- item :: t.items;
    match item with
    | Merlin_lexer.Error _ -> None
    | Merlin_lexer.Valid (startp, token, _) ->
      match token_class token with
      | Raw_parser_values.CN_ _ -> assert false
      | Raw_parser_values.CT_ (tc, _) ->
        let line, h = Linehasher.push startp tc t.line in
        t.line <- line;
        match h with
        | None -> None
        | Some hash ->
          let items = List.rev t.items in
          t.items <- [];
          let h, result = Rollingwindow.push hash (Some items) t.window in
          Some (h, Option.value ~default:[] result)

  let flush t =
    let line, hash = Linehasher.flush t.line in
    t.line <- line;
    let items = List.rev t.items in
    t.items <- [];
    let h, result = Rollingwindow.push hash (Some items) t.window in
    h, Option.value ~default:[] result
end

module HashSet = Set.Make (struct
    type t = int64
    let compare = Int64.compare
  end)

let line_window_size = 5

module Learner : sig
  type t
  val fresh: unit -> t
  val learn: t -> from:(Merlin_lexer.item * Merlin_recover.t) History.t -> unit
end = struct
  (* Map a Linewindow hash to a set of parser hashes *)
  type t = (hash, HashSet.t) Hashtbl.t

  let fresh () = Hashtbl.create 117

  let register t index hasher =
    let hashset =
      try Hashtbl.find t index
      with Not_found -> HashSet.empty
    in
    Hashtbl.replace t index (HashSet.add (Parserhasher.get hasher) hashset)

  let all_lines tokens =
    let window = Linewindow.fresh ~size:line_window_size in
    let rec flush acc =
      match Linewindow.flush window with
      | _, [] -> List.rev acc
      | line -> flush (line :: acc)
    in
    let rec aux acc = function
      | [] -> flush acc
      | (item, _recover) :: items ->
        aux (List.cons_option (Linewindow.push window item) acc) items
    in
    aux [] tokens

  let rec find_parser item = function
    | (_, recover) :: ((item', _) :: _ as tail) when item == item' ->
      Merlin_recover.parser recover, tail
    | _ :: tail -> find_parser item tail
    | [] -> assert false

  let rec learn t hasher tail = function
    | [] -> ()
    | (_, []) :: _ -> assert false
    | (hash, (item :: _ as line)) :: lines ->
      let parser, tail = find_parser item tail in
      let hasher = Parserhasher.hash parser hasher in
      register t hash hasher;
      learn t hasher tail lines

  let learn t ~from =
    let _, current = History.focused from in
    let tail = History.tail from in
    let lines = all_lines tail in
    learn t Parserhasher.empty tail lines
end
