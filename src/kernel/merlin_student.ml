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
  val fresh : size:int -> 'a -> 'a t
  val push : hash -> 'a -> 'a t -> 'a t * hash * 'a
end = struct
  type 'a t = {
    size: int;
    hash: hash;
    head: (hash * 'a) list;
    tail: (hash * 'a) list;
  }

  let fresh ~size x =
    assert (size > 0);
    {
      size;
      hash = Rollinghash.empty;
      head = [];
      tail = List.replicate (0L,x) size;
    }

  let pop tail head = match tail with
    | item :: tail ->
      item, tail, head
    | [] ->
      match List.rev head with
      | item :: tail ->
        item, tail, []
      | [] -> assert false

  let push item payload {size; hash; tail; head}  =
    let (item', payload'), tail, head = pop tail head in
    let hash = Rollinghash.remove ~size ~item:item' hash in
    let hash = Rollinghash.push ~item hash in
    {size; hash; head = (item,payload) :: head; tail}, hash, payload'
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
  type line = hash * Merlin_lexer.item list
  val fresh : size:int -> t
  val push : Merlin_lexer.item -> t -> t * line  option
  val flush : t -> t * line

end = struct
  type t = {
    line: Linehasher.t;
    items: Merlin_lexer.item list;
    window: Merlin_lexer.item list Rollingwindow.t;
  }
  type line = hash * Merlin_lexer.item list

  let fresh ~size =
    { line = Linehasher.empty;
      items = [];
      window = Rollingwindow.fresh ~size [] }

  let token_class token =
    Raw_parser_values.(class_of_symbol (symbol_of_token token))

  let push item t =
    let items = item :: t.items in
    match item with
    | Merlin_lexer.Error _ -> {t with items}, None
    | Merlin_lexer.Valid (startp, token, _) ->
      match token_class token with
      | Raw_parser_values.CN_ _ -> assert false
      | Raw_parser_values.CT_ (tc, _) ->
        let line, h = Linehasher.push startp tc t.line in
        match h with
        | None -> {t with items; line}, None
        | Some hash ->
          let items = List.rev items in
          let window, h, result = Rollingwindow.push hash items t.window in
          let t = {items = []; window; line} in
          match result with
          | [] -> t, None
          | result -> t, Some (h, result)

  let flush t =
    let line, hash = Linehasher.flush t.line in
    let items = List.rev t.items in
    let window, h, result = Rollingwindow.push hash items t.window in
    {line; items; window}, (h, result)
end

module HashSet = Set.Make (struct
    type t = int64
    let compare = Int64.compare
  end)

let line_window_size = 5

module Learner : sig
  type t
  val fresh: unit -> t
  val learn:
    t -> 'a History.t -> ('a -> Merlin_parser.t) -> ('a -> Merlin_lexer.item) -> unit

  val what_about: t -> hash -> HashSet.t
end = struct
  (* Map a Linewindow hash to a set of parser hashes *)
  type t = (hash, HashSet.t) Hashtbl.t

  let fresh () = Hashtbl.create 117

  let what_about t index =
    try Hashtbl.find t index
    with Not_found -> HashSet.empty

  let register t index hasher =
    let hashset = what_about t index in
    let value = Parserhasher.get hasher in
    (*Printf.eprintf "REGISTERING HASH %08LX %08LX\n%!" index value;*)
    Hashtbl.replace t index (HashSet.add value hashset)

  let all_lines get_item tokens =
    let rec flush window acc =
      let window, line = Linewindow.flush window in
      match line with
      | _, [] -> List.rev acc
      | _ -> flush window (line :: acc)
    in
    let rec aux window acc = function
      | [] -> flush window acc
      | x :: xs ->
        let window, line = Linewindow.push (get_item x) window in
        aux window (List.cons_option line acc) xs
    in
    let window = Linewindow.fresh ~size:line_window_size in
    aux window [] tokens

  let rec find_parser get_parser get_item item = function
    | x0 :: (x1 :: _ as tail) when get_item x1 == item ->
      get_parser x0, tail
    | _ :: tail -> find_parser get_parser get_item item tail
    | [] -> assert false

  let rec learn t get_parser get_item hasher tail = function
    | [] -> ()
    | (_, []) :: _ -> assert false
    | (hash, item :: _) :: lines ->
      let parser, tail = find_parser get_parser get_item item tail in
      let hasher = Parserhasher.hash parser hasher in
      register t hash hasher;
      learn t get_parser get_item hasher tail lines

  let learn t history get_parser get_item =
    let tail = History.tail history in
    let lines = List.drop_n 1 (all_lines get_item tail) in
    learn t get_parser get_item Parserhasher.empty tail lines
end
