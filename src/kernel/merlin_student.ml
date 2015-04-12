open Std

type hash = int64

let section = Logger.section "student"

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

  let push item payload {size; hash = oldhash; tail; head}  =
    let (item', payload'), tail, head = pop tail head in
    let hash = Rollinghash.push ~item oldhash in
    let hash = Rollinghash.remove ~size ~item:item' hash in
    {size; hash; head = (item,payload) :: head; tail}, oldhash, payload'
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
      (*try
        let root = Merlin_parser.root_frame frame top in
        hstack_push
          (hstack_unroll ~from:stack ~root)
          (Merlin_parser.unroll_stack ~from:top ~root)
      with Not_found ->*) hstack_fresh frame

  (* hashed parser = hashed stack + current state *)
  type t = int64 * hstack

  let empty = 0L, []
  let get = fst
  let hash parser (_,hstack) =
    let hstack = hstack_update hstack (stack parser) in
    let hash = push_state (hstack_hash hstack) ~lr1:(get_lr1_state parser) in
    Logger.infojf section ~title:"Parserhasher.hash"
      (fun (hash,parser,hstack) ->
         let states = List.map
             (fun (f,_) -> `Int (Merlin_parser.Frame.lr1_state f))
             hstack
         in
         `Assoc [
           "parser", `String (sprintf "%016LX" hash);
           "lr1",    `Int (get_lr1_state parser);
           "states", `List states;
         ])
      (hash,parser,hstack);
    hash, hstack
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
    match item with
    | Merlin_lexer.Error _ -> {t with items = item :: t.items}, None
    | Merlin_lexer.Valid (startp, token, _) ->
      match token_class token with
      | Raw_parser_values.CN_ _ -> assert false
      | Raw_parser_values.CT_ (tc, _) ->
        let line, h = Linehasher.push startp tc t.line in
        match h with
        | None -> {t with items = item :: t.items; line}, None
        | Some hash ->
          let items = List.rev t.items in
          let window, h, result = Rollingwindow.push hash items t.window in
          let t = {items = [item]; window; line} in
          match result with
          | [] -> t, None
          | result ->
            Logger.infojf section ~title:"Linewindow.push"
              (fun (h,result) ->
                 let token =
                   let open Raw_parser_values in function
                     | Merlin_lexer.Error _ -> `String "*err*"
                     | Merlin_lexer.Valid (_,tok,_) ->
                       `String ((string_of_class
                                   (class_of_symbol (symbol_of_token tok))))
                 in
                 `Assoc [
                   "lexer", `String (sprintf "%016LX" h);
                   "tokens", `List (List.map ~f:token result);
                 ])
              (h,result);
            t, Some (h, result)

  let flush t =
    let line, hash = Linehasher.flush t.line in
    let items = List.rev t.items in
    let window, h, result = Rollingwindow.push hash items t.window in
    {line; items = []; window}, (h, result)
end

module HashSet = Set.Make (struct
    type t = int64
    let compare = Int64.compare
  end)

let linewindow_size = 5
let linewindow () = Linewindow.fresh ~size:linewindow_size

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
    Logger.infojf section ~title:"Learner.register"
      (fun (value,index) -> `Assoc [
           "parser", `String (sprintf "%016LX" value);
           "lexer", `String (sprintf "%016LX" index);
         ])
      (value,index);
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
    let window = linewindow () in
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

module type BRANCH = sig
  type t

  val parent: t -> t option
  val depth: t -> int

  val root: t
  val ancestor_depth: t -> t -> int

  type index
  val index: t -> index
  val compare: index -> index -> int
end

module type VALUE = sig
  type t
  val empty: t
  val is_empty: t -> bool
end

module ZTrie (Branch : BRANCH) (V : VALUE) :
sig
  type t
  val empty: t

  val seek: t -> Branch.t -> t
  val get: t -> V.t
  val set: t -> V.t -> t

  val position: t -> Branch.t
end =
struct
  module Map = Map.Make(struct
      type t = Branch.index
      let compare = Branch.compare
    end)

  type node = {
    children: node Map.t;
    value: V.t;
  }

  type t = {
    path: (node * Branch.index) list;
    branch: Branch.t;
    node: node;
  }

  let empty_node = {
    children = Map.empty;
    value = V.empty;
  }
  let is_empty_node n = Map.is_empty n.children && V.is_empty n.value

  let get t = t.node.value
  let set t v = {t with node = {t.node with value = v}}

  let empty = { path = []; node = empty_node;
                branch = Branch.root }

  let rec move_up count node = function
    | path when count = 0 ->
      path, node
    | (node',key) :: path' ->
      let node' = {node' with children = Map.add key node node'.children} in
      move_up (count - 1) node' path'
    | [] -> assert false

  let rec clean_up count = function
    | path when count = 0 ->
      path, empty_node
    | (node',key) :: path' ->
      let node' = {node' with children = Map.remove key node'.children} in
      if is_empty_node node' then
        clean_up (count - 1) path'
      else
        move_up (count - 1) node' path'
    | [] -> assert false

  let move_up t depth =
    let count = Branch.depth t.branch - depth in
    if is_empty_node t.node then
      clean_up count t.path
    else
      move_up count t.node t.path

  let rec unroll acc branch count =
    if count = 0 then
      acc
    else match Branch.parent branch with
      | None -> assert false
      | Some parent ->
        unroll (branch :: acc) parent (count - 1)

  let unroll branch depth = unroll [] branch (Branch.depth branch - depth)

  let rec go_down path node = function
    | [] -> path, node, []
    | branch :: bs ->
      let index = Branch.index branch in
      match Map.find index node.children with
      | node' -> go_down ((node, index) :: path) node' bs
      | exception Not_found ->
        path, node, bs

  let move_to t branch =
    let depth = Branch.ancestor_depth t.branch branch in
    let up_path, node = move_up t depth in
    let down_path = unroll branch depth in
    go_down up_path node down_path

  let seek t branch =
    let path, node, remaining = move_to t branch in
    let path, node =
      match remaining with
      | [] -> path, node
      | branch :: branches ->
        let path' = List.map ~f:(fun b -> empty_node, Branch.index b) branches in
        let path = List.rev_append path' ((node, Branch.index branch) :: path) in
        List.tl path, empty_node
    in
    { path; node; branch }

  let position t = t.branch
end
