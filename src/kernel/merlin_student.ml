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

(* Incremental hashing of a menhir parser *)
module Parserhasher = struct
  open Merlin_parser

  type hash = int * int list

  type t = (hash * frame) list

  let empty : t = []
  let root : hash = 0, []

  let get (x : t) : hash = match x with
    | [] -> assert false
    | (hash,_) :: _ -> hash

  let drop_nonobservable = function
    | n, (x :: xs)
      when not (Merlin_recovery_strategy.observable_lr0_state x) ->
      n - 1, xs
    | xs -> xs

  let add (xxs : t) (f : frame) =
    let nk = match xxs with
      | [] -> root
      | (xs,f) :: _ -> xs
    in
    let n, k = drop_nonobservable nk in
    let nk' = n + 1, Frame.lr0_state f :: k in
    (nk', f) :: xxs

  let rec fresh (f : frame) =
    match Frame.next f with
    | None -> add [] f
    | Some f' -> add (fresh f') f

  let update (f : frame) (x : t) : t =
    try match x with
    | [] -> raise Not_found
    | (br, f') :: _ ->
      let root = root_frame f f' in
      let rec backtrack = function
        | ((_, f') :: _ as t) when Frame.eq f' root -> t
        | _ :: t -> backtrack t
        | [] -> assert false
      in
      List.fold_left ~f:add ~init:(backtrack x)
        (unroll_stack ~from:f ~root)
    with Not_found -> fresh f
end

module Branch : Ztrie.BRANCH with type t = Parserhasher.hash = struct
  type t = Parserhasher.hash
  let parent (n,xs) = match xs with
    | [] -> None
    | _ :: xs' -> Some ((n - 1), xs')

  let depth = fst

  let root = Parserhasher.root

  let ancestor_depth (n1,l1) (n2,l2) =
    let n, l1, l2 =
      if n1 = n2 then
        n1, l1, l2
      else if n1 > n2 then
        n2, List.drop_n (n1 - n2) l1, l2
      else
        n1, l1, List.drop_n (n2 - n1) l2
    in
    let rec pop n l1 l2 = if l1 == l2 then n else match l1, l2 with
        | _ :: l1, _ :: l2 -> pop (n - 1) l1 l2
        | _ -> assert false
    in
    pop n l1 l2

  type index = int
  let index = function
    | _, [] -> assert false
    | _, (n :: _) -> n

  let compare (n1 : int) n2 = compare n1 n2
end

module ParserTrie = Ztrie.Make (Branch) (struct
    type t = HashSet.t
    let empty = HashSet.empty
  end)

module Learner : sig
  type t
  val fresh: unit -> t
  val learn:
    t -> 'a History.t -> ('a -> Merlin_parser.t) -> ('a -> Merlin_lexer.item) -> unit

  val what_about: t -> Parserhasher.hash -> HashSet.t
end = struct
  type t = {
    mutable trie: ParserTrie.t;
  }

  let fresh () = {
    trie = ParserTrie.empty;
  }

  let what_about t branch =
    let trie, branch' = ParserTrie.find t.trie branch in
    t.trie <- trie;
    if branch = branch' then
      ParserTrie.get trie
    else
      HashSet.empty

  let register t index folder =
    let branch = Parserhasher.get folder in
    let trie = ParserTrie.seek t.trie branch in
    let hashes = ParserTrie.get trie in
    let trie = ParserTrie.set trie (HashSet.add index hashes) in
    Logger.infojf section ~title:"Learner.register"
      (fun (_branch,index) -> `Assoc [
           "parser", `String "FIXME";
           "lexer", `String (sprintf "%016LX" index);
         ])
      (branch,index);
    t.trie <- trie

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
      Merlin_parser.stack (get_parser x0), tail
    | _ :: tail -> find_parser get_parser get_item item tail
    | [] -> assert false

  let rec learn t get_parser get_item hasher tail = function
    | [] -> ()
    | (_, []) :: _ -> assert false
    | (hash, item :: _) :: lines ->
      let parser, tail = find_parser get_parser get_item item tail in
      let hasher = Parserhasher.update parser hasher in
      register t hash hasher;
      learn t get_parser get_item hasher tail lines

  let learn t history get_parser get_item =
    let tail = History.tail history in
    let lines = List.drop_n 1 (all_lines get_item tail) in
    learn t get_parser get_item Parserhasher.empty tail lines
end

