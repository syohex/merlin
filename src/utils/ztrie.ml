open Std

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
end

module type S = sig
  type branch
  type value

  type t
  val empty: t

  val seek: t -> branch -> t
  val get: t -> value
  val set: t -> value -> t

  val find: t -> branch -> t * branch
  val position: t -> branch
end

module Validate (Branch : BRANCH) =
struct
  type index' = Branch.index

  include (Branch :
             module type of struct include Branch end
           with type index := index')

  let () = assert (depth root = 0)

  let parent t =
    let result = parent t in
    begin match result with
    | Some t' -> assert (depth t = depth t' + 1)
    | None -> assert (depth t = 0)
    end;
    result

  type index = index' * int
  let index t =
    let depth = Branch.depth t in
    assert (depth > 0);
    index t, depth

  let compare (i1,d1) (i2,d2) =
    assert (d1 = d2);
    compare i1 i2
end

module Make (Branch : BRANCH) (V : VALUE) =
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
    miss: Branch.t;
    node: node;
  }

  let empty_node = {
    children = Map.empty;
    value = V.empty;
  }

  let get t = t.node.value
  let set t v = {t with node = {t.node with value = v}}

  let empty = { path = []; node = empty_node;
                branch = Branch.root; miss = Branch.root; }

  let rec move_up count node = function
    | path when count = 0 ->
      path, node
    | (node',key) :: path' ->
      let node' = {node' with children = Map.add key node node'.children} in
      move_up (count - 1) node' path'
    | [] -> assert false

  exception Miss
  let move_up t depth =
    let count = Branch.depth t.branch - depth in
    if count < 0 then
      raise Miss
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
    | branch :: bs as branches ->
      let index = Branch.index branch in
      match Map.find index node.children with
      | node' -> go_down ((node, index) :: path) node' bs
      | exception Not_found ->
        path, node, branches

  let move_to t branch_hint branch =
    let depth = Branch.ancestor_depth branch_hint branch in
    let up_path, node = move_up t depth in
    let down_path = unroll branch depth in
    go_down up_path node down_path

  let find t target =
    match move_to t t.miss target with
    | path, node, remaining ->
      let branch = match remaining with
        | [] -> target
        | child :: _ ->
          match Branch.parent child with
          | None -> assert false
          | Some branch -> branch
      in
      {path; node; branch; miss = target}, branch
    | exception Miss -> {t with miss = target}, t.branch

  let rec mkdirs path = function
    | branch :: branches ->
      mkdirs ((empty_node,Branch.index branch) :: path) branches
    | [] -> path

  let seek t branch =
    match move_to t t.branch branch with
    | path, node, remaining ->
      let path, node =
        match remaining with
        | [] -> path, node
        | branch :: branches ->
          mkdirs ((node, Branch.index branch) :: path) branches,
          empty_node
      in
      {path; node; branch; miss = branch}
    | exception Miss -> assert false

  let position t = t.branch
end
