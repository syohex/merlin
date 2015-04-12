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

module Make (Branch : BRANCH) (V : VALUE) :
sig
  type t
  val empty: t

  val seek: t -> Branch.t -> t
  val get: t -> V.t
  val set: t -> V.t -> t

  val find: t -> Branch.t -> t * Branch.t
  val position: t -> Branch.t
end
