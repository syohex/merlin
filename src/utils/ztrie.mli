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

module Make (Branch : BRANCH) (V : VALUE) :
  S with type branch := Branch.t
     and type value := V.t
