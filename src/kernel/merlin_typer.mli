(* {{{ COPYING *(

  This file is part of Merlin, an helper for ocaml editors

  Copyright (C) 2013 - 2014  Frédéric Bour  <frederic.bour(_)lakaban.net>
                             Thomas Refis  <refis.thomas(_)gmail.com>
                             Simon Castellan  <simon.castellan(_)iuwt.fr>

  Permission is hereby granted, free of charge, to any person obtaining a
  copy of this software and associated documentation files (the "Software"),
  to deal in the Software without restriction, including without limitation the
  rights to use, copy, modify, merge, publish, distribute, sublicense, and/or
  sell copies of the Software, and to permit persons to whom the Software is
  furnished to do so, subject to the following conditions:

  The above copyright notice and this permission notice shall be included in
  all copies or substantial portions of the Software.

  The Software is provided "as is", without warranty of any kind, express or
  implied, including but not limited to the warranties of merchantability,
  fitness for a particular purpose and noninfringement. In no event shall
  the authors or copyright holders be liable for any claim, damages or other
  liability, whether in an action of contract, tort or otherwise, arising
  from, out of or in connection with the software or the use or other dealings
  in the Software.

)* }}} *)

type t

val is_valid : t -> bool

val fresh : unit_name:string -> stamp:bool ref list -> Extension.set -> t
val update : Merlin_parser.t -> t -> t

val env : t -> Env.t

type 'a result = [ `Ok of 'a | `Fail of Env.t * Location.t ]
type content =
  [ `Str of Parsetree.structure * Typedtree.structure result
  | `Sg of Parsetree.signature * Typedtree.signature result
  ]

val contents : t -> (content * Typecore.delayed_check list) list
val exns : t -> exn list
val delayed_checks : t -> exn list
val extensions : t -> Extension.set

val dump : Format.formatter -> t -> unit

val with_typer : t -> (unit -> 'a) -> 'a
