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

open Std
open Raw_parser

let section = Logger.section "explain_transition"

module Int = struct
  type t = int
  let compare (a : t) b = compare a b
end

module IntMap = Map.Make (Int)
module StringMap = Map.Make (String)

let process acc = function
  | `Assoc ["time", _; "level", _; "section", `String "transition";
            "title", `String "transition";
            "content", `Assoc [
              "from", `Int st_from;
              "to", `Int st_to;
              "class", `String cls;
            ]]
    ->
    let total, transitions =
      try IntMap.find st_from acc
      with Not_found -> 0, StringMap.empty
    in
    let count =
      try StringMap.find cls transitions
      with Not_found -> 0
    in
    IntMap.add st_from
      (total + 1, StringMap.add cls (count + 1) transitions)
      acc
  | _ -> acc

let rec process_stream acc stream =
  match Stream.next stream with
  | exception Stream.Failure -> acc
  | item -> process_stream (process acc item) stream

let filter_entry (total,classes) =
  let reverse =
    StringMap.fold (fun cls count -> IntMap.add count cls)
      classes IntMap.empty
  in
  let k1, v1 = IntMap.max_binding reverse in
  let vs =
    match IntMap.max_binding (IntMap.remove k1 reverse) with
    | k2, v2 when k2 * 2 >= k1 -> [v1;v2]
    | exception Not_found -> [v1]
    | _ -> [v1]
  in
  List.map Raw_parser_values.class_of_string vs

let table =
  match Sys.getenv "TRANSITIONS" with
  | exception Not_found -> IntMap.empty
  | filename ->
    let stream = Json.stream_from_file filename in
    IntMap.map filter_entry (process_stream IntMap.empty stream)

let suggested_transitions lr0 =
  try IntMap.find lr0 table
  with Not_found -> []
