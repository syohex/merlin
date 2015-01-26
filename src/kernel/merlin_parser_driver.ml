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

let section = Logger.section "parser_driver"

let candidate_pos (_,{Location.txt = _; loc}) =
  Lexing.split_pos loc.Location.loc_start

type t = {
  errors: exn list;
  comments: (string * Location.t) list ;
  parser: Merlin_parser.t;
  recovering: Merlin_recovery.t option;
}

let parser t = t.parser
let exns t = t.errors

let comments t = t.comments

let from_parser parser =
  {errors = []; parser; comments = []; recovering = None}

let dump t = `Assoc [
    "parser", Merlin_parser.dump t.parser;
    "recovery", Option.value_map ~default:`Null
      ~f:Merlin_recovery.dump t.recovering;
  ]

let dump_recoverable t =
  let t = match t.recovering with
    | Some _ -> t
    | None -> {t with recovering = Some (Merlin_recovery.from_parser Lexing.dummy_pos t.parser)}
  in
  dump t

let drop_comments_after pos comments =
  List.take_while comments ~f:(fun (_, loc) ->
    Lexing.compare_pos loc.Location.loc_end pos <= 0
  )

let step warnings token t =
  match token with
  | Merlin_lexer.Error _ -> t
  | Merlin_lexer.Valid (s,tok,e) ->
    let s,e = match tok with
      | EOF -> let pos = {e with Lexing.
                              pos_lnum = e.Lexing.pos_lnum + 1;
                              pos_cnum = e.Lexing.pos_bol} in
        pos, pos
      | _ -> s, e in
    warnings := [];
    let pop w = let r = !warnings in w := []; r in
    let first_comments = drop_comments_after s t.comments in
    let recorded_comments = ref first_comments in
    let record_comment c = recorded_comments := c :: !recorded_comments in
    let recover_from t recovery =
      recorded_comments := first_comments;
      match Merlin_recovery.feed_recover ~record_comment (s,tok,e) recovery with
      | Either.L recovery ->
        {t with recovering = Some recovery}
      | Either.R parser ->
        {t with parser; recovering = None}
    in
    match t.recovering with
    | Some recovery -> recover_from t recovery
    | None ->
      begin match Merlin_recovery.feed_normal ~record_comment (s,tok,e) t.parser with
        | `Reject invalid_parser ->
          let recovery = Merlin_recovery.from_parser e t.parser in
          Logger.infojf section ~title:"entering recovery"
            Merlin_recovery.dump recovery;
          let error = Error_classifier.from invalid_parser (s,tok,e) in
          recover_from
            {t with errors = error :: (pop warnings) @ t.errors}
            recovery
        | `Step parser ->
          {t with errors = (pop warnings) @ t.errors; parser }
      end

let step token t =
  let warnings = ref [] in
  Parsing_aux.catch_warnings warnings
    (fun () -> step warnings token t)

