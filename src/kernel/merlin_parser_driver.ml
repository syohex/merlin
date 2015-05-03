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
let learner_section = Logger.section "parser_learner"

let candidate_pos (_,{Location.txt = _; loc}) =
  Lexing.split_pos loc.Location.loc_start

type state =
  | Normal
  | Recovering of Merlin_recovery.t
  | Learning of (Lexing.position * Merlin_lexer.item list * Merlin_student.Linewindow.t)

let wisdom = Merlin_student.Learner.fresh ()

type t = {
  errors: exn list;
  comments: (string * Location.t) list ;
  parser: Merlin_parser.t;
  state: state;
}

let parser t = t.parser
let exns t = t.errors

let comments t = t.comments

let from_parser parser =
  {errors = []; parser; comments = []; state = Normal}

let dump t = `Assoc [
    "parser", Merlin_parser.dump t.parser;
    "state", match t.state with
    | Normal -> `String "normal"
    | Recovering recovery ->
      `List [`String "recovering"; Merlin_recovery.dump recovery]
    | Learning _ ->
      `List [`String "learning"]
  ]

let dump_recoverable t =
  let t = match t.state with
    | Recovering _ -> t
    | _  ->
      let recovery = Merlin_recovery.from_parser Lexing.dummy_pos t.parser in
      {t with state = Recovering recovery}
  in
  dump t

let drop_comments_after pos comments =
  List.take_while comments ~f:(fun (_, loc) ->
    Lexing.compare_pos loc.Location.loc_end pos <= 0
  )

let filter_recovery lexhash learner (Zipper.Zipper (head, _, tail)) =
  let open Merlin_student in
  let parsers = List.rev_append head tail in
  let hasher = ref Parserhasher.empty in
  let check (_,{Location. txt = parser}) =
    hasher := Parserhasher.update (Merlin_parser.stack parser) !hasher;
    let hash = Parserhasher.get !hasher in
    Logger.infojf learner_section ~title:"filter_recovery_hash"
      (fun (lexhash, _hash) -> `String
          (sprintf "lexer(%016LX) parser(FIXME) -> ?\n%!" lexhash))
      (lexhash,hash);
    let result = HashSet.mem lexhash (Learner.what_about learner hash) in
    Logger.infoj learner_section ~title:"filter_recovery_found"
      (`Bool result);
    not result
  in
  let parsers = List.take_while ~f:check parsers in
  Zipper.of_list parsers

let rec step warnings t token =
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
    let recover_from t token recovery =
      recorded_comments := first_comments;
      match Merlin_recovery.feed_recover ~record_comment token recovery with
      | Either.L recovery ->
        {t with state = Recovering recovery}
      | Either.R parser ->
        {t with parser; state = Normal}
    in
    let learn_from t endp tokens window =
      let tokens = token :: tokens in
      let window, line = Merlin_student.Linewindow.push token window in
      match line with
      | None -> {t with state = Learning (endp, tokens, window)}
      | Some (hash, tokens_in_line) ->
        let recovery = Merlin_recovery.from_parser ~endp t.parser in
        let recovery' = filter_recovery hash wisdom recovery in
        let tokens = List.rev tokens in
        let tokens = List.drop_while
            ~f:(fun item -> endp.Lexing.pos_lnum =
                            (Merlin_lexer.item_start item).Lexing.pos_lnum)
            tokens
        in
        let rec aux t recovery' = function
          | Merlin_lexer.Error _ :: tokens -> aux t recovery' tokens
          | [] -> recover_from t (s,tok,e) recovery
          | Merlin_lexer.Valid (s,tok,e) :: tokens ->
            let t = recover_from t (s,tok,e) recovery' in
            match t.state with
            | Recovering recovery' -> aux t recovery' tokens
            | Learning _ -> assert false
            | Normal ->
              List.fold_left ~f:(step warnings) tokens ~init:t
        in
        aux t recovery' tokens
    in
    match t.state with
    | Recovering recovery -> recover_from t (s,tok,e) recovery
    | Learning (endp, tokens, window) -> learn_from t endp tokens window
    | Normal ->
      let token' = (s,tok,e) in
      match Merlin_recovery.feed_normal ~record_comment token' t.parser with
      | `Reject invalid_parser ->
        let error = Error_classifier.from invalid_parser token' in
        learn_from
          {t with errors = error :: (pop warnings) @ t.errors}
          e [token]
          (Merlin_student.linewindow ())
        (*let recovery = Merlin_recovery.from_parser e t.parser in
        Logger.infojf section ~title:"entering recovery"
          Merlin_recovery.dump recovery;
        let error = Error_classifier.from invalid_parser token in
        recover_from
          {t with errors = error :: (pop warnings) @ t.errors}
          token
          recovery*)
      | `Step parser ->
        {t with errors = (pop warnings) @ t.errors; parser }

let step token t =
  let warnings = ref [] in
  Parsing_aux.catch_warnings warnings
    (fun () -> step warnings t token)
