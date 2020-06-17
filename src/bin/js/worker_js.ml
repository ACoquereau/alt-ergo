(******************************************************************************)
(*                                                                            *)
(*     Alt-Ergo: The SMT Solver For Software Verification                     *)
(*     Copyright (C) 2018-2020 --- OCamlPro SAS                               *)
(*                                                                            *)
(*     This file is distributed under the terms of the license indicated      *)
(*     in the file 'License.OCamlPro'. If 'License.OCamlPro' is not           *)
(*     present, please contact us to clarify licensing.                       *)
(*                                                                            *)
(******************************************************************************)

open Js_of_ocaml
open Js_of_ocaml_lwt
open Data_encoding
open Alt_ergo_common
open AltErgoLib

(* Internal state while iterating over input statements *)
type 'a state = {
  env : 'a;
  ctx   : Commands.sat_tdecl list;
  local : Commands.sat_tdecl list;
  global : Commands.sat_tdecl list;
}

let main file =
  let buf_std = Buffer.create 10 in
  Options.set_fmt_std (Format.formatter_of_buffer buf_std);
  let buf_err = Buffer.create 10 in
  Options.set_fmt_err (Format.formatter_of_buffer buf_err);
  let buf_wrn = Buffer.create 10 in
  Options.set_fmt_wrn (Format.formatter_of_buffer buf_wrn);
  let buf_dbg = Buffer.create 10 in
  Options.set_fmt_dbg (Format.formatter_of_buffer buf_dbg);
  let buf_mdl = Buffer.create 10 in
  Options.set_fmt_mdl (Format.formatter_of_buffer buf_mdl);
  let buf_usc = Buffer.create 10 in
  Options.set_fmt_usc (Format.formatter_of_buffer buf_usc);

  Input_frontend.register_legacy ();

  let module SatCont =
    (val (Sat_solver.get_current ()) : Sat_solver_sig.SatContainer) in

  let module TH =
    (val
      (if Options.get_no_theory() then (module Theory.Main_Empty : Theory.S)
       else (module Theory.Main_Default : Theory.S)) : Theory.S ) in

  let module SAT = SatCont.Make(TH) in

  let module FE = Frontend.Make (SAT) in

  let compute_used_context env dep =
    let compute_used_context_aux l =
      List.fold_left (fun acc f ->
          match Expr.form_view f with
          | Lemma {name=name;loc=loc;_} -> 
            let b,e = loc in
            (name,b.Lexing.pos_lnum,e.Lexing.pos_lnum) :: acc
          | _ -> acc
        )[] l in
    let used,unused = SAT.retrieve_used_context env dep in
    (compute_used_context_aux used,compute_used_context_aux unused) in

  let solve all_context (cnf, goal_name) =
    let used_context = FE.choose_used_context all_context ~goal_name in
    SAT.reset_refs ();
    let env,_,dep =
      List.fold_left
        (FE.process_decl
           FE.print_status used_context)
        (SAT.empty (), true, Explanation.empty) cnf in

    if Options.get_save_used_context () then begin
      Format.eprintf "Solve finished, print context@.";
      let context = compute_used_context env dep in

      List.iter (fun (used,_,_) ->
          Format.eprintf "Used : %s@." used
        ) (fst context);
    end;
  in

  let typed_loop all_context state td =
    match td.Typed.c with
    | Typed.TGoal (_, kind, name, _) ->
      let l = state.local @ state.global @ state.ctx in
      let cnf = List.rev @@ Cnf.make l td in
      let () = solve all_context (cnf, name) in
      begin match kind with
        | Typed.Check
        | Typed.Cut -> { state with local = []; }
        | _ -> { state with global = []; local = []; }
      end
    | Typed.TAxiom (_, s, _, _) when Typed.is_global_hyp s ->
      let cnf = Cnf.make state.global td in
      { state with global = cnf; }
    | Typed.TAxiom (_, s, _, _) when Typed.is_local_hyp s ->
      let cnf = Cnf.make state.local td in
      { state with local = cnf; }
    | _ ->
      let cnf = Cnf.make state.ctx td in
      { state with ctx = cnf; }
  in

  let (module I : Input.S) = Input.find (Options.get_frontend ()) in
  let parsed () =
    try
      Options.Time.start ();
      Options.set_is_gui false;
      I.parse_file ~file ~format:None
    with
    | Parsing.Parse_error ->
      Printer.print_err "%a" Errors.report
        (Syntax_error ((Lexing.dummy_pos,Lexing.dummy_pos),""));
      raise Exit
    | Errors.Error e ->
      Printer.print_err "%a" Errors.report e;
      raise Exit
  in
  let all_used_context = FE.init_all_used_context () in
  let typing_loop state p =
    try
      let l, env = I.type_parsed state.env p in
      List.fold_left (typed_loop all_used_context) { state with env; } l
    with Errors.Error e ->
      Printer.print_err "%a" Errors.report e;
      raise Exit
  in

  let state = {
    env = I.empty_env;
    ctx = [];
    local = [];
    global = [];
  } in

  begin
    try let _ : _ state =
          Seq.fold_left typing_loop state (parsed ()) in ()
    with Exit -> () end;

  {
    Worker_interface.results = [Buffer.contents buf_std];
    Worker_interface.errors = [Buffer.contents buf_err];
    Worker_interface.warnings = [Buffer.contents buf_wrn];
    Worker_interface.debugs = [Buffer.contents buf_dbg];
    Worker_interface.model = [Buffer.contents buf_mdl];
    Worker_interface.unsat_core = [Buffer.contents buf_usc];
  }

type t = string * int * string
let encode (v,w,x) =
  let encoding = tup3 string float string in
  let x = (v,w,x) in
  let c = Json.construct encoding x in
  Js.string (Json.to_string c)

(* let w = Json.destruct encoding j in
   assert (v = w)
*)


let results_encoding =
  let open Worker_interface in
  conv
    (fun { results; errors; warnings; debugs; model; unsat_core } ->
       (results, errors, warnings, debugs, model, unsat_core))
    (fun (results, errors, warnings, debugs, model, unsat_core) ->
       { results; errors; warnings; debugs; model; unsat_core })
    (obj6
       (req "results" (list string))
       (req "errors" (list string))
       (req "warnings" (list string))
       (req "debugs" (list string))
       (req "model" (list string))
       (req "unsat_core" (list string)))

(** Worker initialisation
    Run Alt-ergo with the input file (string)
    and the corresponding set of options
    Return a couple of list for status (one per goal) and errors *)
let () =
  Worker.set_onmessage (fun (file,(options:Worker_interface.options)) ->
      Lwt_js_events.async (fun () ->
          Options_interface.set_options options;
          Options.set_file_for_js "";
          let results = main file in
          let json_results = Json.construct results_encoding results in
          Worker.post_message (Js.string (Json.to_string json_results));  
          (* Worker.post_message results; *)
          Lwt.return ();
        )
    )
