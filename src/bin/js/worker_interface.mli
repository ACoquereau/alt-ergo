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

(** {1 Worker interface module} *)

(** This module aims to facilitate the exhanges from/to the Alt-Ergo's worker *)

(** {2 Option types} *)

(** Types extract from AltErgoLib Utils.util and Utils.options usefull for
    interact with the worker *)

type model = MNone | MDefault | MAll | MComplete

type input_format = Native | Smtlib2 | Why3 (* | SZS *) | Unknown of string
type output_format = input_format

type case_split_policy =
  | AfterTheoryAssume (* default *)
  | BeforeMatching
  | AfterMatching

type sat_solver =
  | Tableaux
  | Tableaux_CDCL
  | CDCL
  | CDCL_Tableaux

type frontend =
  | Legacy
  | Unknown of string

(** Record type that contains all options that can be set for the Alt-Ergo's
    worker. *)
type options = {
  debug : bool option;
  debug_ac : bool option;
  debug_adt : bool option;
  debug_arith : bool option;
  debug_arrays : bool option;
  debug_bitv : bool option;
  debug_cc : bool option;
  debug_combine : bool option;
  debug_constr : bool option;
  debug_explanations : bool option;
  debug_fm : bool option;
  debug_fpa : int option;
  debug_gc : bool option;
  debug_interpretation : bool option;
  debug_ite : bool option;
  debug_matching : int option;
  debug_sat : bool option;
  debug_split : bool option;
  debug_sum : bool option;
  debug_triggers : bool option;
  debug_types : bool option;
  debug_typing : bool option;
  debug_uf : bool option;
  debug_unsat_core : bool option;
  debug_use : bool option;
  debug_warnings : bool option;
  rule : int option;

  case_split_policy : case_split_policy option;
  enable_adts_cs : bool option;
  max_split : int option;

  replay : bool option;
  replay_all_used_context : bool option;
  replay_used_context : bool option;
  save_used_context : bool option;

  answers_with_loc : bool option;
  frontend : frontend option;
  input_format : input_format option;
  infer_input_format : bool option;
  parse_only : bool option;
  parsers : (string list) option;
  preludes : (string list) option;
  type_only : bool option;
  type_smt2 : bool option;

  disable_weaks : bool option;
  enable_assertions : bool option;

  age_bound : int option;
  fm_cross_limit : int option;
  steps_bound : int option;

  interpretation : int option;
  model : model option;

  output_format : output_format option;
  infer_output_format : bool option;
  unsat_core : bool option;


  verbose : bool option;

  greedy : bool option;
  instantiate_after_backjump : bool option;
  max_multi_triggers_size : int option;
  nb_triggers : int option;
  no_ematching : bool option;
  no_user_triggers : bool option;
  normalize_instances : bool option;
  triggers_var : bool option;

  arith_matching : bool option;
  bottom_classes : bool option;
  cdcl_tableaux_inst : bool option;
  cdcl_tableaux_th : bool option;
  disable_flat_formulas_simplification : bool option;
  enable_restarts : bool option;
  minimal_bj : bool option;
  no_backjumping : bool option;
  no_backward : bool option;
  no_decisions : bool option;
  no_decisions_on : (string list) option;
  no_sat_learning : bool option;
  sat_solver : sat_solver option;
  tableaux_cdcl : bool option;

  disable_ites : bool option;
  inline_lets : bool option;
  rewriting : bool option;
  term_like_pp : bool option;

  disable_adts : bool option;
  no_ac : bool option;
  no_contracongru : bool option;
  no_fm : bool option;
  no_nla : bool option;
  no_tcp : bool option;
  no_theory : bool option;
  restricted : bool option;
  tighten_vars : bool option;
  use_fpa : bool option;
  timers : bool option;

  file : string option;
}

type statistics =
  (string * int * int) list *
  (string * int * int) list

(** Record type that contains all results that can be returned by the
    Alt-Ergo's worker. *)
type results = {
  results : string option;
  errors : string option;
  warnings : string option;
  debugs : string option;
  statistics : statistics option;
  model : string option;
  unsat_core : string option;
}

(** {2 Functions} *)

(** {3 File functions} *)

(** Take an optional file name and the file content as a string and convert
    it to a json file into Js string *)
val file_to_json :
  string option -> string -> Js_of_ocaml.Js.js_string Js_of_ocaml.Js.t

(** Take a Js string corresponding to a Json file and decoding in into
    an optional file name and the file content *)
val file_from_json :
  Js_of_ocaml.Js.js_string Js_of_ocaml.Js.t -> string option * string

(** {3 Options functions} *)

(** Return a record containing None for all options in the option type
    Since the function set_options in options_interface set only options
    with value (Some v), this function is use to create a record with all
    field to None. *)
val init_options : unit -> options

(** Return a JS string correspondind of the encoding in Json of the options.
    Field with None value or not included in the Json.*)
val options_to_json : options -> Js_of_ocaml.Js.js_string Js_of_ocaml.Js.t

(** Get a JS string corresponding of a Json and decoding it into a record of
    the options type. If some field are not included in the Json,
    the value None is set for this fields *)
val options_from_json : Js_of_ocaml.Js.js_string Js_of_ocaml.Js.t -> options

(** {3 Results functions} *)

(** Return a record containing None for all results field in the results type *)
val init_results : unit -> results

(** Convert the results type to Json into a Js string *)
val results_to_json : results -> Js_of_ocaml.Js.js_string Js_of_ocaml.Js.t

(** Convert Js string corresponding to a Json file into the results type *)
val results_from_json : Js_of_ocaml.Js.js_string Js_of_ocaml.Js.t -> results
