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

(** Types extract from AltErgoLib Utils.util and Utils.options *)

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

type options = {
  debug : bool;
  debug_ac : bool;
  debug_adt : bool;
  debug_arith : bool;
  debug_arrays : bool;
  debug_bitv : bool;
  debug_cc : bool;
  debug_combine : bool;
  debug_constr : bool;
  debug_explanations : bool;
  debug_fm : bool;
  debug_fpa : int;
  debug_gc : bool;
  debug_interpretation : bool;
  debug_ite : bool;
  debug_matching : int;
  debug_sat : bool;
  debug_split : bool;
  debug_sum : bool;
  debug_triggers : bool;
  debug_types : bool;
  debug_typing : bool;
  debug_uf : bool;
  debug_unsat_core : bool;
  debug_use : bool;
  debug_warnings : bool;
  rule : int;

  case_split_policy : case_split_policy;
  enable_adts_cs : bool;
  max_split : int;

  replay : bool;
  replay_all_used_context : bool;
  replay_used_context : bool;
  save_used_context : bool;

  answers_with_loc : bool;
  frontend : frontend;
  input_format : input_format;
  infer_input_format : bool;
  parse_only : bool;
  parsers : string list;
  preludes : string list;
  type_only : bool;
  type_smt2 : bool;

  disable_weaks : bool;
  enable_assertions : bool;

  age_bound : int;
  fm_cross_limit : int;
  steps_bound : int;

  interpretation : int;
  model : model;

  output_format : output_format;
  infer_output_format : bool;
  unsat_core : bool;


  verbose : bool;

  greedy : bool;
  instantiate_after_backjump : bool;
  max_multi_triggers_size : int;
  nb_triggers : int;
  no_ematching : bool;
  no_user_triggers : bool;
  normalize_instances : bool;
  triggers_var : bool;

  arith_matching : bool;
  bottom_classes : bool;
  cdcl_tableaux_inst : bool;
  cdcl_tableaux_th : bool;
  disable_flat_formulas_simplification : bool;
  enable_restarts : bool;
  minimal_bj : bool;
  no_backjumping : bool;
  no_backward : bool;
  no_decisions : bool;
  no_decisions_on : string list;
  no_sat_learning : bool;
  sat_solver : sat_solver;
  tableaux_cdcl : bool;

  disable_ites : bool;
  inline_lets : bool;
  rewriting : bool;
  term_like_pp : bool;

  disable_adts : bool;
  no_ac : bool;
  no_contracongru : bool;
  no_fm : bool;
  no_nla : bool;
  no_tcp : bool;
  no_theory : bool;
  restricted : bool;
  tighten_vars : bool;
  use_fpa : bool;
  timers : bool;

  file : string;
}

type results = {
  results : string list;
  errors : string list;
  warnings : string list;
  debugs : string list;
  model : string list;
  unsat_core : string list;
}

(** Return a record containing initialise value for options in Alt-Ergo.
    This options values are the same as Alt-Ergo default values *)
val init_options : unit -> options