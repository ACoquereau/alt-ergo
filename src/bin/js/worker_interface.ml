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

let init_options () = {
  debug = false;
  debug_ac = false;
  debug_adt = false;
  debug_arith = false;
  debug_arrays = false;
  debug_bitv = false;
  debug_cc = false;
  debug_combine = false;
  debug_constr = false;
  debug_explanations = false;
  debug_fm = false;
  debug_fpa = 0;
  debug_gc = false;
  debug_interpretation = false;
  debug_ite = false;
  debug_matching = 0;
  debug_sat = false;
  debug_split = false;
  debug_sum = false;
  debug_triggers = false;
  debug_types = false;
  debug_typing = false;
  debug_uf = false;
  debug_unsat_core = false;
  debug_use = false;
  debug_warnings = false;
  rule = (-1);

  case_split_policy = AfterTheoryAssume;
  enable_adts_cs = false;
  max_split = 1_000_000;

  replay = false;
  replay_all_used_context = false;
  replay_used_context = false;
  save_used_context = false;

  answers_with_loc = false;
  frontend = Legacy;
  input_format = Native;
  infer_input_format = false;
  parse_only = false;
  parsers = [];
  preludes = [];
  type_only = false;
  type_smt2 = false;

  disable_weaks = false;
  enable_assertions = false;

  age_bound = 50;
  fm_cross_limit = 10_000;
  steps_bound = (-1);

  interpretation = 0;
  model = MNone;

  output_format = Native;
  infer_output_format = false;
  unsat_core = false;


  verbose = false;

  greedy = false;
  instantiate_after_backjump = false;
  max_multi_triggers_size = 4;
  nb_triggers = 2;
  no_ematching = false;
  no_user_triggers = false;
  normalize_instances = false;
  triggers_var = false;

  arith_matching = false;
  bottom_classes = false;
  cdcl_tableaux_inst = false;
  cdcl_tableaux_th = false;
  disable_flat_formulas_simplification = false;
  enable_restarts = false;
  minimal_bj = false;
  no_backjumping = false;
  no_backward = false;
  no_decisions = false;
  no_decisions_on = [];
  no_sat_learning = false;
  sat_solver = CDCL_Tableaux;
  tableaux_cdcl = false;

  disable_ites = false;
  inline_lets = false;
  rewriting = false;
  term_like_pp = false;

  disable_adts = false;
  no_ac = false;
  no_contracongru = false;
  no_fm = false;
  no_nla = false;
  no_tcp = false;
  no_theory = false;
  restricted = false;
  tighten_vars = false;
  use_fpa = false;
  timers = false;

  file = "filename";
}
