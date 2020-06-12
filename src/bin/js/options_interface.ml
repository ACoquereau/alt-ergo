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

open AltErgoLib
open Worker_interface

let set_case_split_policy = function
  | AfterTheoryAssume -> Util.AfterTheoryAssume
  | BeforeMatching -> Util.BeforeMatching
  | AfterMatching -> Util.AfterMatching

let set_input_format = function
  | Native -> Options.Native
  | Smtlib2 -> Options.Smtlib2
  | Why3 -> Options.Why3
  | Unknown s -> Options.Unknown s

let set_output_format = function
  | Native -> Options.Native
  | Smtlib2 -> Options.Smtlib2
  | Why3 -> Options.Why3
  | Unknown s -> Options.Unknown s

let set_sat_solver = function
  | CDCL -> Util.CDCL
  | CDCL_Tableaux -> Util.CDCL_Tableaux
  | Tableaux -> Util.Tableaux
  | Tableaux_CDCL -> Util.Tableaux_CDCL

let set_model = function
  | MNone -> Options.MNone
  | MComplete -> Options.MComplete
  | MAll -> Options.MAll
  | MDefault -> Options.MDefault

let set_no_decisions_on l =
  List.fold_left (fun acc d ->
      Util.SS.add d acc
    ) Util.SS.empty l

let set_frontend = function
  | Legacy -> "legacy"
  | Unknown f -> f

let set_options r =
  Options.set_frontend (set_frontend r.frontend);
  Options.set_debug r.debug;
  Options.set_debug_ac r.debug_ac ;
  Options.set_debug_adt r.debug_adt ;
  Options.set_debug_arith r.debug_arith;
  Options.set_debug_arrays r.debug_arrays;
  Options.set_debug_bitv r.debug_bitv;
  Options.set_debug_cc r.debug_cc;
  Options.set_debug_combine r.debug_combine;
  Options.set_debug_constr r.debug_constr;
  Options.set_debug_explanations r.debug_explanations;
  Options.set_debug_fm r.debug_fm;
  Options.set_debug_fpa r.debug_fpa;
  Options.set_debug_gc r.debug_gc;
  Options.set_debug_interpretation r.debug_interpretation;
  Options.set_debug_ite r.debug_ite;
  Options.set_debug_matching r.debug_matching;
  Options.set_debug_sat r.debug_sat;
  Options.set_debug_split r.debug_split;
  Options.set_debug_sum r.debug_sum;
  Options.set_debug_triggers r.debug_triggers;
  Options.set_debug_types r.debug_types;
  Options.set_debug_typing r.debug_typing;
  Options.set_debug_uf r.debug_uf;
  Options.set_debug_unsat_core r.debug_unsat_core;
  Options.set_debug_use r.debug_use;
  Options.set_debug_warnings r.debug_warnings;
  Options.set_rule r.rule;

  Options.set_case_split_policy (set_case_split_policy r.case_split_policy);
  Options.set_enable_adts_cs r.enable_adts_cs;
  Options.set_max_split (Numbers.Q.from_int r.max_split);

  Options.set_replay r.replay;
  Options.set_replay_all_used_context r.replay_all_used_context;
  Options.set_replay_used_context r.replay_used_context;
  Options.set_save_used_context r.save_used_context;

  Options.set_answers_with_locs r.answers_with_loc;
  Options.set_input_format (set_input_format r.input_format);
  Options.set_infer_input_format r.infer_input_format;
  Options.set_parse_only r.parse_only;
  Options.set_parsers r.parsers;
  Options.set_preludes r.preludes;
  Options.set_type_only r.type_only;
  Options.set_type_smt2 r.type_smt2;

  Options.set_disable_weaks r.disable_weaks;
  Options.set_enable_assertions r.enable_assertions;

  Options.set_age_bound r.age_bound;
  Options.set_fm_cross_limit (Numbers.Q.from_int r.fm_cross_limit);
  Options.set_steps_bound r.steps_bound;

  Options.set_interpretation r.interpretation;
  Options.set_model (set_model r.model);

  Options.set_output_format (set_output_format r.output_format);
  Options.set_infer_output_format r.infer_output_format;
  Options.set_unsat_core r.unsat_core;

  Options.set_verbose r.verbose;

  Options.set_greedy r.greedy;
  Options.set_instantiate_after_backjump r.instantiate_after_backjump;
  Options.set_max_multi_triggers_size r.max_multi_triggers_size;
  Options.set_nb_triggers r.nb_triggers;
  Options.set_no_ematching r.no_ematching;
  Options.set_no_user_triggers r.no_user_triggers;
  Options.set_normalize_instances r.normalize_instances;
  Options.set_triggers_var r.triggers_var;

  Options.set_arith_matching r.arith_matching;
  Options.set_bottom_classes r.bottom_classes;
  Options.set_cdcl_tableaux_inst r.cdcl_tableaux_inst;
  Options.set_cdcl_tableaux_th r.cdcl_tableaux_th;
  Options.set_disable_flat_formulas_simplification
    r.disable_flat_formulas_simplification;
  Options.set_enable_restarts r.enable_restarts;
  Options.set_minimal_bj r.minimal_bj;
  Options.set_no_backjumping r.no_backjumping;
  Options.set_no_backward r.no_backward;
  Options.set_no_decisions r.no_decisions;
  Options.set_no_decisions_on (set_no_decisions_on r.no_decisions_on);
  Options.set_no_sat_learning r.no_sat_learning;
  Options.set_sat_solver (set_sat_solver r.sat_solver);
  Options.set_tableaux_cdcl r.tableaux_cdcl;

  Options.set_disable_ites r.disable_ites;
  Options.set_inline_lets r.inline_lets;
  Options.set_rewriting r.rewriting;
  Options.set_term_like_pp r.term_like_pp;

  Options.set_disable_adts r.disable_adts;
  Options.set_no_ac r.no_ac;
  Options.set_no_contracongru r.no_contracongru;
  Options.set_no_fm r.no_fm;
  Options.set_no_nla r.no_nla;
  Options.set_no_tcp r.no_tcp;
  Options.set_no_theory r.no_theory;
  Options.set_restricted r.restricted;
  Options.set_tighten_vars r.tighten_vars;
  Options.set_use_fpa r.use_fpa;
  Options.set_timers r.timers;

  Options.set_file r.file;
