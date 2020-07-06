(******************************************************************************)
(*                                                                            *)
(*     Alt-Ergo: The SMT Solver For Software Verification                     *)
(*     Copyright (C) 2018-2020 --- OCamlPro SAS                               *)
(*                                                                            *)
(*     This file is distributed under the terms of the license indicated      *)
(*     in the file 'License.OCamlPro'. If 'License.OCamlPro' is not           *)
(*     present please contact us to clarify licensing.                        *)
(*                                                                            *)
(******************************************************************************)
open Js_of_ocaml
open Data_encoding

(** Types extract from AltErgoLib Utils.util and Utils.options *)

type model = MNone | MDefault | MAll | MComplete

let model_encoding =
  union [
    case
      (Tag 1)
      ~title:"MNone"
      (constant "MNone")
      (function MNone -> Some () | _ -> None)
      (fun () -> MNone);
    case
      (Tag 2)
      ~title:"MDefault"
      (constant "MDefault")
      (function MDefault -> Some () | _ -> None)
      (fun () -> MDefault);
    case
      (Tag 3)
      ~title:"MAll"
      (constant "MAll")
      (function MAll -> Some () | _ -> None)
      (fun () -> MAll);
    case
      (Tag 4)
      ~title:"MComplete"
      (constant "MComplete")
      (function MComplete -> Some () | _ -> None)
      (fun () -> MComplete);

  ]

type input_format = Native | Smtlib2 | Why3 (* | SZS *) | Unknown of string
type output_format = input_format

let format_encoding =
  union [
    case(Tag 1)
      ~title:"Native"
      (constant "Native")
      (function Native -> Some () | _ -> None)
      (fun () -> Native);
    case(Tag 2)
      ~title:"Smtlib2"
      (constant "Smtlib2")
      (function Smtlib2 -> Some () | _ -> None)
      (fun () -> Smtlib2);
    case(Tag 3)
      ~title:"Why3"
      (constant "Why3")
      (function Why3 -> Some () | _ -> None)
      (fun () -> Why3);
    case(Tag 4)
      ~title:"Unknown"
      (obj1 (req "Unknown" string))
      (function Unknown s -> Some s | _ -> None)
      (fun s -> Unknown(s));
  ]

type case_split_policy =
  | AfterTheoryAssume (* default *)
  | BeforeMatching
  | AfterMatching

let case_split_policy_encoding =
  union [
    case(Tag 1)
      ~title:"AfterTheoryAssume"
      (constant "AfterTheoryAssume")
      (function AfterTheoryAssume -> Some () | _ -> None)
      (fun () -> AfterTheoryAssume);
    case(Tag 2)
      ~title:"BeforeMatching"
      (constant "BeforeMatching")
      (function BeforeMatching -> Some () | _ -> None)
      (fun () -> BeforeMatching);
    case(Tag 3)
      ~title:"AfterMatching"
      (constant "AfterMatching")
      (function AfterMatching -> Some () | _ -> None)
      (fun () -> AfterMatching);
  ]

type sat_solver =
  | Tableaux
  | Tableaux_CDCL
  | CDCL
  | CDCL_Tableaux

let sat_solver_encoding =
  union [
    case(Tag 1)
      ~title:"Tableaux"
      (constant "Tableaux")
      (function Tableaux -> Some () | _ -> None)
      (fun () -> Tableaux);
    case(Tag 2)
      ~title:"Tableaux_CDCL"
      (constant "Tableaux_CDCL")
      (function Tableaux_CDCL -> Some () | _ -> None)
      (fun () -> Tableaux_CDCL);
    case(Tag 3)
      ~title:"CDCL"
      (constant "CDCL")
      (function CDCL -> Some () | _ -> None)
      (fun () -> CDCL);
    case(Tag 4)
      ~title:"CDCL_Tableaux"
      (constant "CDCL_Tableaux")
      (function CDCL_Tableaux -> Some () | _ -> None)
      (fun () -> CDCL_Tableaux);
  ]

type frontend =
  | Legacy
  | Unknown of string

let frontend_encoding =
  union [
    case(Tag 1)
      ~title:"Legacy"
      (constant "Legacy")
      (function Legacy -> Some () | _ -> None)
      (fun () -> Legacy);
    case(Tag 2)
      ~title:"Unknown"
      (obj1 (req "Unknown" string))
      (function Unknown s -> Some s | _ -> None)
      (fun s -> Unknown(s));
  ]

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

let init_options () = {
  debug = None;
  debug_ac = None;
  debug_adt = None;
  debug_arith = None;
  debug_arrays = None;
  debug_bitv = None;
  debug_cc = None;
  debug_combine = None;
  debug_constr = None;
  debug_explanations = None;
  debug_fm = None;
  debug_fpa = None;
  debug_gc = None;
  debug_interpretation = None;
  debug_ite = None;
  debug_matching = None;
  debug_sat = None;
  debug_split = None;
  debug_sum = None;
  debug_triggers = None;
  debug_types = None;
  debug_typing = None;
  debug_uf = None;
  debug_unsat_core = None;
  debug_use = None;
  debug_warnings = None;
  rule = None;

  case_split_policy = None;
  enable_adts_cs = None;
  max_split = None;

  replay = None;
  replay_all_used_context = None;
  replay_used_context = None;
  save_used_context = None;

  answers_with_loc = None;
  frontend = None;
  input_format = None;
  infer_input_format = None;
  parse_only = None;
  parsers = None;
  preludes = None;
  type_only = None;
  type_smt2 = None;

  disable_weaks = None;
  enable_assertions = None;

  age_bound = None;
  fm_cross_limit = None;
  steps_bound = None;

  interpretation = None;
  model = None;

  output_format = None;
  infer_output_format = None;
  unsat_core = None;


  verbose = None;

  greedy = None;
  instantiate_after_backjump = None;
  max_multi_triggers_size = None;
  nb_triggers = None;
  no_ematching = None;
  no_user_triggers = None;
  normalize_instances = None;
  triggers_var = None;

  arith_matching = None;
  bottom_classes = None;
  cdcl_tableaux_inst = None;
  cdcl_tableaux_th = None;
  disable_flat_formulas_simplification = None;
  enable_restarts = None;
  minimal_bj = None;
  no_backjumping = None;
  no_backward = None;
  no_decisions = None;
  no_decisions_on = None;
  no_sat_learning = None;
  sat_solver = None;
  tableaux_cdcl = None;

  disable_ites = None;
  inline_lets = None;
  rewriting = None;
  term_like_pp = None;

  disable_adts = None;
  no_ac = None;
  no_contracongru = None;
  no_fm = None;
  no_nla = None;
  no_tcp = None;
  no_theory = None;
  restricted = None;
  tighten_vars = None;
  use_fpa = None;
  timers = None;

  file = None;
}


let opt_dbg1_encoding =
  conv
    (fun dbg1 -> dbg1)
    (fun dbg1 -> dbg1)
    (obj10
       (opt "debug" bool)
       (opt "debug_ac" bool)
       (opt "debug_adt" bool)
       (opt "debug_arith" bool)
       (opt "debug_arrays" bool)
       (opt "debug_bitv" bool)
       (opt "debug_cc" bool)
       (opt "debug_combine" bool)
       (opt "debug_constr" bool)
       (opt "debug_explanations" bool)
    )

let opt_dbg2_encoding =
  conv
    (fun dbg2 -> dbg2)
    (fun dbg2 -> dbg2)
    (obj10
       (opt "debug_fm" bool)
       (opt "debug_fpa" int31)
       (opt "debug_gc" bool)
       (opt "debug_interpretation" bool)
       (opt "debug_ite" bool)
       (opt "debug_matching" int31)
       (opt "debug_sat" bool)
       (opt "debug_split" bool)
       (opt "debug_sum" bool)
       (opt "debug_triggers" bool)
    )

let opt_dbg3_encoding =
  conv
    (fun dbg3 -> dbg3)
    (fun dbg3 -> dbg3)
    (obj6
       (opt "debug_types" bool)
       (opt "debug_typing" bool)
       (opt "debug_uf" bool)
       (opt "debug_unsat_core" bool)
       (opt "debug_use" bool)
       (opt "debug_warnings" bool)
    )

let opt1_encoding =
  conv
    (fun opt1 -> opt1)
    (fun opt1 -> opt1)
    (obj8
       (opt "rule" int31)
       (opt "case_split_policy" case_split_policy_encoding)
       (opt "enable_adts_cs" bool)
       (opt "max_split" int31)
       (opt "replay" bool)
       (opt "replay_all_used_context" bool)
       (opt "save_used_context" bool)
       (opt "answers_with_loc" bool)
    )

let opt2_encoding =
  conv
    (fun opt2 -> opt2)
    (fun opt2 -> opt2)
    (obj9
       (opt "answers_with_loc" bool)
       (opt "frontend" frontend_encoding)
       (opt "input_format" format_encoding)
       (opt "infer_input_format" bool)
       (opt "parse_only" bool)
       (opt "parsers" (list string))
       (opt "preludes" (list string))
       (opt "type_only" bool)
       (opt "type_smt2" bool)
    )

let opt3_encoding =
  conv
    (fun opt3 -> opt3)
    (fun opt3 -> opt3)
    (obj10
       (opt "disable_weaks" bool)
       (opt "enable_assertions" bool)
       (opt "age_bound" int31)
       (opt "fm_cross_limit" int31)
       (opt "steps_bound" int31)
       (opt "interpretation" int31)
       (opt "model" model_encoding)
       (opt "output_format" format_encoding)
       (opt "infer_output_format" bool)
       (opt "unsat_core" bool)
    )

let opt4_encoding =
  conv
    (fun opt4 -> opt4)
    (fun opt4 -> opt4)
    (obj9
       (opt "verbose" bool)
       (opt "greedy" bool)
       (opt "instanciate_after_backjump" bool)
       (opt "max_multi_triggers_size" int31)
       (opt "nb_triggers" int31)
       (opt "no_ematching" bool)
       (opt "no_user_triggers" bool)
       (opt "normalize_instances" bool)
       (opt "triggers_var" bool)
    )

let opt5_encoding =
  conv
    (fun opt5 -> opt5)
    (fun opt5 -> opt5)
    (obj10
       (opt "arith_matchin" bool)
       (opt "bottom_classes" bool)
       (opt "cdcl_tableaux_inst" bool)
       (opt "cdcl_tableaux_th" bool)
       (opt "disable_flat_formulas_simplifiaction" bool)
       (opt "enable_restarts" bool)
       (opt "minimal_bj" bool)
       (opt "no_backjumping" bool)
       (opt "no_backward" bool)
       (opt "no_decisions" bool)
    )

let opt6_encoding =
  conv
    (fun opt6 -> opt6)
    (fun opt6 -> opt6)
    (obj10
       (opt "no_decisions_on" (list string))
       (opt "no_sat_learning" bool)
       (opt "sat_solver" sat_solver_encoding)
       (opt "tableaux_cdcl" bool)
       (opt "disable_ites" bool)
       (opt "inline_lets" bool)
       (opt "rewriting" bool)
       (opt "term_like_pp" bool)
       (opt "disable_adts" bool)
       (opt "no_ac" bool)
    )

let opt7_encoding =
  conv
    (fun opt7 -> opt7)
    (fun opt7 -> opt7)
    (obj10
       (opt "no_contracongru" bool)
       (opt "no_fm" bool)
       (opt "no_nla" bool)
       (opt "no_tcp" bool)
       (opt "no_theory" bool)
       (opt "restricted" bool)
       (opt "tighten_vars" bool)
       (opt "use_fpa" bool)
       (opt "timers" bool)
       (opt "file" string)
    )

let options_encoding =
  merge_objs opt_dbg1_encoding
    (merge_objs opt_dbg2_encoding
       (merge_objs opt_dbg3_encoding
          (merge_objs opt1_encoding
             (merge_objs opt2_encoding
                (merge_objs opt3_encoding
                   (merge_objs opt4_encoding
                      (merge_objs opt5_encoding
                         (merge_objs opt6_encoding
                            opt7_encoding))))))))

let options_to_json opt =
  let dbg_opt1 =
    (opt.debug,
     opt.debug_ac,
     opt.debug_adt,
     opt.debug_arith,
     opt.debug_arrays,
     opt.debug_bitv,
     opt.debug_cc,
     opt.debug_combine,
     opt.debug_constr,
     opt.debug_explanations)
  in
  let dbg_opt2 =
    (opt.debug_fm,
     opt.debug_fpa,
     opt.debug_gc,
     opt.debug_interpretation,
     opt.debug_ite,
     opt.debug_matching,
     opt.debug_sat,
     opt.debug_split,
     opt.debug_sum,
     opt.debug_triggers)
  in
  let dbg_opt3 =
    (opt.debug_types,
     opt.debug_typing,
     opt.debug_uf,
     opt.debug_unsat_core,
     opt.debug_use,
     opt.debug_warnings)
  in
  let all_opt1 =
    (opt.rule,
     opt.case_split_policy,
     opt.enable_adts_cs,
     opt.max_split,
     opt.replay,
     opt.replay_all_used_context,
     opt.replay_used_context,
     opt.save_used_context)
  in
  let all_opt2 =
    (opt.answers_with_loc,
     opt.frontend,
     opt.input_format,
     opt.infer_input_format,
     opt.parse_only,
     opt.parsers,
     opt.preludes,
     opt.type_only,
     opt.type_smt2)
  in
  let all_opt3 =
    (opt.disable_weaks,
     opt.enable_assertions,
     opt.age_bound,
     opt.fm_cross_limit,
     opt.steps_bound,
     opt.interpretation,
     opt.model,
     opt.output_format,
     opt.infer_output_format,
     opt.unsat_core)
  in
  let all_opt4 =
    (opt.verbose,
     opt.greedy,
     opt.instantiate_after_backjump,
     opt.max_multi_triggers_size,
     opt.nb_triggers,
     opt.no_ematching,
     opt.no_user_triggers,
     opt.normalize_instances,
     opt.triggers_var)
  in
  let all_opt5 =
    (opt.arith_matching,
     opt.bottom_classes,
     opt.cdcl_tableaux_inst,
     opt.cdcl_tableaux_th,
     opt.disable_flat_formulas_simplification,
     opt.enable_restarts,
     opt.minimal_bj,
     opt.no_backjumping,
     opt.no_backward,
     opt.no_decisions)
  in
  let all_opt6 =
    (opt.no_decisions_on,
     opt.no_sat_learning,
     opt.sat_solver,
     opt.tableaux_cdcl,
     opt. disable_ites,
     opt.inline_lets,
     opt.rewriting,
     opt.term_like_pp,
     opt.disable_adts,
     opt.no_ac)
  in
  let all_opt7 =
    (opt.no_contracongru,
     opt.no_fm,
     opt.no_nla,
     opt.no_tcp,
     opt.no_theory,
     opt.restricted,
     opt.tighten_vars,
     opt.use_fpa,
     opt.timers,
     opt.file)
  in
  let json_all_options = Json.construct options_encoding
      (dbg_opt1,
       (dbg_opt2,
        (dbg_opt3,
         (all_opt1,
          (all_opt2,
           (all_opt3,
            (all_opt4,
             (all_opt5,
              (all_opt6,
               all_opt7)))))))))
  in
  Js.string (Json.to_string json_all_options)

let options_from_json options =
  match Json.from_string (Js.to_string options) with
  | Ok opts ->
    let (dbg_opt1,
         (dbg_opt2,
          (dbg_opt3,
           (all_opt1,
            (all_opt2,
             (all_opt3,
              (all_opt4,
               (all_opt5,
                (all_opt6,
                 all_opt7))))))))) = Json.destruct options_encoding opts in
    let (debug,
         debug_ac,
         debug_adt,
         debug_arith,
         debug_arrays,
         debug_bitv,
         debug_cc,
         debug_combine,
         debug_constr,
         debug_explanations) =
      dbg_opt1 in
    let (debug_fm,
         debug_fpa,
         debug_gc,
         debug_interpretation,
         debug_ite,
         debug_matching,
         debug_sat,
         debug_split,
         debug_sum,
         debug_triggers) = dbg_opt2 in
    let (debug_types,
         debug_typing,
         debug_uf,
         debug_unsat_core,
         debug_use,
         debug_warnings) = dbg_opt3 in
    let (rule,
         case_split_policy,
         enable_adts_cs,
         max_split,
         replay,
         replay_all_used_context,
         replay_used_context,
         save_used_context) = all_opt1 in
    let (answers_with_loc,
         frontend,
         input_format,
         infer_input_format,
         parse_only,
         parsers,
         preludes,
         type_only,
         type_smt2) = all_opt2 in
    let (disable_weaks,
         enable_assertions,
         age_bound,
         fm_cross_limit,
         steps_bound,
         interpretation,
         model,
         output_format,
         infer_output_format,
         unsat_core) = all_opt3 in
    let (verbose,
         greedy,
         instantiate_after_backjump,
         max_multi_triggers_size,
         nb_triggers,
         no_ematching,
         no_user_triggers,
         normalize_instances,
         triggers_var) = all_opt4 in
    let (arith_matching,
         bottom_classes,
         cdcl_tableaux_inst,
         cdcl_tableaux_th,
         disable_flat_formulas_simplification,
         enable_restarts,
         minimal_bj,
         no_backjumping,
         no_backward,
         no_decisions) = all_opt5 in
    let (no_decisions_on,
         no_sat_learning,
         sat_solver,
         tableaux_cdcl,
         disable_ites,
         inline_lets,
         rewriting,
         term_like_pp,
         disable_adts,
         no_ac) = all_opt6 in
    let (no_contracongru,
         no_fm,
         no_nla,
         no_tcp,
         no_theory,
         restricted,
         tighten_vars,
         use_fpa,
         timers,
         file) = all_opt7 in
    {
      debug;
      debug_ac;
      debug_adt;
      debug_arith;
      debug_arrays;
      debug_bitv;
      debug_cc;
      debug_combine;
      debug_constr;
      debug_explanations;
      debug_fm;
      debug_fpa;
      debug_gc;
      debug_interpretation;
      debug_ite;
      debug_matching;
      debug_sat;
      debug_split;
      debug_sum;
      debug_triggers;
      debug_types;
      debug_typing;
      debug_uf;
      debug_unsat_core;
      debug_use;
      debug_warnings;
      rule;
      case_split_policy;
      enable_adts_cs;
      max_split;
      replay;
      replay_all_used_context;
      replay_used_context;
      save_used_context;
      answers_with_loc;
      frontend;
      input_format;
      infer_input_format;
      parse_only;
      parsers;
      preludes;
      type_only;
      type_smt2;
      disable_weaks;
      enable_assertions;
      age_bound;
      fm_cross_limit;
      steps_bound;
      interpretation;
      model;
      output_format;
      infer_output_format;
      unsat_core;
      verbose;
      greedy;
      instantiate_after_backjump;
      max_multi_triggers_size;
      nb_triggers;
      no_ematching;
      no_user_triggers;
      normalize_instances;
      triggers_var;
      arith_matching;
      bottom_classes;
      cdcl_tableaux_inst;
      cdcl_tableaux_th;
      disable_flat_formulas_simplification;
      enable_restarts;
      minimal_bj;
      no_backjumping;
      no_backward;
      no_decisions;
      no_decisions_on;
      no_sat_learning;
      sat_solver;
      tableaux_cdcl;
      disable_ites;
      inline_lets;
      rewriting;
      term_like_pp;
      disable_adts;
      no_ac;no_contracongru;
      no_fm;
      no_nla;
      no_tcp;
      no_theory;
      restricted;
      tighten_vars;
      use_fpa;
      timers;
      file
    }
  | Error _e -> assert false

type statistics =
  (string * int * int) list *
  (string * int * int) list

let statistics_encoding =
  tup2
    (list (tup3 string int31 int31))
    (list (tup3 string int31 int31))

type results = {
  results : string option;
  errors : string option;
  warnings : string option;
  debugs : string option;
  statistics : statistics option;
  model : string option;
  unsat_core : string option;
}

let init_results () = {
  results = None;
  errors = None;
  warnings = None;
  debugs = None;
  statistics = None;
  model = None;
  unsat_core = None;
}

let results_encoding =
  conv
    (fun { results; errors; warnings; debugs; statistics; model; unsat_core } ->
       (results, errors, warnings, debugs, statistics, model, unsat_core))
    (fun (results, errors, warnings, debugs, statistics, model, unsat_core) ->
       { results; errors; warnings; debugs; statistics; model; unsat_core })
    (obj7
       (opt "results" string)
       (opt "errors" string)
       (opt "warnings" string)
       (opt "debugs" string)
       (opt "statistics" statistics_encoding)
       (opt "model" string)
       (opt "unsat_core" string))

let results_to_json res =
  let json_results = Json.construct results_encoding res in
  Js.string (Json.to_string json_results)

let results_from_json res =
  match Json.from_string (Js.to_string res) with
  | Ok res -> Json.destruct results_encoding res
  | Error _e -> assert false


let file_encoding =
  conv
    (fun f -> f)
    (fun f -> f)
    (obj2 (opt "filename" string) (req "content" string))

let file_to_json filename content =
  let json_file = Json.construct file_encoding (filename,content) in
  Js.string (Json.to_string json_file)

let file_from_json res =
  match Json.from_string (Js.to_string res) with
  | Ok res -> Json.destruct file_encoding res
  | Error _e -> assert false
