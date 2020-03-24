(******************************************************************************)
(*                                                                            *)
(*     The Alt-Ergo theorem prover                                            *)
(*     Copyright (C) 2006-2013                                                *)
(*                                                                            *)
(*     Sylvain Conchon                                                        *)
(*     Evelyne Contejean                                                      *)
(*                                                                            *)
(*     Francois Bobot                                                         *)
(*     Mohamed Iguernelala                                                    *)
(*     Stephane Lescuyer                                                      *)
(*     Alain Mebsout                                                          *)
(*                                                                            *)
(*     CNRS - INRIA - Universite Paris Sud                                    *)
(*                                                                            *)
(*     This file is distributed under the terms of the Apache Software        *)
(*     License version 2.0                                                    *)
(*                                                                            *)
(*  ------------------------------------------------------------------------  *)
(*                                                                            *)
(*     Alt-Ergo: The SMT Solver For Software Verification                     *)
(*     Copyright (C) 2013-2018 --- OCamlPro SAS                               *)
(*                                                                            *)
(*     This file is distributed under the terms of the Apache Software        *)
(*     License version 2.0                                                    *)
(*                                                                            *)
(******************************************************************************)

open AltErgoLib
open Options
open Format

module type PARSER_INTERFACE = sig
  val file : Lexing.lexbuf -> Parsed.file
  val expr : Lexing.lexbuf -> Parsed.lexpr
  val trigger : Lexing.lexbuf -> Parsed.lexpr list * bool
end

let parsers = ref ([] : (string * (module PARSER_INTERFACE)) list)

let register_parser ~lang new_parser =
  if List.mem_assoc lang !parsers then
    begin
      eprintf
        "Warning: A parser for extension %S is already registered. \
         It will be hidden !@." lang;
    end;
  parsers := (lang, new_parser) :: !parsers

let debug_no_parser_for_no_extension () =
  if verbose() then
    eprintf
      "error: no extension found, can't infer input format@."

let debug_no_parser_for_extension s =
  if verbose() then
    eprintf
      "error: no parser registered for ext %S @." s

let debug_no_parser_for_input_format s =
  if verbose() then
    eprintf
      "error: no parser registered for the provided input format %S ?@." s

let get_input_parser () =
  match Options.input_format () with
  | Options.Native -> begin
      try List.assoc ".ae" !parsers
      with Not_found ->
        debug_no_parser_for_input_format "ae";
        exit 1
    end
  | Options.Smtlib2 -> begin
      try List.assoc ".smt2" !parsers
      with Not_found ->
        debug_no_parser_for_input_format "smtlib";
        exit 1
    end
  | Options.Why3 -> begin
      try List.assoc ".why3" !parsers
      with Not_found ->
        debug_no_parser_for_input_format "why3";
        exit 1
    end
  | Options.Unknown s -> begin
      debug_no_parser_for_input_format s;
      exit 1
    end

let get_parser lang_opt =
  match lang_opt with
  | Some lang -> begin
      if Options.infer_input_format () then
        try List.assoc lang !parsers
        with Not_found ->
          debug_no_parser_for_extension lang;
          exit 1
      else
        get_input_parser ()
    end
  | None ->
    if Options.infer_input_format () then begin
      debug_no_parser_for_no_extension ();
      exit 1 end
    else
      get_input_parser ()


let parse_file ?lang lexbuf =
  let module Parser = (val get_parser lang : PARSER_INTERFACE) in
  Parser.file lexbuf

let parse_expr ?lang lexbuf =
  let module Parser = (val get_parser lang : PARSER_INTERFACE) in
  Parser.expr lexbuf

let parse_trigger ?lang lexbuf =
  let module Parser = (val get_parser lang : PARSER_INTERFACE) in
  Parser.trigger lexbuf

(* pre-condition: f is of the form f'.zip *)
let extract_zip_file f =
  let cin = MyZip.open_in f in
  try
    match MyZip.entries cin with
    | [e] when not (MyZip.is_directory e) ->
      if verbose () then
        eprintf
          "I'll read the content of '%s' in the given zip@."
          (MyZip.filename e);
      let content = MyZip.read_entry cin e in
      MyZip.close_in cin;
      content
    | _ ->
      MyZip.close_in cin;
      raise (Arg.Bad
               (sprintf "%s '%s' %s@?"
                  "The zipped file" f
                  "should contain exactly one file."))
  with e ->
    MyZip.close_in cin;
    raise e

let parse_input_file file =
  if verbose() then fprintf fmt "[input_lang] parsing file \"%s\"@." file;
  let cin, lb, opened_cin, ext =
    if Filename.check_suffix file ".zip" then
      let ext = Filename.extension (Filename.chop_extension file) in
      let file_content = extract_zip_file file in
      stdin, Lexing.from_string file_content, false, ext
    else
      let ext = Filename.extension file in
      if not (String.equal file "") then
        let cin = open_in file in
        cin, Lexing.from_channel cin, true, ext
      else
        stdin, Lexing.from_channel stdin, false, ext
  in
  try
    (if Options.infer_output_format () then
       let ext =
         try Str.string_after ext 1
         with Invalid_argument _ -> ext
       in
       match Options.match_extension ext with
       | "native" -> Options.set_output_format Native
       | "smtlib" -> Options.set_output_format Smtlib2
       | "why3" -> Options.set_output_format Why3
       (* | "szs" -> Options.set_output_format OSZS *)
       | _ ->
         if Options.infer_input_format () then
           Format.eprintf "Warning: This extension is not supported@."
         else
           match Options.input_format () with
           | Options.Native -> Options.set_output_format Native
           | Options.Smtlib2 -> Options.set_output_format Smtlib2
           | Options.Why3 -> Options.set_output_format Why3
           | Options.Unknown s ->
             Format.eprintf "Warning: The extension %s is not supported@." s
    );
    let ext = if String.equal ext "" then None else Some ext in
    let a = parse_file ?lang:ext lb in
    if opened_cin then close_in cin;
    a
  with
  | Errors.Lexical_error (loc, s) ->
    Loc.report err_formatter loc;
    eprintf "Lexical error: %s\n@." s;
    if opened_cin then close_in cin;
    exit 1

  | Errors.Syntax_error (loc, s) ->
    Loc.report err_formatter loc;
    eprintf "Syntax error when reading token %S\n@." s;
    if opened_cin then close_in cin;
    exit 1

  | Parsing.Parse_error ->
    Loc.report err_formatter (Lexing.dummy_pos,Lexing.dummy_pos);
    eprintf "Syntax error\n@.";
    if opened_cin then close_in cin;
    exit 1


let parse_problem ~filename ~preludes =
  Parsers_loader.load ();
  let acc = parse_input_file filename in
  List.fold_left
    (fun acc prelude ->
       let prelude =
         if Sys.file_exists prelude then prelude
         else Config.preludesdir ^ "/" ^ prelude
       in
       List.rev_append (List.rev (parse_input_file prelude)) acc)
    acc (List.rev preludes)
