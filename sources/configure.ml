#!/usr/bin/env -S ocaml unix.cma

(* Configure script

   goal: register some static configuration & check dependencies
   Steps executed:
   - parse command line options
   - compute actual config options
   - output these config options in Makefile.config
   - check dune is present
   - ask dune to check the external deps
*)

(* configuration options *)
let prefix = ref ""
let libdir = ref ""
let mandir = ref ""

let pkg = ref ""

(* Parse command line arguments *)
let () =
  let args = Arg.align [
      "--prefix", Arg.Set_string prefix, "<path> prefix directory";
      "--libdir", Arg.Set_string libdir, "<path> lib directory";
    ] in
  let anon_fun s =
    match !pkg with
    | "" -> pkg := s
    | _ ->
      Format.eprintf "Anonymous argument ignored: '%s'@." s;
      exit 1
  in
  let usage = "./configure [options]" in
  Arg.parse args anon_fun usage


(* Small wrapper to set options *)
let update name r f =
  match !r with
  | "" ->
    r := f ();
    Format.printf "Using default value for '%s' : %s@." name !r
  | s ->
    Format.printf "Using provided value for '%s' : %s@." name s


(* small wrapper around opam var *)
let opam_var v =
  let cmd = Format.asprintf "opam config var %s" v in
  let ch = Unix.open_process_in cmd in
  let s = input_line ch in
  let _ = Unix.close_process_in ch in
  s


(* Compute actual values for config options *)
let () =
  let prefix_set = !prefix <> "" in
  update "prefix" prefix (fun () -> opam_var "prefix");
  update "libdir" libdir (fun () ->
      if prefix_set
      then Filename.concat !prefix "lib"
      else opam_var "lib");
  update "mandir" mandir (fun () ->
      if prefix_set
      then Filename.concat !prefix "man"
      else opam_var "man");
  ()

(* Output config options into lib/util/config.ml *)
let () =
  let f = Filename.concat (Filename.concat "lib" "util") "config.ml" in
  let () = Format.printf "Generating file %s..." f in
  let ch = open_out f in
  let fmt = Format.formatter_of_out_channel ch in
  let () = Format.fprintf fmt
      "(* Static configuration, automatically generated by configure.ml *)@." in
  let () = Format.fprintf fmt {|let libdir = "%s"@.|} !libdir in
  let () = Format.fprintf fmt {|let mandir = "%s"@.|} !mandir in
  let () = Format.fprintf fmt {|
(* Dynamic configuration, relative to the executable path *)

let follow dir path =
  Filename.concat path dir

let abs_exe_path =
  let exe_name = Sys.executable_name in
  if not (Filename.is_relative exe_name) then exe_name
  else begin
    let cwd = Sys.getcwd () in
    Filename.concat cwd exe_name
  end

let datadir =
  abs_exe_path
  |> Filename.dirname
  |> follow Filename.parent_dir_name
  |> follow "share"
  |> follow "alt-ergo"

let pluginsdir = datadir |> follow "plugins"

let preludesdir = datadir |> follow "preludes"

|} in
  let () = close_out ch in
  let () = Format.printf "done.@." in
  ()

(* Output config options into Makefile.config *)
let () =
  let () = Format.printf "Generating file Makefile.config..." in
  let ch = open_out "Makefile.config" in
  let fmt = Format.formatter_of_out_channel ch in
  let () = Format.fprintf fmt "# Generated by configure@." in
  let () = Format.fprintf fmt "prefix=%s@." !prefix in
  let () = Format.fprintf fmt "libdir=%s@." !libdir in
  let () = close_out ch in
  let () = Format.printf "done.@." in
  ()

(* Small wrapper to read all the contents of a channel *)
let read_all ch =
  let b = Buffer.create 113 in
  try
    while true do
      Buffer.add_channel b ch 30
    done;
    assert false
  with End_of_file ->
    Buffer.contents b

(* check that dune is present *)
let () =
  let cmd = Format.asprintf "which dune" in
  let ch = Unix.open_process_in cmd in
  let _ = read_all ch in
  let res = Unix.close_process_in ch in
  match res with
  | Unix.WEXITED 0 ->
    Format.printf "Found dune in path.@."
  | _ ->
    Format.eprintf "ERROR: Couldn't find dune in env@.";
    exit 1

(* run dune to check that dependencies are installed *)
let () =
  let p_opt = match !pkg with
    | "" -> ""
    | s -> Format.asprintf "-p %s" s
  in
  let cmd = Format.asprintf
      "dune external-lib-deps --display=quiet --missing %s @install"
      p_opt
  in
  let ch = Unix.open_process_in cmd in
  let _ = read_all ch in
  let res = Unix.close_process_in ch in
  match res with
  | Unix.WEXITED 0 ->
    Format.printf "All deps are installed.@."
  | _ ->
    (* dune already prints the missing libs on stderr *)
    exit 2

let () =
  Format.printf "Good to go !@."

