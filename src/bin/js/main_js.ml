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

module Html = Dom_html

let document = Html.window##.document

(* Example of the file to prove *)
let file = ref "goal g : true"

(* This is the extension needed for the parser and corresponding to the input
   file format*)
let extension = ref ".ae"

(* Timeout *)
let timeout = ref 100.

(* This function aims to concat results from the execution of the worker and
   return them as JS.string *)
let process_results (results : Worker_interface.results) =
  let status = String.concat "\n" results.results in
  let error = String.concat "\n" results.errors in
  let warning = String.concat "\n" results.warnings in
  let debug = String.concat "\n" results.debugs in
  let model = String.concat "\n" results.model in
  let unsat_core = String.concat "\n" results.unsat_core in
  print_endline status;
  (Js.string status, Js.string error, Js.string warning,
   Js.string debug, Js.string model, Js.string unsat_core )

(* Function that run the worker. *)
let exec worker file options =
  (* create a cancelable promise that can be cancel in case of timeout *)
  let t, resolver = Lwt.task () in
  (* Set the behaviour of the worker when Lwt send an on_cancel input *)
  Lwt.on_cancel t (fun () -> worker##terminate);
  (* Get the messages returned from the worker and return them *)
  worker##.onmessage :=
    (Js_of_ocaml.Dom_html.handler (fun msg ->
         let res = msg##.data in
         Lwt.wakeup resolver res;
         Js_of_ocaml.Js._true));
  (* Start the worker with the correspondin input, here file_options *)
  worker##postMessage (file,options);
  t

(* Create the web worker and launch 2 threads.
   The first one for the timeout,
   the second on for the call to Alt-Ergo through his web worker *)
let solve () =
  let options =
    {(Worker_interface.init_options ()) with
     file = "try-alt-ergo";
     debug = true;
     verbose = true;
    } in

  let worker = Worker.create "./alt-ergo-worker.js" in

  print_endline
    ("Start Alt-Ergo with timeout of "^ (string_of_float !timeout) ^"s");

  (Lwt.pick [
      (let%lwt () = Lwt_js.sleep !timeout in
       Lwt.return (Js.string "", Js.string "Timeout",
                   Js.string "", Js.string "",
                   Js.string "", Js.string "")
      );
      (let%lwt results = exec worker !file options in
       Lwt.return (process_results results)
      )
    ]
  )

let string_input f area_name area =
  let res = document##createDocumentFragment in
  Dom.appendChild res (document##createTextNode (Js.string area_name));
  Dom.appendChild res (Html.createBr document);
  let input = f document in
  input##.value := Js.string !area;
  input##.onchange :=
    Html.handler (fun _ ->
        (try area := Js.to_string input##.value
         with Invalid_argument _ -> ());
        input##.value := Js.string !area;
        Js._false);
  Dom.appendChild res input;
  Dom.appendChild res (Html.createBr document);
  res

let float_input name value =
  let res = document##createDocumentFragment in
  Dom.appendChild res (document##createTextNode (Js.string name));
  Dom.appendChild res (Html.createBr document);
  let input = Html.createInput document in
  input##.value := Js.string (string_of_float !value);
  input##.onchange :=
    Html.handler (fun _ ->
        (try value := float_of_string (Js.to_string input##.value)
         with Invalid_argument _ -> ());
        input##.value := Js.string (string_of_float !value);
        Js._false);
  Dom.appendChild res input;
  Dom.appendChild res (Html.createBr document);
  res

let button name callback =
  let res = document##createDocumentFragment in
  let input = Html.createInput ~_type:(Js.string "submit") document in
  input##.value := Js.string name;
  input##.onclick := Html.handler callback;
  Dom.appendChild res input;
  res

let result = document##createTextNode (Js.string "")
(* update result text area *)
let print_res res =
  result##.data := res

let error = document##createTextNode (Js.string "")
(* update error text area *)
let print_error err =
  error##.data := err

let warning = document##createTextNode (Js.string "")
(* update warning text area *)
let print_warning wrn =
  warning##.data := wrn

let debug = document##createTextNode (Js.string "")
(* update error text area *)
let print_debug dbg =
  debug##.data := dbg

let model = document##createTextNode (Js.string "")
(* update model text area *)
let print_model mdl =
  model##.data := mdl

let unsat_core = document##createTextNode (Js.string "")
(* update unsat core text area *)
let print_unsat_core usc =
  unsat_core##.data := usc

let onload _ =
  let main = Js.Opt.get (document##getElementById (Js.string "main"))
      (fun () -> assert false) in
  (* Create a text area for the input file *)
  Dom.appendChild main
    (string_input Html.createTextarea "Input file to solve" file);
  Dom.appendChild main (Html.createBr document);
  (* Create a text area for the extension format *)
  Dom.appendChild main (string_input Html.createInput "Extension" extension);
  Dom.appendChild main (Html.createBr document);
  (* Create a text area for the timeout value *)
  Dom.appendChild main (float_input "Timeout" timeout);
  Dom.appendChild main (Html.createBr document);
  (* Create a button to start the solving *)
  Dom.appendChild
    main
    (button "Ask Alt-Ergo" (fun _ ->
         let div = Html.createDiv document in
         Dom.appendChild main div;
         Lwt_js_events.async (fun () ->
             (* Print "solving" until the end of the solving
                or until the timeout *)
             print_res (Js.string "Solving");
             print_error (Js.string "");
             let%lwt (res,err,wrn,dbg,mdl,usc) = solve () in
             (* Update results area *)
             print_res res;
             (* Update errors area if errors occurs at solving *)
             print_error err;
             (* Update warning area if warning occurs at solving *)
             print_warning wrn;
             (* Update debug area *)
             print_debug dbg;
             (* Update model *)
             print_model mdl;
             (* Update unsat core *)
             print_unsat_core usc;
             Lwt.return_unit);
         Js._false));
  Dom.appendChild main (Html.createBr document);
  (* Create a text area for the results *)
  Dom.appendChild main result;
  Dom.appendChild main (Html.createBr document);
  (* Create a text area for the errors *)
  Dom.appendChild main error;
  Dom.appendChild main (Html.createBr document);
  (* Create a text area for the warning *)
  Dom.appendChild main warning;
  Dom.appendChild main (Html.createBr document);
  (* Create a text area for the debug *)
  Dom.appendChild main debug;
  Dom.appendChild main (Html.createBr document);
  (* Create a text area for the model *)
  Dom.appendChild main model;
  Dom.appendChild main (Html.createBr document);
  (* Create a text area for the unsat_core *)
  Dom.appendChild main unsat_core;
  Js._false

let _ = Html.window##.onload := Html.handler onload


