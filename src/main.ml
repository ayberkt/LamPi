open Syntax
open ParseToABT
open Typecheck

open Core_kernel.Core_printf

(* open Printf *)
open Lexing
open Sys
(* open Core_kernel.Std *)
open Core_kernel.Core_list
(* open ParLamPi *)
(* open LexLamPi *)
(* open AbsLamPi *)

let (color_reset, color_red, color_green, color_pink, color_bright) =
  ("\x1b[0m",
   "\x1b[1m\x1b[31m",
   "\x1b[1m\x1b[32m",
   "\x1B[35m",
   "\x1b[1m\x1b[37m");;
let red s =  color_red ^ s ^ color_reset;;
let green s =  color_green ^ s ^ color_reset;;
let pink s = color_pink ^ s ^ color_reset;;

let error_msg (x : Lexing.position) (_ : Lexing.position) : string =
  (red "Syntax error") ^ " at line " ^ (string_of_int x.pos_lnum) ^ ".\n"

let parse_line s = ParLamPi.pProgram LexLamPi.token (from_string s)

let parse_file c = ParLamPi.pProgram LexLamPi.token (from_channel c)

let quit_repl () : unit = printf "\nTake care!\n"; exit 0

let print_error_description x =
  (printf "\n%s (in the definition of %s):\n    " (red "Type Error")) x

let print_ABT a = printf "--> %s\n" (LamPiTerm.to_string a)

let repl () =
  printf "%s" "\n           λΠ v0.0.0           \n\n";
  while true do
    printf "- ";
    let input = Pervasives.read_line () in
    if String.equal input "quit"
    then quit_repl ()
    else
      let abts = parse_program (parse_line input) in
      iter ~f:print_ABT abts
  done

type mode = REPL | RunFile of string

let check_term tm : unit =
    let open Typechecker in
    let showtm = LamPiTerm.to_string in
    try
      let ty = infer [] tm in
      Printf.printf "%s has type %s.\n" (showtm tm) (showtm ty)
    with
    | TypeError s -> Printf.printf "%s\n" s

let run = function
| REPL -> repl ()
| RunFile file_name ->
    let file = open_in file_name in
    let abts =
      try parse_program (parse_file file)
      with
      | ParseToABT.UnboundVariable x ->
          printf "%sError:%s Unbound value %s\n" color_red color_reset x;
          exit 1
      | BNFC_Util.Parse_error (x, y) ->
          printf "Parse error between line %d char %d to line %d char %d\n"
          x.pos_lnum x.pos_cnum x.pos_lnum y.pos_cnum;
          exit 1
    in
      printf "Parsing the file %s...\n\n" file_name;
      iter ~f:check_term abts
;;

let main =
  set_signal sigint (Signal_handle (fun _ -> quit_repl ()));
  if Array.length Sys.argv == 1
  then run REPL
  else run (RunFile (Array.get Sys.argv 1))
;;
