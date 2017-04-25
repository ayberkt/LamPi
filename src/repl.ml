open Syntax
open ParseToABT

open Core_kernel.Core_printf

open Printf
open Lexing
open Sys
open Core_kernel.Std
open Core_kernel.Core_list
open ParLamPi
open LexLamPi
open AbsLamPi

let printf = Core_kernel.Core_printf.printf;;

let (color_reset, color_red, color_green, color_pink, color_bright) =
  ("\x1b[0m",
   "\x1b[1m\x1b[31m",
   "\x1b[1m\x1b[32m",
   "\x1B[35m",
   "\x1b[1m\x1b[37m");;
let red s =  color_red ^ s ^ color_reset;;
let green s =  color_green ^ s ^ color_reset;;
let pink s = color_pink ^ s ^ color_reset;;

  let error_msg (x : Lexing.position) (y : Lexing.position) : string =
    (red "Syntax error") ^ " at line " ^ (string_of_int x.pos_lnum) ^ ".\n"


  let parse_line s = ParLamPi.pProgram LexLamPi.token (from_string s)

  let quit_repl () : unit = printf "\nTake care!\n"; exit 0

  let print_error_description x =
    (printf "\n%s (in the definition of %s):\n    " (red "Type Error")) x

  let repl () =
    printf "%s" "\n           λΠ v0.0.0           \n\n";
    while true do
      printf "- ";
      let input = Pervasives.read_line () in
      if String.equal input "quit"
      then quit_repl ()
      else
        let (PDecls [TmDecl (x, tm1, tm2)]) = parse_line input in
        let abt = termToABT [] (TmAnn (tm1, tm2)) in
        printf "--> %s\n" (LamPiTerm.to_string abt)
        (* printf "\n%s\n" "-------   CLAUSES   -------"; *)
        (* let print_clauses i x = *)
          (* printf "(C%d) %s       " i (CNF.CNF.show_clause x) in *)
        (* CL.iteri ~f:print_clauses cnf; printf "\n\n"; *)
        (* let refuted = cnf |> refute in *)
        (* if refuted then printf "Refuted!\n" else printf "Could not refute\n" *)
    done

  let main =
    set_signal sigint (Signal_handle (fun _ -> quit_repl ()));
    repl ()
  ;;
