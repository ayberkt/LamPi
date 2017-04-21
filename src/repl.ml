open Syntax

open Core_kernel.Core_printf

let _ =
  printf "%s\n" (LamPiTerm.to_string tm1)
