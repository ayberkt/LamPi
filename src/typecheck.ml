open Syntax

type term    = LamPiTerm.t
type context = string * term list

type decl = string * term

let is_type : context -> term -> bool = failwith "todo"

let check_decl : context -> decl -> context =
  fun ctx dcl -> failwith "foo"

let check : context -> term -> bool = failwith "todo"

let infer : context -> term -> term = failwith "todo"
