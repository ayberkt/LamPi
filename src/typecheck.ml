open Syntax
open LamPiView
module CL = Core_kernel.Core_list

module Typechecker = struct

  type term    = LamPiTerm.t
  type var     = Syntax.LamPiTerm.Variable.t
  type context = (var, term) CL.Assoc.t
  open CL.Assoc

  let update : context -> var -> term -> context =
    fun ctx ->
      add ~equal:(fun x y -> Syntax.LamPiTerm.Variable.equal (x, y)) ctx

  type decl = string * term

  let rec is_type (ctx : context) tm =
      match abt_to_view tm with
      | NatV  -> true
      | StarV -> true
      | PiV (tm1, x, tm2) ->
          is_type ctx tm1 && is_type (update ctx x tm1) tm2
      | _ -> false

  let check ctx tm1 tm2 =
    match (abt_to_view tm1, abt_to_view tm2) with
    | LamV (x, tm1'), PiV (dom, y, tm2') -> failwith "Typecheck TODO"
    | AppV (fn, arg), tm2' -> failwith "Typecheck TODO"
    | _ -> failwith "Typecheck TODO"

    (* | NatV
| ZeroV
| StarV
| AnnV of LamPiTerm.t * LamPiTerm.t
| SuccV of LamPiTerm.t
| PiV of LamPiTerm.t * Variable.t * LamPiTerm.t
| AppV of LamPiTerm.t * LamPiTerm.t
| LamV of Variable.t * LamPiTerm.t
| LetV of Variable.t * LamPiTerm.t * LamPiTerm.t
 *)

  let check : context -> term -> bool = failwith "todo"
  let infer : context -> term -> term = failwith "todo"
end
