open Syntax
open LamPiTerm
open LamPiView
module CL = Core_kernel.Core_list
open Core_kernel.Core_printf

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
      | PiV (x, tm1, tm2) ->
          is_type ctx tm1 && is_type (update ctx x tm1) tm2
      | _ -> false


  exception UnboundVariable
  exception InternalError

  let get_typ ctx x =
    match find ctx x with
      | Some t -> t
      | None -> raise UnboundVariable

  exception TypeError of string

  let errty s = raise (TypeError s)

  let (-->>) x e = LamPiTerm.subst e x

  let (<==>) tm1 tm2 = aequiv (tm1, tm2)

  type result = Step of term | Val

  let rec step (ctx : context) tmvw : result =
    match abt_to_view tmvw with
    | AnnV (_, _) -> failwith "Annotation should not occcur in step."
    | NatV  -> Val
    | StarV -> Val
    | LamV (_, _) -> Val
    | PiV (_, _, _)     as tm -> step_pi ctx tm
    | LetV (_, _, _)    as tm -> step_let ctx tm
    | (ZeroV | SuccV _) as tm -> step_nat ctx tm
    | AppV (_, _)       as tm -> step_app ctx tm
  and step_nat ctx =
    function
    | ZeroV -> Val
    | SuccV tm ->
        begin match step ctx tm with
        | Val -> Val
        | Step tm' -> Step (Succ $$ [tm'])
        end
    | _ -> raise InternalError
  and step_app ctx =
    function
    | AppV (tm1, tm2) ->
        begin match step ctx tm2 with
        | Val ->
            begin match step ctx tm1 with
            | Val ->
                begin match abt_to_view tm1 with
                | LamV (x, body) -> Step ((x -->> tm2) body)
                | _ -> raise InternalError
                end
            | Step tm1' -> Step (App $$ [tm1'; tm2])
            end
        | Step tm2' -> Step (App $$ [tm1; tm2'])
        end
    | _ -> raise InternalError
  and step_pi ctx tm =
    match tm with
    | PiV (x, tm1, tm2) ->
        begin match step ctx tm1 with
        | Step tm1' -> Step (Pi $$ [tm1'; x ^^ tm2])
        | Val ->
            begin match step (update ctx x tm1) tm2 with
            | Step tm2' -> Step (Pi $$ [tm1; x ^^ tm2'])
            | Val -> Val
            end
        end
    | _ -> raise InternalError
  and step_let _ = failwith "TODO step_let"

  let rec normalize ctx tm =
    match step ctx tm with
    | Val -> tm
    | Step tm' -> normalize ctx tm'

  let rec check ctx tm1 tm2 : bool =
    match (abt_to_view tm1, abt_to_view tm2) with
    | ZeroV, NatV -> true
    | SuccV tm, NatV -> check ctx tm (Nat $$ [])
    | _ -> failwith "Check TODO"
  and infer ctx tm =
    match abt_to_view tm with
    | ZeroV -> Nat $$ []
    | SuccV tm' when check ctx tm' (Nat $$ []) -> Nat $$ []
    | SuccV tm' -> errty (sprintf "%s is expected to be a nat"  (to_string tm'))
    | LamV (x, tm) ->
        let xty = get_typ ctx x in
        let tmty = infer (update ctx x xty) tm in
        Pi $$ [xty; x ^^ tmty]
    | AppV (tm1, tm2) ->
        let ty1 = infer_pi ctx tm1 in
          begin match abt_to_view ty1 with
          | PiV (x, s, t) when check ctx tm2 s -> (x -->> tm2) t
          | PiV (_, s, t) ->
              errty (sprintf "%s and %s not equal" (to_string s) (to_string t))
          | _ -> errty "TODO: find message"
          end
    | (PiV(_, _, _) | NatV | StarV) -> Star $$ []
    | AnnV (tm, ty) when check ctx tm ty -> ty
    | AnnV (tm, ty) ->
        errty (sprintf "%s does not have type %s" (to_string tm) (to_string ty))
    | LetV (x, tm1, tm2) -> infer (update ctx x tm1) tm2
  and infer_pi ctx tm =
    match abt_to_view (normalize ctx tm) with
    | PiV (x, tm1, tm2) -> Pi $$ [tm1; x ^^ tm2]
    | _ -> raise InternalError

end

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
