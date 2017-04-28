open Syntax
open LamPiTerm
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
      | PiV (x, tm1, tm2) ->
          is_type ctx tm1 && is_type (update ctx x tm1) tm2
      | _ -> false


  exception UnboundVariable

  let get_typ ctx x =
    match find ctx x with
      | Some t -> t
      | None -> raise UnboundVariable

  exception TypeError

  let (-->>) x e = LamPiTerm.subst e x

  let rec check _ tm1 tm2 : bool =
    match (abt_to_view tm1, abt_to_view tm2) with
    (* | LamV (x, tm1'), PiV (dom, y, tm2') -> failwith "Typecheck TODO" *)
    (* | AppV (fn, arg), tm2' -> failwith "Typecheck TODO" *)
    | _ -> failwith "Typecheck TODO"
  and infer ctx tm =
    match abt_to_view tm with
    | ZeroV -> Nat $$ []
    | SuccV tm' ->
        if check ctx tm' (Nat $$ [])
        then Nat $$ []
        else raise TypeError
    | LamV (x, tm) ->
        let x_typ = get_typ ctx x in
        let tm_typ = infer (update ctx x x_typ) tm in
        Pi $$ [x_typ; x ^^ tm_typ]
    | AppV (tm1, tm2) -> failwith "TODO"
    | PiV (_, _, _) -> Star $$ []
    | _ -> failwith "TODO"
  and infer_pi ctx tm = failwith "TODO"

  type result = Step of term | Val

  exception InternalError

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
  and step_pi ctx = failwith "TODO step_pi"
  and step_let ctx = failwith "TODO step_let"

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
