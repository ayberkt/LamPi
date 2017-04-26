open Operator
module CL = Core_kernel.Core_list

type op = Ann | Star | Pi | App | Lam | Zero | Succ | Nat | Let

module LamPiOp : (OPERATOR with type t = op) = struct

  type t = op

  let arity = function
    | Ann  -> [0; 0]
    | Star -> []
    | Nat  -> []
    | Zero -> []
    | Succ -> [0]
    | Pi   -> [0; 1]
    | App  -> [0; 0]
    | Lam  -> [1]
    | Let  -> [0; 1]

  let to_string = function
    | Ann -> "ann"
    | Star -> "star"
    | Nat -> "nat"
    | Zero -> "zero"
    | Succ -> "succ"
    | Pi -> "pi"
    | App -> "app"
    | Lam -> "lam"
    | Let -> "let"

  let equal = function
    | Ann, Ann -> true
    | Star, Star -> true
    | Pi, Pi -> true
    | Lam, Lam -> true
    | App, App -> true
    | Nat, Nat -> true
    | Zero, Zero -> true
    | Succ, Succ -> true
    | Let, Let -> true
    | _, _ -> false
end

module LamPiTerm = Abt.MakeAbt(LamPiOp)

(* let tm1 = *)
  (* let x1 = newvar "x" in *)
  (* let x2 = newvar "a" in *)
  (* Ann $$ [Lam $$ [x1 ^^ (!! x1)]; Pi $$ [Star $$ []; x2 ^^ (!! x2)]] *)

module ParseToABT = struct
  open LamPiTerm
  open Variable
  open AbsLamPi

  type var = LamPiTerm.Variable.t
  type context = (string * var) list

  open CL.Assoc

  exception UnboundVariable of string

  let rec termToABT ctx tm : LamPiTerm.t =
    match tm with
    | TmId (Ident name) ->
        begin match find ctx name with
        | Some x -> !! x
        | None -> raise (UnboundVariable name)
        end
    | TmZero -> Zero $$ []
    | TmSet -> Star $$ []
    | TmNat -> Nat $$ []
    | TmSucc tm -> Succ $$ [termToABT ctx tm]
    | TmApp (tm1, tm2) -> App $$ [termToABT ctx tm1; termToABT ctx tm2]
    | TmAbs (Ident x, tm) ->
        let x1 = newvar x in
        let ctx' = add ctx x x1 in
        Lam $$ [x1 ^^ termToABT ctx' tm]
    | TmArrow (tm1, tm2) ->
        let x1 = newvar "_" in
        Pi $$ [termToABT ctx tm1; x1 ^^ termToABT ctx tm2]
    | TmForall (Ident x, tm1, tm2) ->
        let x1 = newvar x in
        let ctx' = add ctx x x1 in
        Pi $$ [termToABT ctx tm1; x1 ^^ termToABT ctx' tm2]
    | TmAnn (tm1, tm2) ->
        Ann $$ [termToABT ctx tm1; termToABT ctx tm2]

    let parse_program : program -> LamPiTerm.t list =
      fun (PDecls decls) ->
        let declToABT (TmDecl (_, tm1, tm2)) =
          termToABT [] (TmAnn (tm2, tm1)) in
        CL.map ~f:declToABT decls
end

module LamPiView = struct
  open LamPiTerm

  type term_view =
    | NatV
    | ZeroV
    | AnnV of LamPiTerm.t * LamPiTerm.t
    | SuccV of LamPiTerm.t
    | PiV of LamPiTerm.t * Variable.t * LamPiTerm.t
    | AppV of LamPiTerm.t * LamPiTerm.t
    | LamV of Variable.t * LamPiTerm.t
    | LetV of Variable.t * LamPiTerm.t * LamPiTerm.t

  exception CannotConvert

  let app_to_view (op, tms) =
    match (op, tms) with
    | Ann, [tm1; tm2] ->
        AnnV (tm1, tm2)
    | Nat, [] -> NatV
    | Zero, [] -> ZeroV
    | Succ, [tm] -> SuccV tm
    | Pi, [tm1; tm2'] ->
        begin match out tm2' with
        | AbsView (x, tm2) -> PiV (tm1, x, tm2)
        | _ -> raise CannotConvert
        end
    | App, [tm1; tm2] -> AppV (tm1, tm2)
    | Lam, [tm] ->
        begin match out tm with
        | VarView x -> LamV (x, tm)
        | _ -> raise CannotConvert
        end
    | Let, xs -> failwith "TODO"
    | _, _ -> raise CannotConvert

  let abt_to_view (a : LamPiTerm.t) =
    match out a with
    | AppView (op, tms) -> app_to_view (op, tms)
    | _ -> raise CannotConvert
end
