open Operator

type op = Ann | Star | Pi | App | Lam | Zero | Succ | Nat

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

  let to_string = function
    | Ann -> "ann"
    | Star -> "star"
    | Nat -> "nat"
    | Zero -> "zero"
    | Succ -> "succ"
    | Pi -> "pi"
    | App -> "app"
    | Lam -> "lam"

  let equal = function
    | Ann, Ann -> true
    | Star, Star -> true
    | Nat, Nat -> true
    | Zero, Zero -> true
    | Succ, Succ -> true
    | Pi, Pi -> true
    | App, App -> true
    | Lam, Lam -> true
    | _ -> false
end

module LamPiTerm = Abt.MakeAbt(LamPiOp)
open LamPiTerm
open Variable

let tm1 =
  let x1 = newvar "x" in
  let x2 = newvar "a" in
  Ann $$ [Lam $$ [x1 ^^ (!! x1)]; Pi $$ [Star $$ []; x2 ^^ (!! x2)]]

module ParseToABT = struct
  open AbsLamPi
  open Core_kernel.Core_list

  type var = LamPiTerm.Variable.t
  type context = (string * var) list

  open Assoc

  exception UnboundVariable of string

  let rec termToABT ctx tm =
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
end
