#require "abt"
open Operator

type op = Ann | Star | Pi | App | Lam

module LamPiOp : (OPERATOR with type t = op) = struct

  type t = op

  let arity = function
    | Ann  -> [0; 0]
    | Star -> []
    | Pi   -> [1]
    | App  -> [0; 0]
    | Lam  -> [1]

  let toString = function
    | Ann -> "ann"
    | Star -> "star"
    | Pi -> "pi"
    | App -> "app"
    | Lam -> "lam"

  let equal = function
    | Ann, Ann -> true
    | Star, Star -> true
    | Pi, Pi -> true
    | App, App -> true
    | Lam, Lam -> true
    | _ -> false
end

module Abt = Abt.MakeAbt(LamPiOp)
module AbtUtil = Abt_util.MakeABTUtil(Abt)

module LamPiSyntax = struct
  open AbtUtil

  type var = AbtUtil.Variable.t

  type term_view =
    | AnnV of t view * t view
    | StarV
    | PiV of var * t view
    | AppV of t  view * t view
    | LamV of var * t view
    | VarV of var

  let term_to_view : term_view -> AbtUtil.t view = function
    | AnnV (tm1, tm2) -> AppView (Ann, [into tm1; into tm2])
    | StarV -> AppView (Star, [])
    | PiV (x, tm) -> AppView (Pi, [into (AbsView (x, into tm))])
    | AppV (tm1, tm2) -> AppView (App, [into tm1; into tm2])
    | LamV (x, tm) -> AppView (Lam, [into (AbsView (x, into tm))])
    | VarV x -> VarView x


  (* let to_term_view t = function
    | AppView (f, [tm1; tm2]) when f == Ann -> AnnV (tm1, tm2)
    | AppView (f, []) when f == Star -> StarV
    | AppView (f, [tm1]) when f == Pi ->
        begin match out tm1 with
        | AbsView (x, tm2) -> PiV (x, tm2)
        | _ -> raise Malformed
        end
    | AppView (f, [tm1; tm2]) when f == App -> AppV (tm1, tm2)
    | AppView (f, [tm1]) when f == Lam ->
        begin match out tm1 with
        | AbsView (x, tm2) -> LamV (x, tm2)
        | _ -> raise Malformed
        end
    | _ -> failwith "todo"
 *)
  let phi = term_to_view StarV
  let theta =
    let foo =
      let v = Variable.newvar "f" in (LamV (v, term_to_view (VarV v)))
    in
      term_to_view foo

end
