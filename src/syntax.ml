open Operator

type op = Ann | Star | Pi | App | Lam

module LamPiOp : (OPERATOR with type t = op) = struct

  type t = op

  let arity = function
    | Ann  -> [0; 0]
    | Star -> []
    | Pi   -> [0; 1]
    | App  -> [0; 0]
    | Lam  -> [1]

  let to_string = function
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

module LamPiTerm = Abt.MakeAbt(LamPiOp)
open LamPiTerm
open Variable

let tm1 =
  let x1 = newvar "x" in
  let x2 = newvar "a" in
  Ann $$ [Lam $$ [x1 ^^ (!! x1)]; Pi $$ [Star $$ []; x2 ^^ (!! x2)]]
