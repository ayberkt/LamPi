open Syntax
open AbtUtil

type lam_pi_term = Syntax.AbtUtil.t

let star_abt : lam_pi_term = AbtUtil.intoApp Star []

let abt1 : lam_pi_term =
  AbtUtil.intoApp App [star_abt; star_abt];;

let main =
  Printf.printf "%s\n" (AbtUtil.toString abt1);
