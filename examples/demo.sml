(* demo.sml - exact inference on the classic 3-node sprinkler Bayesian
   network (Rain -> Sprinkler, Rain -> WetGrass, Sprinkler -> WetGrass) via
   BayesNet's enumeration-based query engine. Deterministic: no wall-clock,
   no unseeded randomness. *)

structure BN = BayesNet

(* Real.toString would print "1E~2"-style / platform-varying output; fix the
   digit count and normalize negative zero so MLton and Poly/ML agree. *)
fun fmtR r =
  let val r = if Real.== (r, 0.0) then 0.0 else r
  in Real.fmt (StringCvt.FIX (SOME 4)) r end

val net = BN.sprinkler ()

val () = print "Sprinkler network: Rain -> Sprinkler, Rain -> WetGrass, Sprinkler -> WetGrass\n\n"

val jp = BN.jointProb net [("Rain", true), ("Sprinkler", false), ("WetGrass", true)]
val () = print ("P(Rain=T, Sprinkler=F, WetGrass=T) = " ^ fmtR jp ^ "\n")

val mR = BN.marginal net "Rain" true
val () = print ("P(Rain=T)                          = " ^ fmtR mR ^ "\n")

val qR = BN.query net "Rain" true [("WetGrass", true)]
val () = print ("P(Rain=T | WetGrass=T)             = " ^ fmtR qR ^ "\n")

val (assign, post) = BN.mostProbable net [("WetGrass", true)]
val () = print "\nMAP assignment given WetGrass=T:\n"
val () =
  List.app
    (fn (v, b) => print ("  " ^ v ^ " = " ^ (if b then "true" else "false") ^ "\n"))
    assign
val () = print ("  posterior probability             = " ^ fmtR post ^ "\n")
