structure Tests = struct open Harness structure B = BayesNet
fun run () = let
  val net = B.sprinkler ()
  val () = section "sprinkler network"
  (* P(Rain=true | WetGrass=true) computed from CPTs by enumeration.
     With P(Rain=T)=0.2, P(S=T|R=F)=0.5, P(S=T|R=T)=0.1,
     P(WG=T|R=T,S=F)=1.0, P(WG=T|R=T,S=T)=1.0, P(WG=T|R=F,S=T)=0.9:
     P(R=T|WG=T) = 0.2 / 0.56 = 0.357... *)
  val p = B.query net "Rain" true
  val () = checkRealTol 0.05 "P(Rain|WetGrass=T)" (0.357, p)
  (* P(Rain=false | WetGrass=true) should be complement *)
  val pf = B.query net "Rain" false
  val () = checkRealTol 1E~6 "P sums to 1" (1.0, p + pf)
in Harness.run () end end
