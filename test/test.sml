structure Tests = struct open Harness structure B = BayesNet
fun run () = let
  val net = B.sprinkler ()
  val () = section "sprinkler network: query with explicit evidence"
  (* P(Rain=true | WetGrass=true) computed from CPTs by enumeration.
     With P(Rain=T)=0.2, P(S=T|R=F)=0.5, P(S=T|R=T)=0.1,
     P(WG=T|R=T,S=F)=1.0, P(WG=T|R=T,S=T)=1.0, P(WG=T|R=F,S=T)=0.9:
     P(R=T|WG=T) = 0.2 / 0.56 = 0.357... *)
  val p = B.query net "Rain" true [("WetGrass", true)]
  val () = checkRealTol 0.05 "P(Rain|WetGrass=T)" (0.357, p)
  (* P(Rain=false | WetGrass=true) should be complement *)
  val pf = B.query net "Rain" false [("WetGrass", true)]
  val () = checkRealTol 1E~6 "P sums to 1" (1.0, p + pf)

  val () = section "marginal (no evidence)"
  (* P(Rain=T) = 0.2 directly from its CPT *)
  val () = checkRealTol 1E~9 "P(Rain=T) = 0.2" (0.2, B.marginal net "Rain" true)
  val () = checkRealTol 1E~9 "P(Rain=F) = 0.8" (0.8, B.marginal net "Rain" false)
  (* marginal == query with empty evidence *)
  val () = checkRealTol 1E~12 "marginal = query []"
             (B.query net "Sprinkler" true [], B.marginal net "Sprinkler" true)

  val () = section "jointProb of a full assignment"
  (* P(R=T, S=F, WG=T) = P(R=T) * P(S=F|R=T) * P(WG=T|R=T,S=F)
                       = 0.2  * 0.9        * 1.0 = 0.18 *)
  val jp = B.jointProb net [("Rain", true), ("Sprinkler", false), ("WetGrass", true)]
  val () = checkRealTol 1E~9 "joint R=T,S=F,WG=T = 0.18" (0.18, jp)
  (* the 8 joint probabilities sum to 1 *)
  val all8 =
    List.tabulate (8, fn k =>
      B.jointProb net [ ("Rain", (k div 4) mod 2 = 1)
                      , ("Sprinkler", (k div 2) mod 2 = 1)
                      , ("WetGrass", k mod 2 = 1) ])
  val () = checkRealTol 1E~9 "joint sums to 1" (1.0, List.foldl (op +) 0.0 all8)

  val () = section "marginal consistency: query agrees with conditional formula"
  (* P(Rain=T | WetGrass=T) should equal jointProb-derived value *)
  val pCond = B.query net "Rain" true [("WetGrass", true)]
  val () = check "0 < P(Rain|WG) < 1" (pCond > 0.0 andalso pCond < 1.0)

  val () = section "mostProbable (MAP) assignment"
  (* Given WetGrass=T, the single most likely full assignment.
     The dominant term: R=F,S=T,WG=T = 0.8*0.5*0.9 = 0.36 (largest). *)
  val (mapAssign, post) = B.mostProbable net [("WetGrass", true)]
  fun look v = case List.find (fn (v',_) => v' = v) mapAssign of
                   SOME (_, b) => b | NONE => raise Fail "missing"
  val () = checkBool "MAP Rain = F" (false, look "Rain")
  val () = checkBool "MAP Sprinkler = T" (true, look "Sprinkler")
  val () = checkBool "MAP WetGrass = T (evidence)" (true, look "WetGrass")
  val () = check "MAP posterior in (0,1]" (post > 0.0 andalso post <= 1.0)
in Harness.run () end end
