structure BayesNet :> BAYESNET =
struct
  type var = string
  type cpt = real array array
  type net = { order : var list, parents : (var * var list) list, cpts : cpt list }

  fun array2 rows = Array.fromList (List.map Array.fromList rows)

  (* Classic 3-node sprinkler network (Rain -> Sprinkler, Rain -> WetGrass, Sprinkler -> WetGrass).
     CPTs give P(Rain=true | WetGrass=true) = 0.357. *)
  fun sprinkler () =
    { order   = ["Rain", "Sprinkler", "WetGrass"]
    , parents = [ ("Rain",      [])
                , ("Sprinkler", ["Rain"])
                , ("WetGrass",  ["Rain", "Sprinkler"]) ]
    , cpts    =
        [ array2 [[0.8, 0.2]]
        (* P(Rain=F)=0.8, P(Rain=T)=0.2 *)

        , array2 [[0.5, 0.5], [0.9, 0.1]]
        (* row 0: P(S|Rain=F)   = [0.5, 0.5]
           row 1: P(S|Rain=T)   = [0.9, 0.1] *)

        , array2 [ [1.0, 0.0]     (* R=F, S=F *)
                 , [0.1, 0.9]     (* R=F, S=T *)
                 , [0.0, 1.0]     (* R=T, S=F *)
                 , [0.0, 1.0] ]   (* R=T, S=T *)
        ] }

  (* --- enumeration core ----------------------------------------------- *)

  fun varIdxIn order v =
    let
      fun find [] _ = raise Fail ("Unknown variable: " ^ v)
        | find (x :: xs) i = if x = v then i else find xs (i + 1)
    in find order 0 end

  fun pow2 0 = 1 | pow2 k = 2 * pow2 (k - 1)

  (* Boolean value of variable j in assignment integer k. *)
  fun bitSet k j = (k div pow2 j) mod 2 = 1

  (* P(assignment k) for the full net via the chain rule. *)
  fun jointOfInt (net : net) k =
    let
      val { order, parents, cpts } = net
      val n = List.length order
      fun varIdx v = varIdxIn order v
      fun rowIdx vi =
        let val (_, varParents) = List.nth (parents, vi)
        in List.foldl
             (fn (p, acc) => acc * 2 + (if bitSet k (varIdx p) then 1 else 0))
             0 varParents
        end
      fun probEntry vi =
        let val cpt = List.nth (cpts, vi)
            val rIdx = rowIdx vi
            val vIdx = if bitSet k vi then 1 else 0
        in Array.sub (Array.sub (cpt, rIdx), vIdx) end
    in
      List.foldl (fn (vi, acc) => acc * probEntry vi) 1.0 (List.tabulate (n, fn i => i))
    end

  (* Does assignment k satisfy all evidence pairs? *)
  fun consistent (net : net) evidence k =
    let val { order, ... } = net
    in List.all (fn (v, b) => bitSet k (varIdxIn order v) = b) evidence end

  fun jointProb (net : net) assignment =
    let
      val { order, ... } = net
      val n = List.length order
      (* build the assignment integer; every variable must be present *)
      fun lookup v =
        case List.find (fn (v', _) => v' = v) assignment of
            SOME (_, b) => b
          | NONE => raise Fail ("jointProb: missing variable " ^ v)
      val k = List.foldl
                (fn (i, acc) =>
                   let val v = List.nth (order, i)
                   in acc + (if lookup v then pow2 i else 0) end)
                0 (List.tabulate (n, fn i => i))
    in jointOfInt net k end

  fun query (net : net) queryVar queryVal evidence =
    let
      val { order, ... } = net
      val n = List.length order
      val qIdx = varIdxIn order queryVar
      val total = pow2 n
      val num = ref 0.0
      val den = ref 0.0
      val () =
        List.app (fn k =>
          if consistent net evidence k then
            let val jp = jointOfInt net k
            in den := !den + jp;
               if bitSet k qIdx = queryVal then num := !num + jp else ()
            end
          else ()) (List.tabulate (total, fn k => k))
    in
      if !den < 1E~15 then 0.0 else !num / !den
    end

  fun marginal net queryVar queryVal = query net queryVar queryVal []

  fun mostProbable (net : net) evidence =
    let
      val { order, ... } = net
      val n = List.length order
      val total = pow2 n
      (* find the consistent assignment with the highest joint probability, and
         the total evidence mass for normalisation *)
      fun fold (k, (bestK, bestP, mass)) =
        if consistent net evidence k then
          let val jp = jointOfInt net k
              val mass' = mass + jp
              val (bk, bp) = if jp > bestP then (k, jp) else (bestK, bestP)
          in (bk, bp, mass') end
        else (bestK, bestP, mass)
      val (bestK, bestP, mass) =
        List.foldl fold (~1, ~1.0, 0.0) (List.tabulate (total, fn k => k))
      val assignment =
        List.tabulate (n, fn i => (List.nth (order, i), bitSet bestK i))
      val post = if mass < 1E~15 then 0.0 else bestP / mass
    in (assignment, post) end
end
