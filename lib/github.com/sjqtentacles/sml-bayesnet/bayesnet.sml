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

  (* Exact inference by full enumeration of all 2^n variable assignments.
     Queries P(queryVar = queryVal | last-variable = true). *)
  fun query (net : net) queryVar queryVal =
    let
      val { order, parents, cpts } = net
      val n = List.length order

      fun varIdx v =
        let
          fun find [] _ = raise Fail ("Unknown variable: " ^ v)
            | find (x :: xs) i = if x = v then i else find xs (i + 1)
        in find order 0 end

      val qIdx   = varIdx queryVar
      val evidIdx = n - 1   (* evidence = last variable, assumed true *)

      (* 2^j, tail-recursive *)
      fun pow2 0 = 1 | pow2 k = 2 * pow2 (k - 1)

      (* Boolean value of variable j in assignment integer k *)
      fun bitSet k j = (k div pow2 j) mod 2 = 1

      (* CPT row index for variable vi given assignment k *)
      fun rowIdx vi k =
        let val (_, varParents) = List.nth (parents, vi)
        in List.foldl
             (fn (p, acc) => acc * 2 + (if bitSet k (varIdx p) then 1 else 0))
             0 varParents
        end

      (* P(vi = its value in k | parents) *)
      fun probEntry vi k =
        let
          val cpt  = List.nth (cpts, vi)
          val rIdx = rowIdx vi k
          val vIdx = if bitSet k vi then 1 else 0
        in Array.sub (Array.sub (cpt, rIdx), vIdx) end

      (* Joint P(assignment k) *)
      fun jointProb k =
        List.foldl (fn (vi, acc) => acc * probEntry vi k) 1.0
          (List.tabulate (n, fn i => i))

      val total = pow2 n

      val num = ref 0.0
      val den = ref 0.0
      val () =
        List.app (fn k =>
          if bitSet k evidIdx then
            let val jp = jointProb k
            in den := !den + jp;
               if bitSet k qIdx = queryVal then num := !num + jp else ()
            end
          else ()) (List.tabulate (total, fn k => k))
    in
      if !den < 1E~15 then 0.0 else !num / !den
    end
end
