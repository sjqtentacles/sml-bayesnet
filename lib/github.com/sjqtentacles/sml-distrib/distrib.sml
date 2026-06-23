(* distrib.sml — distributions as a functor over RANDOM. *)

functor DistribFn (R : RANDOM) :> DISTRIB where type rng = R.state =
struct
  type rng = R.state
  exception Domain of string

  val pi = Math.pi

  fun checkPos name x =
    if x <= 0.0 then raise Domain (name ^ " must be positive") else ()

  fun checkProb p =
    if p < 0.0 orelse p > 1.0 then raise Domain "p must be in [0,1]" else ()

  (* ---- special functions ---- *)

  fun erf x =
    let
      val sign = if x < 0.0 then ~1.0 else 1.0
      val ax = Real.abs x
      val t = 1.0 / (1.0 + 0.3275911 * ax)
      val poly =
        ((((1.061405429 * t - 1.453152027) * t + 1.421413741) * t
          - 0.284496736) * t + 0.254829592) * t
      val y = 1.0 - poly * Math.exp (~(ax * ax))
    in sign * y end

  local
    val g = 7.0
    val c =
      [ 0.99999999999980993, 676.5203681218851, ~1259.1392167224028,
        771.32342877765313, ~176.61502916214059, 12.507343278686905,
        ~0.13857109526572012, 9.9843695780195716E~6, 1.5056327351493116E~7 ]
  in
    fun gammaln z0 =
      let
        val z = z0 - 1.0
        val (a, _) =
          List.foldl (fn (ci, (acc, i)) =>
            if i = 0 then (ci, 1) else (acc + ci / (z + real i), i + 1))
            (0.0, 0) c
        val tt = z + g + 0.5
      in 0.5 * Math.ln (2.0 * pi) + (z + 0.5) * Math.ln tt - tt + Math.ln a end
  end

  fun betacf (a, b, x) =
    let
      val maxIt = 200
      val epsCf = 3.0E~12
      val fpmin = 1.0E~300
      val qab = a + b
      val qap = a + 1.0
      val qam = a - 1.0
      fun guard d = if Real.abs d < fpmin then fpmin else d
      fun loop (m, c, d, h) =
        if m > maxIt then h
        else
          let
            val rm = real m
            val aa1 = rm * (b - rm) * x / ((qam + 2.0 * rm) * (a + 2.0 * rm))
            val d1 = guard (1.0 + aa1 * d)
            val c1 = guard (1.0 + aa1 / c)
            val d1' = 1.0 / d1
            val h1 = h * d1' * c1
            val aa2 =
              ~((a + rm) * (qab + rm) * x / ((a + 2.0 * rm) * (qap + 2.0 * rm)))
            val d2 = guard (1.0 + aa2 * d1')
            val c2 = guard (1.0 + aa2 / c1)
            val del = (1.0 / d2) * c2
            val h2 = h1 * del
          in if Real.abs (del - 1.0) < epsCf then h2 else loop (m + 1, c2, 1.0 / d2, h2) end
      val d0 = guard (1.0 - qab * x / qap)
    in loop (1, 1.0, 1.0 / d0, 1.0 / d0) end

  fun betai (a, b, x) =
    if x <= 0.0 then 0.0
    else if x >= 1.0 then 1.0
    else
      let
        val bt = Math.exp (gammaln (a + b) - gammaln a - gammaln b
                         + a * Math.ln x + b * Math.ln (1.0 - x))
      in
        if x < (a + 1.0) / (a + b + 2.0)
        then bt * betacf (a, b, x) / a
        else 1.0 - bt * betacf (b, a, 1.0 - x) / b
      end

  (* Regularised lower incomplete gamma P(a,x) = gamma(a,x)/Gamma(a), via the
     series representation for x < a+1 and the Lentz continued fraction for the
     complement Q(a,x) when x >= a+1 (Numerical Recipes "gammp"). *)
  fun gammainc (a, x) =
    if x <= 0.0 then 0.0
    else
      let
        val gln = gammaln a
        val fpmin = 1.0E~300
        val tol = 1.0E~15
      in
        if x < a + 1.0 then
          (* power series: P = e^{-x} x^a / Gamma(a) * sum_{n>=0} x^n / (a(a+1)..(a+n)) *)
          let
            fun loop (sum, term, ap, n) =
              if n > 1000 then sum
              else
                let val ap'   = ap + 1.0
                    val term' = term * x / ap'
                    val sum'  = sum + term'
                in if Real.abs term' < Real.abs sum' * tol then sum'
                   else loop (sum', term', ap', n + 1) end
            val s0 = 1.0 / a
            val s  = loop (s0, s0, a, 0)
          in s * Math.exp (~x + a * Math.ln x - gln) end
        else
          (* continued fraction for Q(a,x); P = 1 - Q *)
          let
            fun guard d = if Real.abs d < fpmin then fpmin else d
            fun loop (b, c, d, h, i) =
              if i > 1000 then h
              else
                let val an   = ~(real i) * (real i - a)
                    val b'   = b + 2.0
                    val d'   = 1.0 / guard (an * d + b')
                    val c'   = guard (b' + an / c)
                    val del  = d' * c'
                    val h'   = h * del
                in if Real.abs (del - 1.0) < tol then h' else loop (b', c', d', h', i + 1) end
            val b0 = x + 1.0 - a
            val d0 = 1.0 / b0
            val q  = Math.exp (~x + a * Math.ln x - gln) * loop (b0, 1.0 / fpmin, d0, d0, 1)
          in 1.0 - q end
      end

  fun gammaCdf k scale x =
    if x <= 0.0 then 0.0
    else gammainc (k, x / scale)

  structure Normal =
  struct
    type param = { mu : real, sigma : real }
    fun pdf { mu, sigma } x =
      let val z = (x - mu) / sigma
      in Math.exp (~0.5 * z * z) / (sigma * Math.sqrt (2.0 * pi)) end
    fun cdf { mu, sigma } x =
      0.5 * (1.0 + erf ((x - mu) / (sigma * Math.sqrt 2.0)))
    fun sample { mu, sigma } s =
      let
        val (u1, s1) = R.real01 s
        val (u2, s2) = R.real01 s1
        val u1' = if u1 <= 0.0 then 1.0E~300 else u1
        val z = Math.sqrt (~2.0 * Math.ln u1') * Math.cos (2.0 * pi * u2)
      in (mu + sigma * z, s2) end
  end

  structure Gamma =
  struct
    type param = { shape : real, scale : real }
    fun pdf { shape, scale } x =
      if x <= 0.0 then 0.0
      else
        Math.exp ((shape - 1.0) * Math.ln x - x / scale - gammaln shape
                  - shape * Math.ln scale)
    fun cdf { shape, scale } x = gammaCdf shape scale x
    (* Marsaglia-Tsang for shape >= 1; for shape < 1 use the standard boost
       g(shape) = g(shape+1) * U^(1/shape) (Marsaglia-Tsang sec. 6) so the
       method stays valid (d = shape - 1/3 would otherwise be negative). *)
    fun sample { shape, scale } s =
      if shape < 1.0 then
        let
          val (g, s1) = sample { shape = shape + 1.0, scale = 1.0 } s
          val (u, s2) = R.real01 s1
          val u' = if u <= 0.0 then 1.0E~300 else u
        in (g * Math.pow (u', 1.0 / shape) * scale, s2) end
      else
      let
        val d = shape - 1.0 / 3.0
        val c = 1.0 / Math.sqrt (9.0 * d)
        fun draw s =
          let
            val (u1, s1) = R.real01 s
            val (u2, s2) = R.real01 s1
            val z = Math.sqrt (~2.0 * Math.ln (if u1 <= 0.0 then 1.0E~300 else u1))
                      * Math.cos (2.0 * pi * u2)
            val v = 1.0 + c * z
          in
            if v <= 0.0 then draw s2 else (d * v * v * v, s2)
          end
        val (g, s') = draw s
      in (g * scale, s') end
  end

  structure Beta =
  struct
    type param = { alpha : real, beta : real }
    fun pdf { alpha, beta } x =
      if x <= 0.0 orelse x >= 1.0 then 0.0
      else Math.exp ((alpha - 1.0) * Math.ln x + (beta - 1.0) * Math.ln (1.0 - x)
                     - gammaln alpha - gammaln beta + gammaln (alpha + beta))
    fun cdf { alpha, beta } x = betai (alpha, beta, x)
    fun sample { alpha, beta } s =
      let
        val (x, s1) = Gamma.sample { shape = alpha, scale = 1.0 } s
        val (y, s2) = Gamma.sample { shape = beta, scale = 1.0 } s1
      in (x / (x + y), s2) end
  end

  structure Binomial =
  struct
    type param = { n : int, p : real }
    fun choose (n, k) =
      if k < 0 orelse k > n then 0.0
      else
        let val k = Int.min (k, n - k)
            fun loop (i, acc) = if i > k then acc else loop (i + 1, acc * real (n - k + i) / real i)
        in loop (1, 1.0) end
    fun pdf { n, p } k =
      if k < 0 orelse k > n then 0.0
      else choose (n, k) * Math.pow (p, real k) * Math.pow (1.0 - p, real (n - k))
    fun cdf (par as { n, ... }) k =
      if k < 0 then 0.0
      else List.foldl (fn (i, acc) => acc + pdf par i) 0.0 (List.tabulate (Int.min (k, n) + 1, fn i => i))
    fun sample { n, p } s =
      let fun loop (0, acc, s) = (acc, s)
            | loop (i, acc, s) =
                let val (u, s') = R.real01 s in loop (i - 1, if u < p then acc + 1 else acc, s') end
      in loop (n, 0, s) end
  end

  structure Poisson =
  struct
    type param = { lambda : real }
    fun pdf { lambda } k =
      if k < 0 then 0.0
      else Math.exp (~lambda + real k * Math.ln lambda - gammaln (real k + 1.0))
    fun cdf (par as { lambda }) k =
      if k < 0 then 0.0
      else List.foldl (fn (i, acc) => acc + pdf par i) 0.0 (List.tabulate (k + 1, fn i => i))
    fun sample { lambda } s =
      let
        val lcap = Math.exp (~lambda)
        fun loop (k, p, s) =
          let val (u, s') = R.real01 s val p' = p * u
          in if p' <= lcap then (k, s') else loop (k + 1, p', s') end
      in loop (0, 1.0, s) end
  end
end

structure Distrib = DistribFn (SplitMix64)
