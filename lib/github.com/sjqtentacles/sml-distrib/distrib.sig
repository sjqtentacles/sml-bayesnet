(* distrib.sig

   Probability distributions with pdf, cdf, and pure sampling via sml-prng.

   All sampling is referentially transparent: `sample` threads generator state.
   The default `Distrib` structure uses SplitMix64. *)

signature DISTRIB =
sig
  type rng

  exception Domain of string

  structure Normal :
  sig
    type param = { mu : real, sigma : real }
    val pdf    : param -> real -> real
    val cdf    : param -> real -> real
    val sample : param -> rng -> real * rng
  end

  structure Gamma :
  sig
    type param = { shape : real, scale : real }
    val pdf    : param -> real -> real
    val cdf    : param -> real -> real
    val sample : param -> rng -> real * rng
  end

  structure Beta :
  sig
    type param = { alpha : real, beta : real }
    val pdf    : param -> real -> real
    val cdf    : param -> real -> real
    val sample : param -> rng -> real * rng
  end

  structure Poisson :
  sig
    type param = { lambda : real }
    val pdf    : param -> int -> real
    val cdf    : param -> int -> real
    val sample : param -> rng -> int * rng
  end

  structure Binomial :
  sig
    type param = { n : int, p : real }
    val pdf    : param -> int -> real
    val cdf    : param -> int -> real
    val sample : param -> rng -> int * rng
  end
end
