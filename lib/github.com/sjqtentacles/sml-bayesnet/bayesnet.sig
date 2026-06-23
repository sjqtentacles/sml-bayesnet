signature BAYESNET =
sig
  type var = string
  type cpt = real array array
  type net = { order : var list, parents : (var * var list) list, cpts : cpt list }
  val sprinkler : unit -> net
  val query : net -> var -> bool -> real
end
