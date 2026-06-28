signature BAYESNET =
sig
  type var = string
  type cpt = real array array
  type net = { order : var list, parents : (var * var list) list, cpts : cpt list }

  (* The classic 3-node sprinkler network fixture. *)
  val sprinkler : unit -> net

  (* The joint probability of a *complete* assignment (every variable in the
     net assigned a boolean), by the chain rule over the CPTs. The assignment
     may be given in any order; raises Fail if a variable is missing or unknown. *)
  val jointProb : net -> (var * bool) list -> real

  (* The marginal probability P(queryVar = queryVal) (no evidence). *)
  val marginal : net -> var -> bool -> real

  (* Conditional probability P(queryVar = queryVal | evidence) by exact
     enumeration, where `evidence` is an arbitrary list of (var, bool)
     observations. With an empty evidence list this equals `marginal`.

     NOTE: this is a BREAKING change from the previous
     `query : net -> var -> bool -> real`, which hardcoded the evidence to
     "the last variable is true". Pass that explicitly now, e.g.
     `query net "Rain" true [("WetGrass", true)]`. *)
  val query : net -> var -> bool -> (var * bool) list -> real

  (* The most probable joint assignment of all variables given the evidence
     (the MAP assignment), returned in `order`, together with its posterior
     probability. *)
  val mostProbable : net -> (var * bool) list -> (var * bool) list * real
end
