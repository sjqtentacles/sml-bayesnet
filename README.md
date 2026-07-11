# sml-bayesnet

[![CI](https://github.com/sjqtentacles/sml-bayesnet/actions/workflows/ci.yml/badge.svg)](https://github.com/sjqtentacles/sml-bayesnet/actions/workflows/ci.yml)

Discrete Bayesian network inference for Standard ML. Builds networks of binary
random variables with Conditional Probability Tables (CPTs) and performs exact
inference via full enumeration: arbitrary-evidence conditional queries,
marginals, joint probabilities, and the MAP (most-probable) assignment.

## API sketch

```sml
(* Use the built-in sprinkler network *)
val net : BayesNet.net = BayesNet.sprinkler ()

(* Conditional query with arbitrary evidence: P(Rain=true | WetGrass=true) *)
val p : real = BayesNet.query net "Rain" true [("WetGrass", true)]
(* p ≈ 0.357 *)

(* Marginal (no evidence): P(Rain = true) *)
val m : real = BayesNet.marginal net "Rain" true        (* 0.2 *)

(* Joint probability of a complete assignment *)
val j : real =
  BayesNet.jointProb net [("Rain",true),("Sprinkler",false),("WetGrass",true)]
(* 0.18 *)

(* MAP: the single most-probable full assignment given evidence *)
val (assign, post) = BayesNet.mostProbable net [("WetGrass", true)]
(* assign = [("Rain",false),("Sprinkler",true),("WetGrass",true)] *)
```

> **Breaking change:** `query` now takes an explicit evidence list as its final
> argument — `query : net -> var -> bool -> (var * bool) list -> real`. The
> previous form hardcoded the evidence to "the last variable is true"; to
> reproduce it, pass `[(lastVar, true)]` (or `[]` for the unconditioned
> marginal, which is also available as `marginal`).

## Sprinkler network structure

```
Rain  →  Sprinkler
  ↘          ↘
       WetGrass
```

| P(Rain=T) | P(S=T\|R=F) | P(S=T\|R=T) | P(WG=T\|R=F,S=F) | ... |
|---|---|---|---|---|
| 0.2 | 0.5 | 0.1 | 0.0 | ... |

## Example

`make example` builds and runs [`examples/demo.sml`](examples/demo.sml), which
runs the joint/marginal/conditional/MAP queries above against the sprinkler
network and prints their results (output is byte-identical under MLton and
Poly/ML):

```
Sprinkler network: Rain -> Sprinkler, Rain -> WetGrass, Sprinkler -> WetGrass

P(Rain=T, Sprinkler=F, WetGrass=T) = 0.1800
P(Rain=T)                          = 0.2000
P(Rain=T | WetGrass=T)             = 0.3571

MAP assignment given WetGrass=T:
  Rain = false
  Sprinkler = true
  WetGrass = true
  posterior probability             = 0.6429
```

## Types

```sml
type var = string
type cpt = real array array    (* row = parent assignment, col = variable value *)
type net = { order   : var list
           , parents : (var * var list) list
           , cpts    : cpt list }
```

## Known limitations

- **Binary variables only**: each variable must have exactly two states (false,
  true). Multi-valued variables are not supported.
- **Full enumeration**: inference is O(2^n) in the number of network variables.
  For networks with more than ~20 variables this becomes impractical. Variable
  elimination with factor operations is not yet implemented.
- `jointProb` requires a **complete** assignment (every variable present);
  missing or unknown variables raise `Fail`.
- No learning from data (parameter estimation / structure learning).

## Installing with smlpkg

```sh
smlpkg add github.com/sjqtentacles/sml-bayesnet
smlpkg sync
```

Reference from your `.mlb`:

```
lib/github.com/sjqtentacles/sml-bayesnet/bayesnet.mlb
```

## Building and testing

```sh
make test        # MLton
make test-poly   # Poly/ML
make all-tests   # both
make clean
```

## Project layout

```
sml.pkg
Makefile
lib/github.com/sjqtentacles/sml-bayesnet/
  bayesnet.sig     BAYESNET signature
  bayesnet.sml     CPT representation + enumeration inference (query/marginal/MAP)
  bayesnet.mlb
test/
  test.sml         query/marginal/jointProb/mostProbable on the sprinkler net
```

## License

MIT. See [LICENSE](LICENSE).
