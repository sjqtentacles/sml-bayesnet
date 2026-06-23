# sml-bayesnet

[![CI](https://github.com/sjqtentacles/sml-bayesnet/actions/workflows/ci.yml/badge.svg)](https://github.com/sjqtentacles/sml-bayesnet/actions/workflows/ci.yml)

Discrete Bayesian network inference for Standard ML. Builds networks of binary
random variables with Conditional Probability Tables (CPTs) and performs exact
inference via full enumeration.

## API sketch

```sml
(* Use the built-in sprinkler network *)
val net : BayesNet.net = BayesNet.sprinkler ()

(* Compute P(Rain = true | WetGrass = true) *)
val p : real = BayesNet.query net "Rain" 1
(* p ≈ 0.357 *)

(* P(Sprinkler = false | WetGrass = true) *)
val q : real = BayesNet.query net "Sprinkler" 0
```

## Sprinkler network structure

```
Rain  →  Sprinkler
  ↘          ↘
       WetGrass
```

| P(Rain=T) | P(S=T\|R=F) | P(S=T\|R=T) | P(WG=T\|R=F,S=F) | ... |
|---|---|---|---|---|
| 0.2 | 0.5 | 0.1 | 0.0 | ... |

## Types

```sml
type var = string
type cpt = real array array    (* row = parent assignment, col = variable value *)
type net = { order   : var list
           , parents : (var * var list) list
           , cpts    : cpt list }
```

## Known limitations

- **Binary variables only**: each variable must have exactly two states (false=0,
  true=1). Multi-valued variables are not supported.
- **Full enumeration**: inference is O(2^n) in the number of network variables.
  For networks with more than ~20 variables this becomes impractical. Variable
  elimination with factor operations is not yet implemented.
- **No evidence clamping**: `query` conditions on no evidence beyond the built-in
  network structure; injecting arbitrary evidence requires constructing a new
  network or extending the API.
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
  bayesnet.sml     CPT representation + enumeration inference
  bayesnet.mlb
test/
  test.sml         sprinkler P(Rain|WetGrass) ≈ 0.357 test
```

## License

MIT. See [LICENSE](LICENSE).
