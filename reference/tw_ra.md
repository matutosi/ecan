# Reciprocal averaging (first correspondence analysis axis)

Reciprocal averaging (first correspondence analysis axis)

## Usage

``` r
tw_ra(y, w = NULL, rw = NULL, max_iter = 999, tol = 1e-10)
```

## Arguments

- y:

  A binary matrix of stands by pseudospecies.

- w:

  A numeric vector of pseudospecies weights, or NULL.

- rw:

  A numeric vector of stand weights, or NULL.

- max_iter:

  An integer of the maximum number of iterations.

- tol:

  A numeric of the convergence tolerance.

## Value

A list of stand scores (\$sample), pseudospecies scores (\$species), the
eigenvalue (\$eig) and \$converged.

## Examples

``` r
# \donttest{
data(dune, package = "vegan")
ra <- tw_ra(pseudospecies(dune))
ra$eig
#> [1] 0.5403802
# }
```
