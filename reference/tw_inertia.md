# Total inertia of a pseudospecies matrix

Used as the heterogeneity of a group in modified TWINSPAN.

## Usage

``` r
tw_inertia(y, w = NULL)
```

## Arguments

- y:

  A binary matrix of stands by pseudospecies.

- w:

  A numeric vector of pseudospecies weights, or NULL.

## Value

A numeric of the total inertia (0 when it cannot be computed).

## Examples

``` r
# \donttest{
data(dune, package = "vegan")
tw_inertia(pseudospecies(dune))
#> [1] 2.500848
# }
```
