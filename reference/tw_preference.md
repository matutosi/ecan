# Preference of pseudospecies for one side of a division

The preference is (f2 - f1) / (f2 + f1), where f1 and f2 are the
relative frequencies of the pseudospecies in the negative and the
positive group. It ranges from -1 (only in the negative group) to 1.

## Usage

``` r
tw_preference(y, positive)
```

## Arguments

- y:

  A binary matrix of stands by pseudospecies.

- positive:

  A logical vector. TRUE: the stand is in the positive group.

## Value

A numeric vector of the preference of each pseudospecies.

## Examples

``` r
# \donttest{
data(dune, package = "vegan")
psp <- pseudospecies(dune)
pos <- tw_ra(psp)$sample > 0
summary(tw_preference(psp, pos))
#>    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#> -1.0000 -1.0000 -0.3333 -0.2077  0.6364  1.0000 
# }
```
