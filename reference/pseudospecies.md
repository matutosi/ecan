# Pseudospecies transformation

Expands each species into binary pseudospecies by cut levels. A
pseudospecies of a cut level is present when the abundance is larger
than zero and not less than the cut level. Pseudospecies that occur in
no stand are dropped.

## Usage

``` r
pseudospecies(x, cut_levels = c(0, 2, 5, 10, 20))
```

## Arguments

- x:

  A community data matrix or data.frame. rownames: stands, colnames:
  species.

- cut_levels:

  A numeric vector of pseudospecies cut levels.

## Value

A binary matrix of stands by pseudospecies with "species", "level" and
"cut_levels" attributes.

## Examples

``` r
# \donttest{
data(dune, package = "vegan")
psp <- pseudospecies(dune)
dim(psp)
#> [1] 20 75
head(colnames(psp))
#> [1] "Achimill_1" "Agrostol_1" "Airaprae_1" "Alopgeni_1" "Anthodor_1"
#> [6] "Bellpere_1"
# }
```
