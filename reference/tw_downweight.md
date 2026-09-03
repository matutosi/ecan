# Downweighting of rare pseudospecies

Gives a weight to each pseudospecies, so that the rare ones weigh less
in the ordination. The original TWINSPAN downweights them before the
correspondence analysis, and
[`twinspan()`](https://matutosi.github.io/ecan/reference/twinspan.md)
does the same by default. The weights are used only in the ordination:
the preference of the pseudospecies is counted on the raw occurrences.

## Usage

``` r
tw_downweight(
  y,
  method = c("hill", "decorana"),
  fraction = 5,
  frq_lim = 0.2,
  w_min = 0.01,
  rw = NULL
)
```

## Arguments

- y:

  A binary matrix of stands by pseudospecies.

- method:

  A string, "hill" or "decorana".

- fraction:

  A numeric of the downweighting fraction of "decorana".

- frq_lim:

  A numeric of the frequency above which "hill" does not downweight.

- w_min:

  A numeric of the smallest weight of "hill".

- rw:

  A numeric vector of stand weights, or NULL.

## Value

A numeric vector of the weight of each pseudospecies.

## Details

Two ways are available. "hill" is the `WEIGHT` subroutine of the
original TWINSPAN: a pseudospecies occurring in a smaller proportion of
the stands than `frq_lim` is weighted in proportion to that shortfall,
and no weight falls below `w_min`. "decorana" is the downweighting of
[`decorana()`](https://vegandevs.github.io/vegan/reference/decorana.html)
and of
[`vegan::downweight()`](https://vegandevs.github.io/vegan/reference/decorana.html),
where the frequencies are compared with the most frequent pseudospecies
instead of a fixed proportion.

## Examples

``` r
# \donttest{
data(dune, package = "vegan")
psp <- pseudospecies(dune)
summary(tw_downweight(psp))
#>    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#>  0.2575  0.6288  1.0000  0.8020  1.0000  1.0000 
summary(tw_downweight(psp, method = "decorana"))
#>    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#>  0.2778  0.6944  1.0000  0.8207  1.0000  1.0000 
# }
```
