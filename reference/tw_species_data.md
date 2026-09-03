# Data on which the species of TWINSPAN are classified

The original TWINSPAN does not classify the species on the pseudospecies
table itself, but on how faithful each species is to the groups of
stands. Every group of the hierarchy, the terminal ones and the ones
that were divided further, becomes three pseudo-quadrats, at the cut
levels 0.8, 2 and 6 of the ratio between the frequency of the species
inside the group and its frequency outside. A species weighs as much as
it occurs, and a group weighs as much as it holds, multiplied by
`sqrt(2)` for every level it stands above the deepest one, and doubled
for the two upper cut levels.

## Usage

``` r
tw_species_data(
  object,
  psp = object$pseudospecies,
  sp_map = attr(psp, "species"),
  levmax = object$max_depth
)
```

## Arguments

- object:

  A "twinspan" object, or the result of `tw_tree()`.

- psp:

  The pseudospecies matrix of that object.

- sp_map:

  The species of each pseudospecies.

- levmax:

  The deepest level of division.

## Value

A list of the binary matrix (\$y) of species by pseudo-quadrats, the
weights of the species (\$rw) and of the pseudo-quadrats (\$cw), and the
ratios (\$ratio).

## Examples

``` r
# \donttest{
data(dune, package = "vegan")
tw <- twinspan(dune)
str(tw_species_data(tw))
#> List of 4
#>  $ y    : int [1:30, 1:69] 0 0 0 0 0 0 0 0 0 0 ...
#>   ..- attr(*, "dimnames")=List of 2
#>   .. ..$ : chr [1:30] "Achimill" "Agrostol" "Airaprae" "Alopgeni" ...
#>   .. ..$ : chr [1:69] "group_1_1" "group_1_2" "group_1_3" "group_2_1" ...
#>  $ rw   : num [1:30] 7 10 2 8 6 6 5 1 1 2 ...
#>  $ cw   : num [1:69] 3464 6928 6928 1697 3394 ...
#>  $ ratio: num [1:30, 1:23] 0 0 0 0 0 0 0 0 0 0 ...
#>   ..- attr(*, "dimnames")=List of 2
#>   .. ..$ : chr [1:30] "Achimill" "Agrostol" "Airaprae" "Alopgeni" ...
#>   .. ..$ : NULL
# }
```
