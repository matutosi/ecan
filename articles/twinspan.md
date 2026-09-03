# TWINSPAN with ecan

## What TWINSPAN does

TWINSPAN (Two-Way INdicator SPecies ANalysis, Hill 1979) classifies a
community data table in the way a vegetation scientist arranges it by
hand: it splits the stands into two groups again and again, and it names
the species that best indicate each split. Both the stands and the
species are classified, hence *two-way*.

`ecan` implements it in plain R, so no compiler is needed. It is **not**
a port of Hill’s original FORTRAN program;
[`?twinspan`](https://matutosi.github.io/ecan/reference/twinspan.md)
lists the known differences.

``` r

data(dune, package = "vegan")
tw <- twinspan(dune)
tw
#> TWINSPAN
#>   stands:       20
#>   pseudospecies: 75 
#>   cut levels:   0 2 5 10 20
#>   divisions:    6
#>   groups:       7
#> 
#> division 1 at level 0 (n = 20, eig = 0.511)
#>   indicators: Ranuflam_1(+) Agrostol_1(+) Eleopalu_1(+) Lolipere_1(-)
#> division 2 at level 1 (n = 13, eig = 0.384)
#>   indicators: Hyporadi_1(-)
#> division 3 at level 1 (n = 7, eig = 0.411)
#>   indicators: Sagiproc_1(-)
#> division 4 at level 2 (n = 10, eig = 0.317)
#>   indicators: Planlanc_1(-)
#> division 5 at level 3 (n = 5, eig = 0.284)
#>   indicators: Achimill_1(+)
#> division 6 at level 3 (n = 5, eig = 0.301)
#>   indicators: Juncarti_1(+)
```

## The steps of one division

Each division is made in three steps.

**1. Pseudospecies.** TWINSPAN works on presence and absence, so a
quantitative table is first cut into binary pseudospecies at the cut
levels (by default `0, 2, 5, 10, 20`). A species with a cover of 7 is
present at the levels 1, 2 and 3, but not at 4 and 5.

``` r

psp <- pseudospecies(dune)
dim(psp)
#> [1] 20 75
head(colnames(psp))
#> [1] "Achimill_1" "Agrostol_1" "Airaprae_1" "Alopgeni_1" "Anthodor_1"
#> [6] "Bellpere_1"
```

**2. Primary ordination.** The first axis of a correspondence analysis
of the pseudospecies is found by reciprocal averaging, and the stands
are divided at its centroid.

``` r

ra <- tw_ra(psp)
ra$eig
#> [1] 0.5403802
```

**3. Refined and indicator ordination.** The division is then polished
using the species that prefer one side of it. The preference runs from
-1 (only in the negative group) to 1 (only in the positive group), and a
pseudospecies is *differential* when its absolute preference reaches
`diff_threshold` (1/3 by default, a frequency ratio of 2:1). Finally a
few of the most preferential pseudospecies are chosen as indicators,
which summarise the division without defining it.

``` r

pos <- ra$sample > 0
pref <- tw_preference(psp, pos)
head(sort(pref))
#> Achimill_1 Airaprae_1 Anthodor_1 Bellpere_1 Bromhord_1 Cirsarve_1 
#>         -1         -1         -1         -1         -1         -1
```

The indicators of every division are shown by
[`print()`](https://rdrr.io/r/base/print.html), together with the
eigenvalue of the axis that made the division (see the output above).

## Classification of the stands

The result gives one row per stand, with the group and the binary path
of the divisions that led to it.

``` r

head(tw$classification)
#> # A tibble: 6 × 4
#>   stand group path  depth
#>   <chr> <int> <chr> <int>
#> 1 11        1 00        2
#> 2 17        1 00        2
#> 3 19        1 00        2
#> 4 18        2 0100      4
#> 5 5         3 0101      4
#> 6 6         3 0101      4
table(tw$classification$group)
#> 
#> 1 2 3 4 5 6 7 
#> 3 1 4 4 1 3 4
```

The division tree becomes an `hclust` object, so the clustering helpers
of `ecan` and the usual plotting functions can be used.

``` r

cls <- stats::as.hclust(tw)
plot(cls, hang = -1, main = "TWINSPAN", xlab = "", sub = "")
```

![](twinspan_files/figure-html/dendrogram-1.png)

## Modified TWINSPAN

The original TWINSPAN divides every group of a level before going
deeper, so groups of the same level can differ widely in how
heterogeneous they are. Roleček et al. (2009) instead divide the most
heterogeneous group first, which makes the resulting groups more
comparable. `ecan` measures the heterogeneity by the total inertia of
the group.

``` r

tw_inertia(psp)
#> [1] 2.500848
tw_mod <- twinspan(dune, modified = TRUE, n_clusters = 4)
table(tw_mod$classification$group)
#> 
#>  1  2  3  4 
#>  3 10  3  4
```

With `n_clusters` the number of groups is chosen directly, which the
original hierarchy cannot do.

## The ordered two-way table

[`tw_two_way()`](https://matutosi.github.io/ecan/reference/tw_two_way.md)
arranges the stands and the species by their divisions and shows the cut
level of each cell. The digits below the table are the dichotomy of each
stand, and those on the right are the dichotomy of each species.

``` r

tw_two_way(tw)
#>          11115671123498111112
#>          1798   0      234560
#> Cirsarve -----------2--------  0000
#> Elymrepe ----2---22223-------  0000
#> Bellpere ---22--2-222--------  0001
#> Bromhord ----2-22-2-2--------  0001
#> Trifprat ----232-------------  0001
#> Airaprae -22-----------------  00100
#> Empenigr --2-----------------  00100
#> Hyporadi 223-----------------  00100
#> Vicilath 2--1---1------------  00100
#> Achimill -2--222212----------  00101
#> Anthodor -22-2222------------  00101
#> Planlanc 22-23332------------  00101
#> Lolipere 3--22333333322------  0011
#> Poaprat  21-22222223222-2----  0011
#> Rumeacet ----332-----2-2-----  0011
#> Bracruta 2-232322--22222--222  01
#> Poatriv  ----323223333223--2-  01
#> Scorautu 32332222-322222222-2  01
#> Trifrepe 2-222323-321222231--  01
#> Sagiproc 2-2--------32222----  100
#> Salirepe --22---------------3  100
#> Agrostol ----------2322232233  101
#> Alopgeni ---------2322333--2-  101
#> Juncbufo ------2-----2-22----  101
#> Ranuflam -------------2-22222  1100
#> Callcusp ----------------2-22  1101
#> Comapalu ----------------22--  1101
#> Eleopalu -------------2--2332  1101
#> Juncarti ------------22---222  1101
#> Chenalbu ---------------1----  111
#> 
#>          00000000000001111111
#>          00011111111110001111
#>             0000011111       
#>             0111100001
```

## References

Hill, M.O. (1979) *TWINSPAN: a FORTRAN program for arranging
multivariate data in an ordered two-way table by classification of the
individuals and attributes*. Cornell University, Ithaca.

Roleček, J., Tichý, L., Zelený, D. and Chytrý, M. (2009) Modified
TWINSPAN classification in which the hierarchy respects cluster
heterogeneity. *Journal of Vegetation Science* 20: 596-602.
