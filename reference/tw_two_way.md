# Ordered two-way table of a TWINSPAN result

Arranges the community data with the stands and the species in the order
of their division paths, as in the printed output of TWINSPAN. The
dichotomy of each stand is shown by the digits below the table.

## Usage

``` r
tw_two_way(object, cells = c("level", "abundance"))

# S3 method for class 'tw_two_way'
print(x, ...)
```

## Arguments

- object:

  A "twinspan" object made with species = TRUE.

- cells:

  A string. "level": the pseudospecies cut level of each cell.
  "abundance": the original values.

- x:

  A "tw_two_way" object.

- ...:

  Ignored.

## Value

tw_two_way() returns a character matrix with class "tw_two_way",
"stand_path" and "species_path" attributes.

print() returns the object invisibly.

## Examples

``` r
# \donttest{
data(dune, package = "vegan")
tw_two_way(twinspan(dune))
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
# }
```
