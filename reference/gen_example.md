# Generate vegetation example

Stand, species, and cover are basic. Layer, height, st_group, are
sp_group optional.

## Usage

``` r
gen_example(
  n = 300,
  use_layer = TRUE,
  height_max = 20,
  ly_list = "",
  st_list = LETTERS[1:9],
  sp_list = letters[1:9],
  st_group = NULL,
  sp_group = NULL,
  cover_list = 2^(0:6)
)
```

## Arguments

- n:

  A numeric to generate no of occurrences.

- use_layer:

  A logical. If FALSE, height_max and ly_list will be omitted.

- height_max:

  A numeric. The highest layer of samples.

- ly_list, st_list, sp_list, st_group, sp_group:

  A string vector. st_group and sp_group are optional (default is NULL).
  Length of st_list and sp_list should be the same as st_group and
  sp_group, respectively.

- cover_list:

  A numeric vector.

## Value

A dataframe with columns: stand, layer, species, cover, st_group and
sp_group.

## Examples

``` r
n <- 300
height_max <- 20
ly_list    <- c("B1", "B2", "S1", "S2", "K")
st_list    <- LETTERS[1:9]
sp_list    <- letters[1:9]
st_group   <- rep(LETTERS[24:26], 3)
sp_group   <- rep(letters[24:26], 3)
cover_list <- 2^(0:6)
gen_example(n = n, use_layer = TRUE,
            height_max = height_max, ly_list = ly_list, 
            st_list  = st_list,  sp_list  = sp_list,
            st_group = st_group, sp_group = sp_group,
            cover_list = cover_list)
#> Joining with `by = join_by(stand)`
#> Joining with `by = join_by(stand, layer)`
#> Joining with `by = join_by(species)`
#> # A tibble: 217 × 7
#>    stand layer species cover st_group height sp_group
#>    <chr> <chr> <chr>   <dbl> <chr>     <dbl> <chr>   
#>  1 A     B1    c       32    X          19.7 z       
#>  2 A     B1    i        4    X          19.7 z       
#>  3 A     B1    d        3    X          19.7 x       
#>  4 A     B1    f        2    X          19.7 z       
#>  5 A     B1    g        2    X          19.7 x       
#>  6 A     B2    f       64    X          16.3 z       
#>  7 A     B2    e       32    X          16.3 y       
#>  8 A     B2    a        2    X          16.3 x       
#>  9 A     B2    g        2    X          16.3 x       
#> 10 A     B2    c        1.33 X          16.3 z       
#> # ℹ 207 more rows
```
