# Helper function for Indicator Species Analysis

Calculating diversity indices such as species richness (s), Shannon's H'
(h), Simpson' D (d), Simpson's inverse D (i).

## Usage

``` r
ind_val(
  df,
  stand = NULL,
  species = NULL,
  abundance = NULL,
  group = NULL,
  row_data = FALSE
)
```

## Arguments

- df:

  A data.frame, which has three cols: stand, species, abundance.
  Community matrix should be converted using table2df().

- stand, species, abundance:

  A text to specify each column. If NULL, 1st, 2nd, 3rd column will be
  used.

- group:

  A text to specify group column.

- row_data:

  A logical. TRUE: return row result data of labdsv::indval().

## Value

A data.frame.

## Examples

``` r
# \donttest{
library(dplyr)
library(tibble)
data(dune, package = "vegan")
data(dune.env, package = "vegan")
df <- 
  dune %>%
  table2df(st = "stand", sp = "species", ab = "cover") %>%
  dplyr::left_join(tibble::rownames_to_column(dune.env, "stand"))
#> Joining with `by = join_by(stand)`
ind_val(df, abundance = "cover", group = "Moisture")
#> Joining with `by = join_by(numeric_Moisture)`
#> # A tibble: 30 × 4
#>    Moisture species  ind.val p.value
#>    <ord>    <chr>      <dbl>   <dbl>
#>  1 1        Achimill   0.348   0.208
#>  2 1        Lolipere   0.462   0.062
#>  3 1        Scorautu   0.291   0.793
#>  4 1        Planlanc   0.504   0.1  
#>  5 1        Trifprat   0.429   0.14 
#>  6 1        Vicilath   0.180   0.674
#>  7 2        Poaprat    0.358   0.345
#>  8 2        Bellpere   0.45    0.136
#>  9 2        Bromhord   0.302   0.179
#> 10 2        Cirsarve   0.25    0.3  
#> # ℹ 20 more rows
# }
```
