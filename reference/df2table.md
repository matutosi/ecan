# Convert data.frame and table to each other.

Convert data.frame and table to each other.

## Usage

``` r
df2table(df, st = "stand", sp = "species", ab = "abundance")

table2df(tbl, st = "stand", sp = "species", ab = "abundance")

dist2df(dist)
```

## Arguments

- df:

  A data.frame.

- st, sp, ab:

  A string.

- tbl:

  A table. community matrix. rownames: stands. colnames: species.

- dist:

  A distance table.

## Value

    df2table() return table,
    table2df() return data.frame,
    dist2df() return data.frame.

## Examples

``` r
tibble::tibble(
   st = paste0("st_", rep(1:2, times = 2)), 
   sp = paste0("sp_", rep(1:2, each = 2)), 
   ab = runif(4)) %>%
  dplyr::bind_rows(., .) %>%
  print() %>%
  df2table("st", "sp", "ab")
#> # A tibble: 8 × 3
#>   st    sp        ab
#>   <chr> <chr>  <dbl>
#> 1 st_1  sp_1  0.0808
#> 2 st_2  sp_1  0.834 
#> 3 st_1  sp_2  0.601 
#> 4 st_2  sp_2  0.157 
#> 5 st_1  sp_1  0.0808
#> 6 st_2  sp_1  0.834 
#> 7 st_1  sp_2  0.601 
#> 8 st_2  sp_2  0.157 
#>           sp_1      sp_2
#> st_1 0.1615003 1.2015218
#> st_2 1.6686661 0.3144169
```
