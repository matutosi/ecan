# Helper function for calculating diversity

Calculating diversity indices such as species richness (s), Shannon's H'
(h), Simpson' D (d), Simpson's inverse D (i).

## Usage

``` r
shdi(df, stand = NULL, species = NULL, abundance = NULL)
```

## Arguments

- df:

  A data.frame, which has three cols: stand, species, abundance.
  Community matrix should be converted using table2df().

- stand, species, abundance:

  A text to specify each column. If NULL, 1st, 2nd, 3rd column will be
  used.

## Value

A data.frame. Including species richness (s), Shannon's H' (h), Simpson'
D (d), Simpson's inverse D (i).

## Examples

``` r
data(dune, package = "vegan")
df <- table2df(dune)
shdi(df)
#> # A tibble: 20 × 5
#>    stand     s     h     d     i
#>    <chr> <int> <dbl> <dbl> <dbl>
#>  1 1         5  1.44 0.735  3.77
#>  2 10       12  2.40 0.903 10.3 
#>  3 11        9  2.11 0.867  7.53
#>  4 12        9  2.11 0.869  7.61
#>  5 13       10  2.10 0.852  6.76
#>  6 14        7  1.86 0.833  6   
#>  7 15        8  1.98 0.851  6.70
#>  8 16        8  1.96 0.843  6.37
#>  9 17        7  1.88 0.836  6.08
#> 10 18        9  2.08 0.861  7.22
#> 11 19        9  2.13 0.874  7.94
#> 12 2        10  2.25 0.890  9.09
#> 13 20        8  2.05 0.868  7.57
#> 14 3        10  2.19 0.879  8.25
#> 15 4        13  2.43 0.901 10.1 
#> 16 5        14  2.54 0.914 11.6 
#> 17 6        11  2.35 0.900 10.0 
#> 18 7        13  2.47 0.908 10.8 
#> 19 8        12  2.43 0.909 11.0 
#> 20 9        13  2.49 0.912 11.3 
```
