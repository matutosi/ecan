# Read data from BiSS (Biodiversity Investigation Support System) to data frame.

BiSS data is formatted as JSON.

## Usage

``` r
read_biss(txt, join = TRUE)
```

## Arguments

- txt:

  A JSON string, URL or file.

- join:

  A logical. TRUE: join plot and occurrence, FALSE: do not join.

## Value

A data frame.

## Examples

``` r
library(dplyr)
# path <- "set file path"
path <- "https://raw.githubusercontent.com/matutosi/biodiv/main/man/example.json"
read_biss(path)
#> Warning: JSON string contains (illegal) UTF8 byte-order-mark!
#> Joining with `by = join_by(PLOT, Photo, Memo)`
#>     PLOT NO Investigator    Location Altitude Aspect Inclination T1_height
#> 1 biss01  1              Kobe, Jaoan       50      N          15        15
#> 2 biss01  1              Kobe, Jaoan       50      N          15        15
#> 3 biss01  1              Kobe, Jaoan       50      N          15        15
#> 4 biss02  2              Kobe, Jaoan      100      S          10        12
#> 5 biss02  2              Kobe, Jaoan      100      S          10        12
#> 6 biss02  2              Kobe, Jaoan      100      S          10        12
#>   T2_height S1_height S2_height H_height T1_cover T2_cover S1_cover S2_cover
#> 1        12         8         2      0.5       90       30       40       10
#> 2        12         8         2      0.5       90       30       40       10
#> 3        12         8         2      0.5       90       30       40       10
#> 4        10         6       1.8      0.3       90       20       30       10
#> 5        10         6       1.8      0.3       90       20       30       10
#> 6        10         6       1.8      0.3       90       20       30       10
#>   H_cover Photo Memo Layer Species Cover Abundance Rank Sampled Identified
#> 1      10               T1     sp1    80                  false       true
#> 2      10               T2     sp2    40                  false       true
#> 3      10                H     sp3                        false       true
#> 4      50                H     sp3                        false       true
#> 5      50                H     sp4   1.5                  false       true
#> 6      50                H     sp5   0.5                  false       true
#>   SameAs
#> 1       
#> 2       
#> 3       
#> 4       
#> 5       
#> 6       
```
