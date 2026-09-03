# Constants of the original TWINSPAN

The values that Hill's FORTRAN program sets for one division. They are
used when `twinspan(polish = "hill")`, and are collected here so that
the correspondence with the original is easy to check.

## Usage

``` r
tw_hill_const()
```

## Value

A named list of the constants. `rat_lim`, `frq_lim`, `feeble`,
`icw_exp`, `ipr_exp`, `cwt_min`, `cr_long`, `cr_cut`, `polish_iter`,
`mz_crit`, `mz_out` and `mz_ind`.

## Examples

``` r
# \donttest{
unlist(tw_hill_const())
#>     rat_lim     frq_lim      feeble     icw_exp     ipr_exp     cwt_min 
#>        3.00        0.20        0.10        1.00        4.00        0.01 
#>     cr_long      cr_cut polish_iter     mz_crit      mz_out      mz_ind 
#>        0.20        0.20        2.00        8.00        4.00        4.00 
# }
```
