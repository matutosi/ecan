# Check cols one-to-one, or one-to-multi in data.frame

Check cols one-to-one, or one-to-multi in data.frame

## Usage

``` r
is_one2multi(df, col_1, col_2)

is_one2one(df, col_1, col_2)

is_multi2multi(df, col_1, col_2)

cols_one2multi(df, col, include_self = TRUE, inculde_self)

select_one2multi(df, col, include_self = TRUE, inculde_self)

unique_length(df, col_1, col_2)
```

## Arguments

- df:

  A data.frame

- col, col_1, col_2:

  A string to specify a colname.

- include_self:

  A logical. If TRUE, return value including input col.

- inculde_self:

  Deprecated: the misspelt name of include_self, kept so that older code
  keeps working.

## Value

is_one2multi(), is_one2one(), is_multi2multi() return a logical.
cols_one2multi() returns strings of colnames that has one2multi relation
to input col. unique_length() returns a list.

## Examples

``` r
df <- tibble::tibble(
  x     = rep(letters[1:6], each = 1),
  x_grp = rep(letters[1:3], each = 2),
  y     = rep(LETTERS[1:3], each = 2),
  y_grp = rep(LETTERS[1:3], each = 2),
  z      = rep(LETTERS[1:3], each = 2),
  z_grp  = rep(LETTERS[1:3], times = 2))

unique_length(df, "x", "x_grp")
#> $x
#> [1] 6
#> 
#> $y
#> [1] 3
#> 
#> $xy
#> [1] 6
#> 

is_one2one(df, "x", "x_grp")
#> [1] FALSE
is_one2one(df, "y", "y_grp")
#> [1] TRUE
is_one2one(df, "z", "z_grp")
#> [1] FALSE
```
