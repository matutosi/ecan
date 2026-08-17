# Pad a string to the longest width of the strings.

Pad a string to the longest width of the strings.

## Usage

``` r
pad2longest(string, side = "right", pad = " ")
```

## Arguments

- string:

  Strings.

- side:

  Side on which padding character is added (left, right or both).

- pad:

  Single padding character (default is spaces).

## Value

        Strings.

## Examples

``` r
x <- c("a", "ab", "abc")
pad2longest(x, side = "right", pad = " ")
#> [1] "a  " "ab " "abc"
```
