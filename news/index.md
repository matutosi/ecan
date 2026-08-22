# Changelog

## ecan 0.2.2

CRAN release: 2026-08-22

- 2026-08-18

- Bug fix in
  [`ordination()`](https://matutosi.github.io/ecan/reference/ordination.md)

  - `o_method = "pcoa"` returned eigen values in `$st_scores` and
    nothing in `$eig_val`. It now returns the stand coordinates in
    `$st_scores` and the eigen values in `$eig_val`.

- `$distance_method` is `NULL` for “ca” and “dca”, which do not use a
  distance, as it already was for “pca”.

- Added tests for
  [`cluster()`](https://matutosi.github.io/ecan/reference/cluster.md),
  [`df2table()`](https://matutosi.github.io/ecan/reference/df2table.md),
  [`table2df()`](https://matutosi.github.io/ecan/reference/df2table.md),
  [`dist2df()`](https://matutosi.github.io/ecan/reference/df2table.md),
  [`ind_val()`](https://matutosi.github.io/ecan/reference/ind_val.md)
  and the one-to-multi helpers.

## ecan 0.2.1

CRAN release: 2023-07-07

- 2023-07-07

- Removed method “fsap” in
  [`ordination()`](https://matutosi.github.io/ecan/reference/ordination.md),
  because package dave was archived.

## ecan 0.2.0

CRAN release: 2023-03-23

- 2023-03-16

- Bug fix

- Release to CRAN

## ecan 0.1.0

- 2022-05-24

- First release

  - Panels: Read data, Diversity, ISA (ind val), Cluster, Ordination.
  - Can download data.
  - Can show groups.
