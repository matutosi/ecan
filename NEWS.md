# ecan release news

#  ecan 0.2.2.9000 (development)

* Added `twinspan()`, a native R implementation of TWINSPAN (Hill 1979) 
  and modified TWINSPAN (Roleček et al. 2009). 
  It is written in plain R from the published description of the 
  algorithm, and is not a port of the original FORTRAN program. 
  The known differences from the original are listed in `?twinspan`, 
  which also points to the `twinspan` package of Oksanen for those 
  who need the results of the original program.
    * `polish = "hill"` (the default) follows the steps of Hill's 
      program, and reproduces it exactly on the `dune`, `sipoo`, 
      `varespec`, `mite`, `BCI` and `pyrifos` data of 'vegan': the same 
      groups of stands and of species, with the same numbers, divisions 
      and eigenvalues, and on twenty randomly generated data sets. 
      `polish = "ecan"` keeps the earlier way, written from the 
      published description alone.
    * Rare pseudospecies are downweighted in the ordination as the 
      original does, which can be turned off with `downweight = FALSE`.
    * Helpers: `pseudospecies()`, `tw_ra()`, `tw_downweight()`, 
      `tw_inertia()`, `tw_preference()`, `tw_hill_const()` and 
      `tw_two_way()`.
    * `stats::as.hclust()` converts the result so that `cls_color()`, 
      `cls_add_group()` and `ggdendro::ggdendrogram()` can be used.

#  ecan 0.2.2

* 2026-08-18

* Bug fix in `ordination()`
    * `o_method = "pcoa"` returned eigen values in `$st_scores` and 
      nothing in `$eig_val`. It now returns the stand coordinates in 
      `$st_scores` and the eigen values in `$eig_val`.
* `$distance_method` is `NULL` for "ca" and "dca", 
  which do not use a distance, as it already was for "pca".
* Added tests for `cluster()`, `df2table()`, `table2df()`, `dist2df()`, 
  `ind_val()` and the one-to-multi helpers.
* Bug fixes
    * `shdi()` gave `NaN` for `h` when an abundance was zero. 
      A zero is now read as an absent species, as `s` already did.
    * `ind_val()` did not order its result at all. The species of a 
      group are now given in decreasing order of `ind.val`.
    * `cls_add_group()` turned every label into `NA` when a single 
      stand was missing from `df`. Only the labels of the missing 
      stands are `NA` now: `pad2longest()` no longer lets an `NA` 
      decide the width.
    * `dist2df()` dropped a distance of zero between two different 
      plots. It now drops the diagonal only.
    * `ordination()` raised an unrelated error for an unknown 
      `o_method`. It now names the method and lists the valid ones.
* `ord_add_group()` keeps the column named by `group`, even when that 
  column is not one-to-multi to `indiv`.
* `cols_one2multi()` and `select_one2multi()` spell their argument 
  `include_self`. The misspelt `inculde_self` is still accepted.
* `twinspan(polish = "hill")` takes the downweighting limits from 
  `tw_hill_const()` instead of the defaults of `tw_downweight()`. 
  The values are the same, so the results do not change.

#  ecan 0.2.1

* 2023-07-07

* Removed method "fsap" in `ordination()`, because package dave was archived.

#  ecan 0.2.0

* 2023-03-16

* Bug fix
* Release to CRAN

#  ecan 0.1.0

* 2022-05-24

* First release
    * Panels: Read data, Diversity, ISA (ind val), Cluster, Ordination.
    * Can download data.
    * Can show groups.
