# ecan release news

#  ecan 0.2.2.9000 (development)

* Added `twinspan()`, a native R implementation of TWINSPAN (Hill 1979) 
  and modified TWINSPAN (Roleček et al. 2009). 
  It is written from the published description of the algorithm and 
  is not a port of the original FORTRAN program.
    * Helpers: `pseudospecies()`, `tw_ra()`, `tw_inertia()`, 
      `tw_preference()` and `tw_two_way()`.
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
