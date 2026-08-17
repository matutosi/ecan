# Helper function for ordination methods

Helper function for ordination methods

## Usage

``` r
ordination(tbl, o_method, d_method = NULL, ...)

ord_plot(ord, score = "st_scores", x = 1, y = 2)

ord_add_group(ord, score = "st_scores", df, indiv, group)

ord_extract_score(ord, score = "st_scores", row_name = NULL)
```

## Arguments

- tbl:

  A community data matrix. rownames: stands. colnames: species.

- o_method:

  A string of ordination method. "pca", "ca", "dca", "pcoa", or "nmds".
  "fspa" is removed, because package dave was archived.

- d_method:

  A string of distance method. "correlation", "manhattan", "euclidean",
  "canberra", "clark", "bray", "kulczynski", "jaccard", "gower",
  "altGower", "morisita", "horn", "mountford", "raup", "binomial",
  "chao", "cao", "mahalanobis", "chisq", "chord", "aitchison", or
  "robust.aitchison".

- ...:

  Other parameters for PCA.

- ord:

  A result of ordination().

- score:

  A string to specify score for plot. "st_scores" means stands and
  "sp_scores" species.

- x, y:

  A column number for x and y axis.

- df:

  A data.frame to be added into ord scores

- indiv, group, row_name:

  A string to specify indiv, group, row_name column in df.

## Value

ordination() returns result of ordination. \$st_scores: scores for
stand. \$sp_scores: scores for species. \$eig_val: eigen value for
stand. \$results_raw: results of original ordination function.
\$ordination_method: o_method. \$distance_method: d_method. ord_plot()
returns ggplot2 object. ord_extract_score() extracts stand or species
scores from ordination result. ord_add_group() adds group data.frame
into ordination scores.

## Examples

``` r
# \donttest{
library(ggplot2)
library(vegan)
#> Loading required package: permute
data(dune)
data(dune.env)

df <- 
  table2df(dune) %>%
  dplyr::left_join(tibble::rownames_to_column(dune.env, "stand"))
#> Joining with `by = join_by(stand)`
sp_dammy <- 
 tibble::tibble("species" = colnames(dune), 
                "dammy_1" = stringr::str_sub(colnames(dune), 1, 1),
                "dammy_6" = stringr::str_sub(colnames(dune), 6, 6))
df <- dplyr::left_join(df, sp_dammy)
#> Joining with `by = join_by(species)`

ord_dca <- ordination(dune, o_method = "dca")
ord_pca <- 
  df %>%
  df2table() %>%
  ordination(o_method = "pca")

ord_dca_st <- 
  ord_extract_score(ord_dca, score = "st_scores")
ord_pca_sp <- 
  ord_add_group(ord_pca, 
  score = "sp_scores", df, indiv = "species", group = "dammy_1")
#> Joining with `by = join_by(species)`
# }
```
