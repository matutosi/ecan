% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cluster.R
\name{cluster}
\alias{cluster}
\alias{cls_add_group}
\alias{cls_color}
\title{Helper function for clustering methods}
\usage{
cluster(x, c_method, d_method)

cls_add_group(cls, df, indiv, group)

cls_color(cls, df, indiv, group)
}
\arguments{
\item{x}{A community data matrix.
rownames: stands.
colnames: species.}

\item{c_method}{A string of clustering method.
"ward.D", "ward.D2", "single", "complete",
"average" (= UPGMA), "mcquitty" (= WPGMA),
"median" (= WPGMC), "centroid" (= UPGMC), or
"diana".}

\item{d_method}{A string of distance method.
"correlation", "manhattan", "euclidean", "canberra",
"clark", "bray", "kulczynski", "jaccard", "gower",
"altGower", "morisita", "horn", "mountford", "raup",
"binomial", "chao", "cao", "mahalanobis", "chisq",
"chord", "aitchison", or "robust.aitchison".}

\item{cls}{A result of cluster or dendrogram.}

\item{df}{A data.frame to be added into ord scores}

\item{indiv, group}{A string to specify indiv, group, row_name column in df.}
}
\value{
Result of clustering.
$clustering_method: c_method
$distance_method:   d_method

\if{html}{\out{<div class="sourceCode">}}\preformatted{    Inputing cls return a color vector, 
            inputing dend return a dend with color.
}\if{html}{\out{</div>}}
}
\description{
Helper function for clustering methods

Add group names to hclust labels.

Add colors to dendrogram
}
\examples{
library(vegan)
library(ggdendro)
data(dune)
data(dune.env)

df <- 
  table2df(dune) \%>\%
  dplyr::left_join(tibble::rownames_to_column(dune.env, "stand"))
sp_dammy <- 
 tibble::tibble("species" = colnames(dune), 
                "dammy_1" = stringr::str_sub(colnames(dune), 1, 1),
                "dammy_6" = stringr::str_sub(colnames(dune), 6, 6))
df <- 
  df \%>\%
  dplyr::left_join(sp_dammy)

cls <- cluster(dune, c_method = "average", d_method = "euclidean")
ggdendro::ggdendrogram(cls)

cls <- cls_add_group(cls, df, indiv = "stand", group = "Use")
ggdendro::ggdendrogram(cls)

}
