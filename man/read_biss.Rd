% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/read_biss.R
\name{read_biss}
\alias{read_biss}
\title{Read data from BiSS (Biodiversity Investigation Support System) to data frame.}
\usage{
read_biss(txt, join = TRUE)
}
\arguments{
\item{txt}{A JSON string, URL or file.}

\item{join}{A logical. TRUE: join plot and occurrence, FALSE: do not join.}
}
\value{
A data frame.
}
\description{
BiSS data is formatted as JSON.
}
\examples{
library(dplyr)
# path <- "set file path"
path <- "https://raw.githubusercontent.com/matutosi/biodiv/main/man/example.json"
read_biss(path)

}
