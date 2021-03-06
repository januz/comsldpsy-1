% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/text.R
\name{count_filter_results}
\alias{count_filter_results}
\title{Number of excluded participants per exclusion criterion}
\usage{
count_filter_results(data, filter_cond, output = c("full", "groups",
  "subgroups"))
}
\arguments{
\item{data}{tbl. The data frame to be filtered}

\item{output}{character. "full" returns full data frame; "groups" returns
excluded participants per group of exclusion criteria; "subgroups"
returns excluded participants per exclusion criterion.}

\item{df_filter_cond}{tbl. A data frame with exclusion criteria}
}
\value{
tbl. A data frame listing the number of excluded participants
per exclusion criterion.
}
\description{
Computes the number of participants that are excluded for each
exclusion criterion and totals for groups of exclusion criteria.
}
