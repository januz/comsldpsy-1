% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/text.R
\name{report_trend_or}
\alias{report_trend_or}
\title{Odds ratio and CI for trend test}
\usage{
report_trend_or(data, psy)
}
\arguments{
\item{data}{tbl. Data frame with results of the trend test}

\item{psy}{character. Psychopathology variable name}
}
\value{
character. String reporting odds ratio and CI,
e.g., "1.25 (95% CI = 1.15–1.35)"
}
\description{
Outputs a character string with the odds ratio and CI for
a specified psychopathology
}
