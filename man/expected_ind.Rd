% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\docType{data}
\name{expected_ind}
\alias{expected_ind}
\title{Full list of expected indicators}
\format{
A data frame with 62 rows and 5 columns:
\describe{
\item{indicator_edms}{Parsed EDMS indicator name}
\item{age, sex}{EDMS indicator disaggregates}
\item{source}{Source folder on EDMS}
\item{expected}{Expected as part of EDMS Epi + 95s pull (TRUE)}
}
}
\usage{
expected_ind
}
\description{
A dataframe containing the different indicators and their disaggregate
components to ensure all data expected for are being exported from EDMS.
}
\keyword{datasets}
