% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pull_unaids.R
\name{pull_unaids}
\alias{pull_unaids}
\title{Pull clean UNAIDS HIV Estimates and Test & Treat Data}
\usage{
pull_unaids(data_type, pepfar_only = TRUE)
}
\arguments{
\item{data_type}{returns one of 2  available data set types
eg "HIV Estimates", "HIV Test & Treat" (or both if missing)}

\item{pepfar_only}{filters dataset to only PEPFAR countries if TRUE (default = TRUE)}
}
\value{
df
}
\description{
Pull clean UNAIDS estimates
}
\examples{
 \dontrun{
   pull_unaids(data_type = "HIV Estimates", pepfar_only = TRUE)
}

}
