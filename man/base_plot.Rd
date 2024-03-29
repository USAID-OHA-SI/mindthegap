% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/base_plot.R
\name{base_plot}
\alias{base_plot}
\title{95's Table Plot}
\usage{
base_plot(sel_base, sel_cntry)
}
\arguments{
\item{sel_base}{Returns one of 2 table types eg "PLHIV", "Relative"}

\item{sel_cntry}{PEPFAR country to visualize (list OU name)}
}
\value{
df_tt
}
\description{
This function creates a summary table showing OU progress toward the 95-95-95's
}
\examples{
 \dontrun{
   base_plot(sel_base = "PLHIV", sel_cntry = "Lesotho")
   base_plot(sel_base = "Relative", sel_cntry = "Lesotho")
}

}
