# mindthegap <img src="man/figures/logo.png" align="right" height="120"/>

<!-- badges: start -->

[![R-CMD-check](https://github.com/USAID-OHA-SI/mindthegap/workflows/R-CMD-check/badge.svg)](https://github.com/USAID-OHA-SI/mindthegap/actions) [![mindthegap status badge](https://usaid-oha-si.r-universe.dev/badges/mindthegap)](https://usaid-oha-si.r-universe.dev/mindthegap) [![Lifecycle: stable](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://lifecycle.r-lib.org/articles/stages.html#stable) [![:name status badge](https://usaid-oha-si.r-universe.dev/badges/:name)](https://usaid-oha-si.r-universe.dev/)

<!-- badges: end -->

## Overview

`mindthegap` was developed in 2019 in preparation for agency self assessments and COP to show UNAIDS 90-90-90 Progress and ART Gap. It has been turned into a package to process, clean, and export tidy UNAIDS Estimates data for ease of access. Each year, the package has evolved to meet the needs of the Office of HIV/AIDS. The current release shifts emphasis to UNAIDS 95-95-95 goals. Please see the [Articles](https://usaid-oha-si.github.io/mindthegap/articles/) section and [Changelog](https://usaid-oha-si.github.io/mindthegap/news/index.html) for details.

## Installing mindthegap

`mindthegap` is not on CRAN, so you will have to install it directly from [rOpenSci](https://usaid-oha-si.r-universe.dev/packages) or [GitHub](https://github.com/USAID-OHA-SI/) using the code found below.

``` r
## SETUP

  #install from rOpenSci
    install.packages(c('mindthegap', 'glitr'), repos = c('https://usaid-oha-si.r-universe.dev', getOption("repos")))
    
  #alt: install from GitHub using pak
    #install.packages("pak")
    #pak::pak("USAID-OHA-SI/mindthegap")
    #pak::pak("USAID-OHA-SI/glitr")
    
  #load the package
    library(mindthegap)

## LIST TYPES OF STYLES INCLUDED WITH PACKAGE
  ls("package:mindthegap")
  
```

## Example

The UNAIDS data reports on HIV estimates with uncertainty bounds from 1990 onward, and includes both epidemic control indicators and indicators that show progress to the 95s. The data are sourced from [UNAIDS EDMS](https://edms.unaids.org/), originally from the [UNAIDS data](aidsinfo.unaids.org/) database. Data available here are updated in July when UNAIDS issues a new releases.

To access the core set of UNAIDS Data see the below example using `load_unaids()`.

-   A core dataset contains both epidemic control and 95-95-95 goals indicators.
-   To view the available indicators `count()` the indicator column in the dataset.

``` r
library(mindthegap)

#load data
 df_unaids <- load_unaids(pepfar_only = TRUE)

#review structure
 str(df_unaids)
 
#review available indicators
 df_unaids %>% count(indicator)

#plot epi curve
 plot_epi_trends(df_unaids, sel_cntry = "Kenya")

#plot epi curve with a connected scatterplot
 plot_connected_scatter(df_unaids, sel_cntry = "Zambia")

#plot UNAIDS 95 target achievement
 tab_95s(df_unaids, "Namibia", denom = "PLHIV", grp = "All")
```

Additionally, data can be manually downloaded from the [GitHub Releases page](https://github.com/USAID-OHA-SI/mindthegap/releases).

------------------------------------------------------------------------

*Disclaimer: The findings, interpretation, and conclusions expressed herein are those of the authors and do not necessarily reflect the views of United States Agency for International Development. All errors remain our own.*
