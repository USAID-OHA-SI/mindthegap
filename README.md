# mindthegap <img src='man/figures/logo.png' align="right" height="120" />

<!-- badges: start -->
[![R-CMD-check](https://github.com/USAID-OHA-SI/mindthegap/workflows/R-CMD-check/badge.svg)](https://github.com/USAID-OHA-SI/mindthegap/actions)
[![mindthegap status badge](https://usaid-oha-si.r-universe.dev/badges/mindthegap)](https://usaid-oha-si.r-universe.dev/mindthegap)
[![Lifecycle: stable](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://lifecycle.r-lib.org/articles/stages.html#stable)
[![:name status badge](https://usaid-oha-si.r-universe.dev/badges/:name)](https://usaid-oha-si.r-universe.dev/)
<!-- badges: end -->

mindthegap was developed in 2019 in preparation for agency self assessments and COP to show UNAIDS 90-90-90 Progress and ART Gap. It has been turned into a package to process, clean, and export tidy UNAIDS Estimates data for ease of access.

## Installing mindthegap

`mindthegap` is not on CRAN, so you will have to install it directly from [rOpenSci](https://usaid-oha-si.r-universe.dev/packages) or [GitHub](https://github.com/USAID-OHA-SI/) using the code found below.


```{r}
## SETUP

  #install from rOpenSci
    install.packages('mindthegap', repos = c('https://usaid-oha-si.r-universe.dev', 'https://cloud.r-project.org'))
    
  #alt: install from GitHub using pak
    #install.packages("pak")
    #pak::pak("USAID-OHA-SI/mindthegap")
    
  #load the package
    library(mindthegap)

## LIST TYPES OF STYLES INCLUDED WITH PACKAGE
  ls("package:mindthegap")
    
```

## Example

The UNAIDS data reports on HIV estimates with uncertainty bounds from 1990 to 2021, and includes both epidemic control indicators and indicators that show progress to the 95s. The data is sourced from the UNAIDS AIDSInfo database, making it publicly accessible data.

To access the core set of UNAIDS Data, released in 2022, see the below example using `pull_unaids()`.

- There are 2 main groups of data in this dataset:
    - HIV/AIDS Estimates
    - HIV Testing and Treatment Cascade Data

This is a basic example of how to load the data from Google Drive using `pull_unaids()`. 


```{r}
library(mindthegap)

df_est <- pull_unaids(data_type = "HIV Estimates", pepfar_only = TRUE)
df_tt <- pull_unaids(data_type = "HIV Test & Treat", pepfar_only = TRUE)

```


## Data Sources

Data are sourced from UNAIDS' AIDSInfo [public] and PEPFAR Implementation and Planning Attributes (IMPATT, now referred to as NAT_SUBNAT) [non public].

  - UNAIDS Progress towards 90-90-90
    - data downloaded from aidsinfo.unaids.org/ 
    - data extracted on 2020-10-17
  - IMPATT data
    - data downloaded from pepfar-panorama.org/
    - MER_Structured_Datasets_NAT_SUBNAT_FY15-21_20200918_v2_1.txt
    - data extracted on 2020-09-30
    - munged IMPATT dataset stored on USAID Google Drive


---

*Disclaimer: The findings, interpretation, and conclusions expressed herein are those of the authors and do not necessarily reflect the views of United States Agency for International Development. All errors remain our own.*
