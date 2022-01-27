<!-- badges: start -->
[![R-CMD-check](https://github.com/USAID-OHA-SI/mindthegap/workflows/R-CMD-check/badge.svg)](https://github.com/USAID-OHA-SI/mindthegap/actions)
<!-- badges: end -->

# mindthegap <img src='man/figures/logo.png' align="right" height="120" />

mindthegap was developed in 2019 in preparation for agency self assessments and COP to show UNAIDS 90-90-90 Progress and ART Gap. It has been turned into a package to process, clean, and export tidy UNAIDS Estimates data for ease of access.

## Installing mindthegap

mindthegap is not on CRAN, so you will have to install it directly from Github using devtools.

If you do not have the devtools package installed, you will have to run the `install.packages("devtools")` line in the code below as well. It also relies on `ICPIutitlies` which will need to be installed the same way.

```{r}

  #install
    install.packages("devtools")
    devtools::install_github("USAID-OHA-SI/mindthegap")
    devtools::install_github("ICPI/ICPIutilities")
    
```

## Example

This is a basic example of how to load the data from Google Drive using `pull_unaids`.

```{r}
library(mindthegap)

df_est <- pull_unaids(sheetname = "HIV Estimates - Integer", pepfar_only = TRUE)

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
