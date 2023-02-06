<!-- badges: start -->
[![R-CMD-check](https://github.com/USAID-OHA-SI/mindthegap/workflows/R-CMD-check/badge.svg)](https://github.com/USAID-OHA-SI/mindthegap/actions)
<!-- badges: end -->

# mindthegap <img src='man/figures/logo.png' align="right" height="120" />

mindthegap was developed in 2019 in preparation for agency self assessments and COP to show UNAIDS 90-90-90 Progress and ART Gap. It has been turned into a package to process, clean, and export tidy UNAIDS Estimates data for ease of access.

## Installing mindthegap

mindthegap is not on CRAN, so you will have to install it directly from Github using devtools.

If you do not have the devtools package installed, you will have to run the `install.packages("devtools")` line in the code below as well.

```{r}

  #install
    install.packages("devtools")
    devtools::install_github("USAID-OHA-SI/mindthegap")
    
```

## Example

The UNAIDS data reports on HIV estimates with uncertainty bounds from 1990 to 2021, and includes both epidemic control indicators and indicators that show progress to the 95s. The data is sourced from the UNAIDS AIDSInfo database, making it publicly accessible data.

To access the core set of UNAIDS Data, released in 2021, see the below example using `pull_unaids()`.


This is a basic example of how to load the data from Google Drive using `pull_unaids`.

```{r}
library(mindthegap)

df_est <- pull_unaids(orginal_unaids = TRUE, data_type = "HIV Estimates", pepfar_only = TRUE)
df_tt <- pull_unaids(orginal_unaids = TRUE, data_type = "HIV Test & Treat", pepfar_only = TRUE)

```

Our team also receives a secondary dataset from UNAIDS with indicators that are not found in the original dataset (i.e. total deaths of PLHIV, deaths averted due to ART, etc.) To access this dataset using `pull_unaids()`, see the example below. Note that the parameter `original_unaids = FALSE` is set to FALSE to access these data.

```{r}
library(mindthegap)

df_epi <- pull_unaids(orginal_unaids = FALSE, data_type = "epicontrol", pepfar_only = TRUE)
df_cascade <- pull_unaids(orginal_unaids = FALSE, data_type = "cascade", pepfar_only = TRUE)
df_prev <- pull_unaids(orginal_unaids = FALSE, data_type = "prev", pepfar_only = TRUE)


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
