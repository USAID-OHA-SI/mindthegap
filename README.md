
# Mind The Gap Package

<!-- badges: start -->
[![Travis build status](https://travis-ci.com/USAID-OHA-SI/mindthegap.svg?branch=master)](https://travis-ci.com/USAID-OHA-SI/mindthegap)
<!-- badges: end -->


mindthegap was developed in 2019 in preparation for agency self assessments and COP to show UNAIDS 90-90-90 Progress and ART Gap. It has been turned into a package in advance of this year's cycle for ease of access.

### Installing mindthegap

mindthegap is not on CRAN, so you will have to install it directly from Github using devtools.

If you do not have the devtools package installed, you will have to run the `install.packages("devtools")` line in the code below as well. It also relies on `ICPIutitlies` which will need to be installed the same way.

```{r}
## SETUP

  #install
    install.packages("devtools")
    devtools::install_github("USAID-OHA-SI/mindthegap")
    devtools::install_github("ICPI/ICPIutilities")
    
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(mindthegap)

#load the data from Google Drive
  load_data()

#plot one country


```


## Data Sources

Data are sourced from UNAIDS' AIDSInfo [public] and PEPFAR Implemntation and Planning Attributes (IMPATT, now referred to as NAT_SUBNAT) [non public].

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
