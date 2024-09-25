# mindthegap 2.0
* Develop unit tests for `munge_edms` help ensures the code is performing as expected with annual updates and make updating/catching errors easier [2024-09-24]
* Create `munge_edms` (and internal sub functions + data) for handing UNAIDS data directly out of the EDMS database [2024-09-23]

# mindthegap 1.1
* Added a [codebook output](https://github.com/USAID-OHA-SI/mindthegap/blob/update-2023/data-raw/dataReporter_UNAIDS_2024_Clean_Estimates.pdf) [2024-08-02]
* Simplified the number of package dependencies [2024-08-02]
* Resolve vignette issue withe pkgdown [2024-08-02]
* Converted static year references to dyanmic ones [2024-08-01]
* Update pull_unaids to allow user to not specify a data type and return both in the same data frame [2024-08-01]
* Added a `unaids_year` to make the current year dynamic on the back end [2024-08-01]
* Updated the Google Ids and Sheet names to work for 2024 data [2024-06-23]
* Change instructions to install from rOpenSci [2024-01-04]
* Deprecate outdated function and internalize ones only needed within the package [2023-10-13]
* Create wrappers around `pull_unaids`, `pull_estimates` and `pull_testtreat` to  minimize typing and improve clarity [2022-10-13]
* Duplicate release with the "latest" tag to minimizing confusion and manual updating in future [2022-10-13]
* Quiet output for user when running `pull_unaids` [2022-10-13]

# mindthegap 1.0.0
* No change log capture in earlier version of mindthegap
