
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ageproR

<!-- badges: start -->

<!-- badges: end -->

ageproR is a developing R-package designed to handle input data to and
from AGEPRO (Age Structured Projection Model) calculation engine.

> \[!IMPORTANT\]
>
> **If you using AGEPRO for production, please use:**
> <!-- Leave links to PIFSC until repos are migrated to nmfs-ost -->
>
> - [Original AGEPRO command-line calculation engine (source
>   code)](https://github.com/PIFSCstockassessments/AGEPRO)  
> - [GUI interface for AGEPRO (installer &
>   source)](https://github.com/PIFSCstockassessments/AGEPRO-GUI)

## Note

**ageproR** is still in development. These features will be implemented
in future updates.

- Predictor Recruitment Models
- Fixed Recruitment Model
- Empirical Cumulative Distribution Factor for w/ Linear Decline to Zero
- Markov Matrix Recruitment Model
- Run AGEPRO models with the AGEPRO calculation engine within R.
- Exported AGEPRO output R data objects.

Importing Stock Synthesis report data as AGEPRO input data is being
developed as a separate R-package: ss3agepro

## Installation

You can install the development version of ageproR from [NOAA Fisheries
OST Repository](https://github.com/nmfs-ost/ageproR) with:

``` r
# via `pak`
install.packages("pak")
pak::pkg_install("nmfs-ost/ageproR")

# alternative method
install.packages("remotes")
remotes::install_github("nmfs-ost/ageproR")
```

## AGEPRO input file format

**ageproR** is compatible with the AGEPRO input file formats
`AGEPRO VERSION 4.0` & `AGEPRO VERSION 4.25`. By default, **ageproR**
saves to the `AGEPRO VERSION 4.0` Input File Format.

Please refer to the *AGEPRO Reference Manual* for more technical
details.

## Hawaiian Uku Projection base example

TODO

<!-- Do not edit below. This adds the Disclaimer and NMFS footer. -->

------------------------------------------------------------------------

## Disclaimer

“This repository is a scientific product and is not official
communication of the National Oceanic and Atmospheric Administration, or
the United States Department of Commerce. All NOAA GitHub project code
is provided on an ‘as is’ basis and the user assumes responsibility for
its use. Any claims against the Department of Commerce or Department of
Commerce bureaus stemming from the use of this GitHub project will be
governed by all applicable Federal law. Any reference to specific
commercial products, processes, or services by service mark, trademark,
manufacturer, or otherwise, does not constitute or imply their
endorsement, recommendation or favoring by the Department of Commerce.
The Department of Commerce seal and logo, or the seal and logo of a DOC
bureau, shall not be used in any manner to imply endorsement of any
commercial product or activity by DOC or the United States Government.”

------------------------------------------------------------------------

<img src="https://raw.githubusercontent.com/nmfs-general-modeling-tools/nmfspalette/main/man/figures/noaa-fisheries-rgb-2line-horizontal-small.png" width="200" style="height: 75px !important;"  alt="NOAA Fisheries">

[U.S. Department of Commerce](https://www.commerce.gov/) \| [National
Oceanographic and Atmospheric Administration](https://www.noaa.gov) \|
[NOAA Fisheries](https://www.fisheries.noaa.gov/)
