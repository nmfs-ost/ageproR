

<!-- README.md is generated from README.Rmd. Please edit that file -->

# ageproR

<!-- badges: start -->

<!-- badges: end -->

ageproR is a developing R-package designed to handle input data to and
from AGEPRO (Age Structured Projection Model) calculation engine.

> \[!IMPORTANT\]
>
> **If you using AGEPRO for production, please use:**
>
> - [AGEPRO-GUI, includes binary for AGEPRO calculation
>   engine](https://github.com/nmfs-ost/AGEPRO-GUI)
> - [AGEPRO Command Line based Calculation Engine source code
>   repo](https://github.com/noaa-pifsc/AGEPRO)

## Note

**ageproR** is still in development. These features will be implemented
in future updates.

- Predictor Recruitment Models
- Fixed Recruitment Model
- Empirical Cumulative Distribution Factor for w/ Linear Decline to Zero
- Markov Matrix Recruitment Model
- ~~Run AGEPRO models with the AGEPRO calculation engine within R.~~
- ~~Exported AGEPRO output R data objects.~~

Importing Stock Synthesis Report Data will handled as a separate
R-package project: ss3agepro

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
saves to the `AGEPRO VERSION 4.25` Input File Format.

**Note: Models sent to the AGEPRO calualtion engine MUST be in the
`AGEPRO VERSION 4.25` fornat**

Please refer to the [*AGEPRO Reference
Manual*](https://nmfs-ost.github.io/agepro-ref-manual/) for more
technical details.

# Agepro Model Workflow Examples

## Setting up a new `agepro_inp_model`

``` r
library("ageproR")

# Create a agepro_inp_model with default values
test <- ageproR::create_agepro_model()
# By Default, creating a new instance of agepro_inp_model with default values will include a 
# single "NULL Recritment Model", a placeholder recruitment model. It will print a WARNING 
# to replace this Model with a vaild recruitment model before saving to file. AGEPRO  
# Calcuation Engine will not recognize NULL Recruitment models.

# The function create_agepro_model is designed to ease users to create agepro model classes 
# wtihout the nuances of R6class initializtion. This is the equivalent to:
# test <- ageproR::agepro_inp_model$new()

# Set model with Recruit Model 14: "Empirical Cumulative Distribution Function of Recruitment". 
test$set_recruit_model(c(14))

# New agepro_models instances return NULL
test$bootstrap$bootstrap_file

# Load path of ageproR's included Example1 Bootstrap File
bsnfile <- system.file("extdata/Example1.BSN", package = "ageproR")

# Set Path of Bootstrap File 
# Note: Leaving parameter blank may request a file dialog window.
test$set_bootstrap_filename(bsnfile)
```

## Setting up a new `agepro_inp_model` w/ multiple recruitment models

``` r
# New instance of agepro_inp_model refreses class to default values.
# Specify the "number of recuitment models" (num_rec_models) at initializtion
test <- ageproR::create_agepro_model(num_rec_models=3)

# Multiple recruitments depend on agepro_model's "number of recruitment models". 
# For example, to set a 3 recruit agepro_inp_model w/ two Beverton-Holt models and a single
# Ricker model:
test$set_recruit_model(c(5,5,6)) 
# NOTE: A valid rerun of set_recruit_model will OVERWRITE the previous [RECRUIT] values.
```

## Reading or importing from AGEPRO input file (\*.inp)

> \[!NOTE\]
>
> Loading AGEPRO Input Files with multiple recuitment models will
> automatically set the **number of recruits** and overwrite the
> existing model’s recruitment data; For instance, reading a AGEPRO
> input file with multiple recruits will overwrite newly created model’s
> default data single NULL Recruitment.

``` r
# Load path of ageproR's included Example1 Input File
inpfile <- system.file("extdata/Example1.INP", package = "ageproR")

# Creates a new instance of agepro_inp_model class, wieh to default values,
# and imports model data from the agepro input data file from 'inpfile'
# Leaving the first parameter ('file') with a blank value will call for a file dialog window.
test <- ageproR::import_agepro_model(inpfile)

# The function import_agepro_model is designed to ease users from the nuances of R6class initializtion, 
# and is a combination of these calls:
# test <- ageproR::agepro_inp_model$new()
# test$read_inp(inpfile) 
```

### Setting Bootstrap Filepath when importing AGEPRO input data files

> \[!NOTE\]
>
> Input Files with the `AGEPRO VERSION 4.0` format can be loaded to
> ageproR currently, but will be deprecated. In future updates. By
> default, AGPRO input files will be saved in the `AGEPRO VERSION 4.25`
> format.

``` r
# Load path of ageproR's included Example1 Bootstrap File
bsnfile <- system.file("extdata/Example1.BSN", package = "ageproR")

# Reading agepro input files via `import_agepro_model` or `read_inp` will give a 
# WARNING for non-existant bootstrap paths.  

# Double check if the model's bootstrap filepath equals the intended "bsnpath"
test$bootstrap$bootstrap_file

# If not, set the path:
# testt$set_bootstrap_filename(bsnfile)
```

## Saving to AGEPRO Input File.

``` r
# Using the "test2" model .... 
# Using tempfile() as example filepath
outfile <- tempfile("example1_", fileext = ".inp")

# Note: Leaving parameter blank may request a file dialog window.  
test2$write_inp(outfile)
```

## Citation

Please cite AGEPRO as:

    Brodziak, Jon and Rago, Paul J. and Conser, Ramon. (1998). A General Approach for Making Short-Term 
    Stochastic Projections from an Age-Structured Fisheries Assessment Model. Fishery Stock Assessment 
    Models, 933–954. https://doi.org/10.4027/fsam.1998.52

The following [BibTeX](http://www.bibtex.org/) entry can be copied and
used in a .bib file:

``` bibtex
@article{article,
author = {Brodziak, Jon and Rago, Paul J. and Conser, Ramon},
year = {1998},
month = {01},
pages = {933-954},
title = {A General Approach for Making Short-Term Stochastic Projections from an Age-Structured Fisheries Assessment Model},
isbn = {9781566120579},
doi = {10.4027/fsam.1998.52}
}
```

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
