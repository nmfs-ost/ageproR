

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

You can install ageproR from the [NOAA Fisheries OST
Repository](https://github.com/nmfs-ost/ageproR) with:

``` r
# via `pak`
install.packages("pak")
pak::pkg_install("nmfs-ost/ageproR")

# alternative method
install.packages("remotes")
remotes::install_github("nmfs-ost/ageproR")
```

## Configuring ageproR with the AGEPRO calcuation engine

ageproR requires the command line based [AGEPRO Calculation
Engine](https://github.com/noaa-pifsc/AGEPRO) to run model projections.
Download the [AGEPRO calculation engine
binary](https://github.com/noaa-pifsc/AGEPRO/releases) at the NOAA-PIFSC
github repository.

Store the AGEPRO calcuation engine binary to a folder location, this
will be the directory path used to connect ageproR with AGEPRO with each
model launch. For convenience, you can set an user-level R envrionmental
variable.

### Open `.Renviron` file

The following command will allow you to set the AGEPRO path to your
user-level .Renviron file. This command opens `~/.Renviron` file in your
editor.

``` r
usethis::edit_r_environ()
```

### Set the `AGEPRO_PATH` value and save `.Renviron`

> \[!WARNING\]
>
> **Windows Paths:** You must use forward slashes (`/`) or double
> backslashes (`\\`), not the standard single Windows backslashes (`\`).

In `.Renviron`, add the line `AGEPRO_PATH`, with your directory. Save
the changes.

    AGEPRO_PATH="C://path//to//folder//AGEPRO.EXE"

### Restart R and Verify

**You must restart the R session** for the changes to make effect. Once
R is restarted, load `ageproR`. If set correctly, you will see an
confirmation message:

    library(ageproR)
    #>'AGEPRO_PATH' found at: "C://path//to//folder//AGEPRO.EXE"

## AGEPRO input file format

**ageproR** is compatible with the AGEPRO input file formats
`AGEPRO VERSION 4.0` & `AGEPRO VERSION 4.25`. By default, **ageproR**
saves AGEPRO input files in the `AGEPRO VERSION 4.25` format.

Loading an input file with the `AGEPRO VERSION 4.0` format to
**ageproR** will maintain its version format until it is saved (or
launched to AGEPRO calcuation engine) in the `AGEPRO VERSION 4.25`
format.

**Models sent to the AGEPRO calualtion engine MUST be in the
`AGEPRO VERSION 4.25` fornat**

Please refer to the [*AGEPRO Reference
Manual*](https://nmfs-ost.github.io/agepro-ref-manual/) for more
technical details.

# Agepro Model Workflow Examples

## Setting up a new `agepro_inp_model`

By Default, creating a new instance of `agepro_inp_model` with default
values will include a single **NULL Recritment Model**, a placeholder
recruitment model. This will trigger a WARNING message to set and
replace this Model with a vaild recruitment model before saving to file.
*The AGEPRO calcuation engine does not recognize models with **NULL
Recruitment Model** of `0`, and will not run this model*.

``` r
# If AGEPRO_PATH is setup, it will print the path of the AGEPRO Calcualtion Engine binary
# right after ageproR package is loaded.
# See readme section 'Configuring ageproR with the AGEPRO calcuation engine' for details
library("ageproR")

# Create a agepro_inp_model with default values
test <- ageproR::create_agepro_model()

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

Loading AGEPRO Input Files with multiple recuitment models will
automatically set the **number of recruits** and overwrite the existing
model’s recruitment data; For instance, reading a AGEPRO input file with
multiple recruits will overwrite newly created model’s default data
single NULL Recruitment.

``` r
# Load path of ageproR's included Example1 Input File
inpfile <- system.file("extdata/Example1.INP", package = "ageproR")

# Creates a new instance of agepro_inp_model class, wieh to default values,
# and imports model data from the agepro input data file from 'inpfile'
# Leaving the first parameter ('file') with a blank value will call for a file dialog window.
test2 <- ageproR::import_agepro_model(inpfile)

# The function import_agepro_model is designed to ease users from the nuances of R6class initializtion, 
# and is a combination of these calls:
# test <- ageproR::agepro_inp_model$new()
# test$read_inp(inpfile) 
```

### Setting Bootstrap Filepath when importing AGEPRO input data files

``` r
# Load path of ageproR's included Example1 Bootstrap File
bsnfile <- system.file("extdata/Example1.BSN", package = "ageproR")

# Reading agepro input files via `import_agepro_model` or `read_inp` will give a 
# WARNING for non-existant bootstrap paths.  

# Double check if the model's bootstrap filepath equals the intended "bsnpath"
test2$bootstrap$bootstrap_file

# If not, set the path:
# test2$set_bootstrap_filename(bsnfile)
```

## Saving to AGEPRO Input File.

By default, AGEPRO input files is saved in the `AGEPRO VERSION 4.25`
format.

``` r
# Using the "test2" model .... 
# Using tempfile() as example filepath
outfile <- tempfile("example1_", fileext = ".inp")

# Note: Leaving parameter blank may request a file dialog window.  
test2$write_inp(outfile)
```

## Launch model

Ensure that the model’s “Export to R” option is enabled (‘1’) before
launching to the AGEPRO calcuation engine. When this flag is saved in
the AGEPRO input file, it will allow AGEPRO to export output data as a
Rdata file (`*.rdat`)

By default, `launch_model` creates a “job directory” in the `~/AGEPRO`
folder, and the job directory name comes from the model’s **Case Id**
name (untitled if null/blank) and a timestamp. The Job Directory will
contain: AGEPRO Input Data file, Bootstrap file, and AGEPRO output
files.

``` r
# Using `test`from 'setting a new agepro_inp_model'
test$options$export_df # Check "Export to R" option is enabled: should be 1
launch_model(test)

# Using `test2` from 'Reading or importing from AGEPRO input file (*.inp)'
test2$options$export_df # Check "Export to R" option is enabled:  should be 1
launch_model(test2)
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
