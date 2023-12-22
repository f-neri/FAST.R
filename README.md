
<!-- README.md is generated from README.Rmd. Please edit that file -->

# FAST.R <a href="https://f-neri.github.io/FAST.R/"><img src="man/figures/logo.png" align="right" height="100" alt="FAST.R website" /></a>

<!-- badges: start -->

[![R-CMD-check](https://github.com/f-neri/FAST.R/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/f-neri/FAST.R/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

The goal of FAST.R is to launch a user-friendly R shiny app to perform
the data analysis and visualization for the Fully Automated Senescence
Test (FAST) workflow. For a step-by-step protocol and simple, no-code
installation, go to [this page](https://gerencserlab.github.io/FAST/).

## Installation

You can install the CRAN version of FAST.R from
[CRAN](https://cran.r-project.org/) with:

``` r
install.packages("FAST.R")
```

You can install the development version of FAST.R from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("f-neri/FAST.R")
```

## Use

To launch the app, use `FAST.R()`

``` r
library(FAST.R)
FAST.R()
```
