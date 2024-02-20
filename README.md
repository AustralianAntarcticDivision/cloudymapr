
<!-- README.md is generated from README.Rmd. Please edit that file -->

# cloudymapr

<!-- badges: start -->

<!-- badges: end -->

The goal of cloudymapr is to provide cloud-oriented mapping
functionality for R. In particular, it aims to provide a leaflet-like
interactive map that works better with modern cloud-oriented file
formats (COGs and similar), and makes use of recent developments in GDAL
for fast data access and rendering.

Everything is entirely experimental at this stage.

## Installation

You can install the development version of cloudymapr from
[GitHub](https://github.com/) with:

``` r
# install.packages("remotes")
remotes::install_github("AustralianAntarcticDivision/cloudymapr")
```

## Demo

Try:

``` r
library(cloudymapr)
vl_demo()
```
