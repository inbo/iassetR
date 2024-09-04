
# iassetR

<!-- badges: start -->
[![R-CMD-check](https://github.com/inbo/iassetR/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/inbo/iassetR/actions/workflows/R-CMD-check.yaml)
[![Project Status: Unsupported – The project has reached a stable, usable state but the author(s) have ceased all work on it. A new maintainer may be desired.](https://www.repostatus.org/badges/latest/unsupported.svg)](https://www.repostatus.org/#unsupported)
<!-- badges: end -->

The goal of iassetR is to interact with the [iAsset](https://iasset.nl/en/) api using R. 
Currently only reading from the iAsset API is possible. 
But we intend to expand the scope to writing data as well if this is somewhat securely possible.

This package is developed for use in case of [VespaWatch](https://vespawatch.be/), which uses iAsset under the umbrella of the [RIPARIAS](https://www.riparias.be/) project. 
Early in the development a wider use, beyond the even the limits of RIPARIAS, was envissioned resulting in a name change from vespawatchR to iassetR. 

The current version of the package should be usable by all users of iAsset however keep in mind it has only been tested for vespawatch use. 
When you encounter issues please raise an [issue](https://github.com/inbo/iassetR/issues/new).

## Installation

You can install the development version of iassetR from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("inbo/iassetR")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(iassetR)
## basic example code
```

