
# cleaned

<!-- badges: start -->
<!-- badges: end -->

The goal of cleaned is to assess changes from transforming livestock value chains.

## Installation

The development version can be installed via:

``` r
library("devtools")
devtools::install_github("ciat/cleaned")
```

## Usage

This is a basic example which shows you how to calculate feed basket quality

``` r
library(cleaned)
data(mufindi)
feed_basket_quality <- feed_quality(mufindi)
```

## More information

  - Please [report any issues or bugs](https://github.com/ciat/cleaned/issues).

  - License: MIT.

  - Get citation information for *cleaned* in R by typing `citation(package = "cleaned")`.

