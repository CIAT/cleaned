# Inclusive and Comprehensive Livestock Environmental Assessment for Improved Nutrition, a Secured Environment, and Sustainable Development along Livestock Value Chains (I-CLEANED)

The goal of I-CLEANED is to assess changes from transforming livestock value chains by prioritising profitable, environmentally sustainable and inclusive livestock interventions.

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

