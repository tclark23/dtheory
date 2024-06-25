
<!-- README.md is generated from README.Rmd. Please edit that file -->

# dtheory

<!-- badges: start -->
<!-- badges: end -->

The goal of dtheory is to provide a more practical and useful set of
functions for carrying out d-studies from Generalizability Theory,
specifically for studies with a random p x t design.

## Installation

You can install the development version of dtheory from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("tclark23/dtheory")
```

## Usage

Imagine you had this simple data frame from a small study you did, and
you wanted to find the optimal number of trials for future studies.
Using Generalizability Theory, the dstudy() function calculates
G-coefficients for each specified number of trials, so that you can
determine the minimum number which would provide an adequate reliability
coefficient (typically 0.8 is set as the threshold).

``` r
library(dtheory)
x <- data.frame(c(1,1,1,2,2,2,3,3,3), c(1,2,3,1,2,3,1,2,3), c(12, 14, 12, 22, 22, 19, 17, 22, 5))
colnames(x) <- c("Person", "Trial", "Metric")
dstudy(x, col.scores = "Metric", from = 1, to = 10, by = 1)
#>        n = 1 n = 2 n = 3 n = 4 n = 5 n = 6 n = 7 n = 8 n = 9 n = 10
#> Metric 0.317 0.482 0.582  0.65 0.699 0.736 0.765 0.788 0.807  0.823
```

Just like magic!
