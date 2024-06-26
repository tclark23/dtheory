
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
x <- data.frame(c(1,1,1,2,2,2,3,3,3), c(1,2,3,1,2,3,1,2,3), c(12, 14, 12, 22, 22, 19, 17, 22, 20))
colnames(x) <- c("Person", "Trial", "Metric")
dstudy(x, col.scores = "Metric", from = 1, to = 10, by = 1)
#>        n = 1 n = 2 n = 3 n = 4 n = 5 n = 6 n = 7 n = 8 n = 9 n = 10
#> Metric 0.844 0.915 0.942 0.956 0.964  0.97 0.974 0.977  0.98  0.982
```

Just like magic!

Or, say you have already decided on the number of trials you wish to
use. Continuing from the previous example, let’s use n = 10. But now,
you want to find the confidence interval for that G-coefficient at the
alpha = .05 confidence level. For that, we can use the `dconf` function!

``` r
dconf(x, col.scores = "Metric", n = 10)
#>                G-coef Lower Bound Upper Bound
#> Metric, n = 10  0.982       0.645           1
```

NOTE: The `dconf()` confidence interval calculation, originally
developed by Arteaga et al. (1982) assumes that score effects are
normally distributed. Hence, any attempt to use `dconf()`with non-normal
data will not produce a reasonable result!
