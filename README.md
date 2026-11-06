
# robustsens

<!-- badges: start -->
<!-- badges: end -->

The goal of robustsens is to measure how outliers affects sensor signal, using the empirical
sensitivity curve. Its case study is the SpO2 signal of a pulse oximeter.

## Installation

You can install the development version of robustsens like so:

``` r
# run from the folder containing file.
remotes::install_github("iCampapp/robustsens")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(robustsens)
## basic example code
plot_sensitivity(simulate_spo2(100))
```

