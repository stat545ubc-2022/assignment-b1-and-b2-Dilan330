
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ASB2

<!-- badges: start -->
<!-- badges: end -->

The goal of ASB2 is to provide information regarding the average adult
height (per sex) and compare it to inputted heights. This package
contains a function which displays the average adult height (for the
selected sex) and calculates the absolute difference between it and an
inputted height (in cm)

## Installation

You can install the development version of ASB2 like so:

``` r
devtools::install_github("stat545ubc-2022/assignment-b1-and-b2-Dilan330")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(ASB2)

## Finding the absolute difference between the Canadian average height for a male and an inputted height (160 cm)
Avg_height_diff(160, male = TRUE)
#> [1] "With the average Canadian male height at 178.1 cm, you are 18.1 cm different than the average."
```
