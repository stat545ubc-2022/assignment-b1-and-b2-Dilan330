AS-B1
================

# Loading libraries

We will start by loading the necessary libraries for this assignment

``` r
library(dplyr) 
```

    ## Warning: package 'dplyr' was built under R version 4.0.5

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
library(tidyverse)
```

    ## ── Attaching packages ─────────────────────────────────────── tidyverse 1.3.1 ──

    ## ✔ ggplot2 3.3.5     ✔ purrr   0.3.4
    ## ✔ tibble  3.1.4     ✔ stringr 1.4.0
    ## ✔ tidyr   1.1.3     ✔ forcats 0.5.1
    ## ✔ readr   1.4.0

    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()

``` r
library(datateachr) # this provides the dataset used to test our functions 
library(testthat) # this is used to test our functions
```

    ## 
    ## Attaching package: 'testthat'

    ## The following object is masked from 'package:purrr':
    ## 
    ##     is_null

    ## The following object is masked from 'package:tidyr':
    ## 
    ##     matches

    ## The following object is masked from 'package:dplyr':
    ## 
    ##     matches

# Making a Function and Documenting

This function will return the difference of the average height (per sex)
to an inputed height (in cm).

``` r
#' @tite Average Height Difference Function 
#'  
#' @description The `Avg_height_diff` function calculates the absolute difference of the average adult height per sex (178.1 cm for Canadian men & 164.7 cm for Canadian women) to an inputted height (in cm).
#'  
#'
#' @param height Height of person (in cm). I named this paramater as height because that is name the input. 
#' @param male Variable to indicate the sex of the inputted height. I named this paramater as male because that corresponds to the input. 
#'
#' @return Absolute value of the difference of the average height and the inputted height (in cm)
#' 
#'
#' @examples 
#' Avg_height_diff(181, male = TRUE)
#' Avg_height_diff(114, male = FALSE)
#' 
Avg_height_diff <- function(height, male = TRUE){
  if(!is.numeric(height)) { 
    stop('This function only works for numeric input!\n',
         'You have provided an object of class: ', class(height)[1])
  }
  if(is.na(height)) { 
    stop('This function only works for numeric input!\n',
         'You have provided an NA value ')
  }
  if(male == TRUE) {
    result <- abs(178.1 - height)
    paste("With the average Canadian male height at 178.1 cm, you are", round(result, 2), "cm different than the average.")
  } else {
    result2 <- abs(164.7 - height)
    paste("With the average Canadian female height at 164.7 cm, you are", round(result2, 2),  "cm different than the average.")
  }
}
```

# Function Examples

``` r
# We can see that inputting the same number will provide different answers depending on the sex
Avg_height_diff(160, male = TRUE)
```

    ## [1] "With the average Canadian male height at 178.1 cm, you are 18.1 cm different than the average."

``` r
Avg_height_diff(160, male = FALSE)
```

    ## [1] "With the average Canadian female height at 164.7 cm, you are 4.7 cm different than the average."

``` r
# Attempting to input non-numeric values will return an error
Avg_height_diff("cow", male = TRUE)
```

    ## Error in Avg_height_diff("cow", male = TRUE): This function only works for numeric input!
    ## You have provided an object of class: character

# Testing the function

Now we can use the `testthat` package to run some tests on the function
to ensure it works. We will be using the *cancer_sample* and
*apt_buildings* dataset from the `datateachr` package.

``` r
print(cancer_sample)
```

    ## # A tibble: 569 × 32
    ##          ID diagnosis radius_mean texture_mean perimeter_mean area_mean
    ##       <dbl> <chr>           <dbl>        <dbl>          <dbl>     <dbl>
    ##  1   842302 M                18.0         10.4          123.      1001 
    ##  2   842517 M                20.6         17.8          133.      1326 
    ##  3 84300903 M                19.7         21.2          130       1203 
    ##  4 84348301 M                11.4         20.4           77.6      386.
    ##  5 84358402 M                20.3         14.3          135.      1297 
    ##  6   843786 M                12.4         15.7           82.6      477.
    ##  7   844359 M                18.2         20.0          120.      1040 
    ##  8 84458202 M                13.7         20.8           90.2      578.
    ##  9   844981 M                13           21.8           87.5      520.
    ## 10 84501001 M                12.5         24.0           84.0      476.
    ## # … with 559 more rows, and 26 more variables: smoothness_mean <dbl>,
    ## #   compactness_mean <dbl>, concavity_mean <dbl>, concave_points_mean <dbl>,
    ## #   symmetry_mean <dbl>, fractal_dimension_mean <dbl>, radius_se <dbl>,
    ## #   texture_se <dbl>, perimeter_se <dbl>, area_se <dbl>, smoothness_se <dbl>,
    ## #   compactness_se <dbl>, concavity_se <dbl>, concave_points_se <dbl>,
    ## #   symmetry_se <dbl>, fractal_dimension_se <dbl>, radius_worst <dbl>,
    ## #   texture_worst <dbl>, perimeter_worst <dbl>, area_worst <dbl>, …

``` r
print(apt_buildings)
```

    ## # A tibble: 3,455 × 37
    ##       id air_conditioning amenities    balconies barrier_free_acc… bike_parking 
    ##    <dbl> <chr>            <chr>        <chr>     <chr>             <chr>        
    ##  1 10359 NONE             Outdoor rec… YES       YES               0 indoor par…
    ##  2 10360 NONE             Outdoor pool YES       NO                0 indoor par…
    ##  3 10361 NONE             <NA>         YES       NO                Not Available
    ##  4 10362 NONE             <NA>         YES       YES               Not Available
    ##  5 10363 NONE             <NA>         NO        NO                12 indoor pa…
    ##  6 10364 NONE             <NA>         NO        NO                Not Available
    ##  7 10365 NONE             <NA>         NO        YES               Not Available
    ##  8 10366 CENTRAL AIR      Indoor pool… YES       NO                Not Available
    ##  9 10367 NONE             <NA>         YES       YES               0 indoor par…
    ## 10 10368 NONE             Indoor recr… YES       YES               Not Available
    ## # … with 3,445 more rows, and 31 more variables: exterior_fire_escape <chr>,
    ## #   fire_alarm <chr>, garbage_chutes <chr>, heating_type <chr>, intercom <chr>,
    ## #   laundry_room <chr>, locker_or_storage_room <chr>, no_of_elevators <dbl>,
    ## #   parking_type <chr>, pets_allowed <chr>, prop_management_company_name <chr>,
    ## #   property_type <chr>, rsn <dbl>, separate_gas_meters <chr>,
    ## #   separate_hydro_meters <chr>, separate_water_meters <chr>,
    ## #   site_address <chr>, sprinkler_system <chr>, visitor_parking <chr>, …

## Test 1

Our first test checks to see if an error message will be displayed when
a non-numeric variable is used for the *height* parameter. We will be
using the *diagnosis* column from the *cancer_sample* dataset as our
non-numeric variable.

``` r
test_that("Function shows an error if non-numerical value is used as the height", {
  expect_error(Avg_height_diff(cancer_sample$diagnosis, male= FALSE))
})
```

    ## Test passed 🌈

## Test 2

Our second test checks to see if an error message will be displayed when
an NA value is used for the *height* parameter. We will be using *row 6*
of the *no_barrier_free_accessible_units* column from the
*apt_buildings* dataset to extract an NA value.

``` r
test_that("Function shows an error if NA value is used as the height", {
  expect_error(Avg_height_diff(apt_buildings$no_barrier_free_accessible_units[6], male = TRUE))
})
```

    ## Test passed 🥳

## Test 3

Our final test will ensure that the class for a given singular output is
a *character*. We will be using *row 1* of the *radius_mean* column from
the *cancer_sample* dataset to extract a *charachter* output.

``` r
test_that("Function produces a output with a charachter class type", {
  expect_true(is.character(Avg_height_diff(cancer_sample$radius_mean[1], male = TRUE)))
})
```

    ## Test passed 🥳
