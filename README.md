
<!-- README.md is generated from README.Rmd. Please edit that file -->

# savictools

<!-- badges: start -->

[![R-CMD-check](https://github.com/saviclab/savictools/workflows/R-CMD-check/badge.svg)](https://github.com/saviclab/savictools/actions)
<!-- badges: end -->

## Overview

savictools is a collection of functions designed to make the
pharmacometrics workflow faster, simpler, and more intuitive.

##### Data preparation

  - `cohort()` generates simulation datasets for NONMEM.
  - `expwt()` computes expected weight for children under 5.
  - `sparse()` detects sparse vs. intensive PK sampling occasions.
  - `tad()` computes time after dose.
  - `zscores()` computes z-scores for WHO’s anthropometric indicators.

##### Visualization

  - `cr_plot()` visualizes the clinical relevance of covariates.
  - `etacorr_cat()` and `etacorr_cont()` plot ETA-covariate
    correlations.
  - `pk_plot()` creates PK concentration curves (“spaghetti plots”).
  - `VPC()` runs the Perl-speaks-Nonmem (PsN) `vpc` command from R, and
    is also a wrapper for xpose::VPC.

##### Workflow optimization

  - `param_table()` creates nicely formatted tables of parameter
    estimates for single or multiple models.
  - `parse_all_sse()` parses PsN `sse`-generated SSE\_results.csv files
    and combines them into dataframes summarizing parameter statistics
    and error rates.

## Installation

To install the development version from GitHub:

``` r
# install.packages("devtools")
devtools::install_github("saviclab/savictools")
```

## Usage

``` r
library(savictools)
```

#### Data preparation

##### `cohort()` is used to generate NONMEM-ready datasets for clinical PK trials, either by sampling from real datasets or generating synthetic data.

``` r

pop_example
#> # A tibble: 1,000 x 5
#>        ID   SEX   AGE    WT    HT
#>     <dbl> <dbl> <dbl> <dbl> <dbl>
#>  1 308857     2    38  13.9  95  
#>  2  40640     1    45  15.6  99  
#>  3 416012     2    40  12.2  87.6
#>  4 102521     2     8   7.5  69  
#>  5 441236     2    50  11.6  91.5
#>  6 320895     2    43  14.4  96.7
#>  7 355954     2     1   3    48.4
#>  8  36653     1    30  11    78.5
#>  9  66799     1    36  12    86.2
#> 10   9352     1    20   8.9  78  
#> # … with 990 more rows

# 1. Sampling 20 individuals, above 10 kg and below 120 cm, with a fixed dose of
# 200 mg, observing every 4 hours for one day and dosing at times 0, 5, and 12.
# Note that the data has columns called "WT" and "HT".

inc <- "WT > 10 & HT < 120"
ot <- seq(0, 24, by = 4)
dt <- c(0, 5, 12)

cohort(
  pop_example,
  include = inc,
  n = 20,
  obs_times = ot,
  dose_times = dt,
  amt = 200
)
#> # A tibble: 200 x 10
#>       ID  TIME  EVID   AMT    DV   TAD   SEX   AGE    WT    HT
#>    <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
#>  1 23381     0     1   200     0     0     2    53  18.8  111.
#>  2 23381     0     0     0     0     0     2    53  18.8  111.
#>  3 23381     4     0     0     0     4     2    53  18.8  111.
#>  4 23381     5     1   200     0     0     2    53  18.8  111.
#>  5 23381     8     0     0     0     3     2    53  18.8  111.
#>  6 23381    12     1   200     0     0     2    53  18.8  111.
#>  7 23381    12     0     0     0     0     2    53  18.8  111.
#>  8 23381    16     0     0     0     4     2    53  18.8  111.
#>  9 23381    20     0     0     0     8     2    53  18.8  111.
#> 10 23381    24     0     0     0    12     2    53  18.8  111.
#> # … with 190 more rows


# 2. Simulating data. We assume WT and HT are normally distributed random
# variables, with means and standard deviations of 16 and 3.4 for WT and 132
# and 13.6 for HT.

p1 <-
  list("WT" = list("rnorm", 16, 3.4),
       "HT" = list("rnorm", 132, 13.6))

cohort(
  param = p1,
  include = inc,
  n = 20,
  pop_size = 1000,
  obs_times = ot,
  dose_times = dt,
  amt = 200,
  original_id = FALSE
)
#> # A tibble: 200 x 8
#>       ID  TIME  EVID   AMT    DV   TAD    WT    HT
#>    <int> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
#>  1     5     0     1   200     0     0  11.3  111.
#>  2     5     0     0     0     0     0  11.3  111.
#>  3     5     4     0     0     0     4  11.3  111.
#>  4     5     5     1   200     0     0  11.3  111.
#>  5     5     8     0     0     0     3  11.3  111.
#>  6     5    12     1   200     0     0  11.3  111.
#>  7     5    12     0     0     0     0  11.3  111.
#>  8     5    16     0     0     0     4  11.3  111.
#>  9     5    20     0     0     0     8  11.3  111.
#> 10     5    24     0     0     0    12  11.3  111.
#> # … with 190 more rows

# 3. As in (2), except we now define a dosing function.

dose_fun <- function(WT) {
  ifelse(WT < 16, 150,
         ifelse(WT < 20, 200, 250))
}

cohort(
  param = p1,
  include = inc,
  n = 20,
  pop_size = 500,
  obs_times = ot,
  dose_times = dt,
  original_id = FALSE,
  amt = dose_fun
)
#> # A tibble: 200 x 8
#>       ID  TIME  EVID   AMT    DV   TAD    WT    HT
#>    <int> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
#>  1     1     0     1   150     0     0  14.7  119.
#>  2     1     0     0     0     0     0  14.7  119.
#>  3     1     4     0     0     0     4  14.7  119.
#>  4     1     5     1   150     0     0  14.7  119.
#>  5     1     8     0     0     0     3  14.7  119.
#>  6     1    12     1   150     0     0  14.7  119.
#>  7     1    12     0     0     0     0  14.7  119.
#>  8     1    16     0     0     0     4  14.7  119.
#>  9     1    20     0     0     0     8  14.7  119.
#> 10     1    24     0     0     0    12  14.7  119.
#> # … with 190 more rows
```

##### `expwt()` calculates expected weight based on age and sex in under-5 children, adding a column called EXPWT.

``` r
expwt(pop_example)
#> # A tibble: 1,000 x 6
#>        ID   SEX   AGE    WT    HT EXPWT
#>     <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
#>  1 308857     2    38  13.9  95    14.2
#>  2  40640     1    45  15.6  99    15.8
#>  3 416012     2    40  12.2  87.6  14.6
#>  4 102521     2     8   7.5  69     7.9
#>  5 441236     2    50  11.6  91.5  16.4
#>  6 320895     2    43  14.4  96.7  15.2
#>  7 355954     2     1   3    48.4   4.2
#>  8  36653     1    30  11    78.5  13.3
#>  9  66799     1    36  12    86.2  14.3
#> 10   9352     1    20   8.9  78    11.3
#> # … with 990 more rows
```

##### `sparse()` automatically detects sparse vs. intensive PK sampling in a dataset.

``` r
library(ggplot2)

pk_example
#> # A tibble: 489 x 7
#>       ID  TIME   AMT    DV  EVID  ADDL    II
#>    <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
#>  1     1   0      25  0        1     0    12
#>  2     1  12      25  0        1     0     0
#>  3     1  24      25  0        1     0     0
#>  4     1  36      25  0        1     0     0
#>  5     1  48.0    25  0        1     0     0
#>  6     1  60.0    25  0        1     0     0
#>  7     1  71.7     0  1.31     0     0     0
#>  8     1  71.9    25  0        1     0     0
#>  9     1  72.9     0  2.60     0     0     0
#> 10     1  73.9     0  3.31     0     0     0
#> # … with 479 more rows

sparse(pk_example, plot = TRUE)
```

<img src="man/figures/README-sparse-1.png" width="100%" />

``` r

sparse(pk_example, plot = TRUE) + 
  facet_wrap("ID", scales = "free_x", )
```

<img src="man/figures/README-sparse-2.png" width="100%" />

##### `tad()` computes time after dose.

``` r
# Deleting and re-calculating TAD
tad(pk_example)
#> # A tibble: 489 x 8
#>       ID  TIME   AMT    DV  EVID  ADDL    II   TAD
#>    <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
#>  1     1   0      25  0        1     0    12   0  
#>  2     1  12      25  0        1     0     0   0  
#>  3     1  24      25  0        1     0     0   0  
#>  4     1  36      25  0        1     0     0   0  
#>  5     1  48.0    25  0        1     0     0   0  
#>  6     1  60.0    25  0        1     0     0   0  
#>  7     1  71.7     0  1.31     0     0     0  11.6
#>  8     1  71.9    25  0        1     0     0   0  
#>  9     1  72.9     0  2.60     0     0     0   1  
#> 10     1  73.9     0  3.31     0     0     0   2  
#> # … with 479 more rows
```

##### `zscores()` computes z-scores indicating nutritional status.

``` r
zscores(pop_example, units = "months")
#> # A tibble: 1,000 x 10
#>        ID   SEX   AGE    WT    HT   WHZ   WAZ   BAZ   HAZ   BMI
#>     <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
#>  1 308857     2    38  13.9  95    0.01 -0.18  0.03 -0.37  15.4
#>  2  40640     1    45  15.6  99    0.41 -0.13  0.42 -0.64  15.9
#>  3 416012     2    40  12.2  87.6  0.14 -1.4   0.41 -2.55  15.9
#>  4 102521     2     8   7.5  69   -0.66 -0.48 -0.75  0.1   15.8
#>  5 441236     2    50  11.6  91.5 -1.32 -2.63 -1.06 -2.83  13.9
#>  6 320895     2    43  14.4  96.7  0.06 -0.38  0.07 -0.73  15.4
#>  7 355954     2     1   3    48.4 -0.17 -2.33 -1.3  -2.68  12.8
#>  8  36653     1    30  11    78.5  0.78 -1.62  1.52 -3.94  17.9
#>  9  66799     1    36  12    86.2  0.03 -1.48  0.44 -2.67  16.1
#> 10   9352     1    20   8.9  78   -1.53 -2.16 -1.15 -2.21  14.6
#> # … with 990 more rows
```

#### Visualization
