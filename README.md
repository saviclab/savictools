
<!-- README.md is generated from README.Rmd. Please edit that file -->

# savictools

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
  - `plot_pk()` creates PK concentration curves (“spaghetti plots”).
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

head(pop_example, n = 10)
#>        ID SEX AGE   WT   HT
#> 1  308857   2  38 13.9 95.0
#> 2   40640   1  45 15.6 99.0
#> 3  416012   2  40 12.2 87.6
#> 4  102521   2   8  7.5 69.0
#> 5  441236   2  50 11.6 91.5
#> 6  320895   2  43 14.4 96.7
#> 7  355954   2   1  3.0 48.4
#> 8   36653   1  30 11.0 78.5
#> 9   66799   1  36 12.0 86.2
#> 10   9352   1  20  8.9 78.0

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
#>  1  8357     0     1   200     0     0     1    34    11  83.2
#>  2  8357     0     0     0     0     0     1    34    11  83.2
#>  3  8357     4     0     0     0     4     1    34    11  83.2
#>  4  8357     5     1   200     0     0     1    34    11  83.2
#>  5  8357     8     0     0     0     3     1    34    11  83.2
#>  6  8357    12     1   200     0     0     1    34    11  83.2
#>  7  8357    12     0     0     0     0     1    34    11  83.2
#>  8  8357    16     0     0     0     4     1    34    11  83.2
#>  9  8357    20     0     0     0     8     1    34    11  83.2
#> 10  8357    24     0     0     0    12     1    34    11  83.2
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
#>  1     1     0     1   200     0     0  23.1  117.
#>  2     1     0     0     0     0     0  23.1  117.
#>  3     1     4     0     0     0     4  23.1  117.
#>  4     1     5     1   200     0     0  23.1  117.
#>  5     1     8     0     0     0     3  23.1  117.
#>  6     1    12     1   200     0     0  23.1  117.
#>  7     1    12     0     0     0     0  23.1  117.
#>  8     1    16     0     0     0     4  23.1  117.
#>  9     1    20     0     0     0     8  23.1  117.
#> 10     1    24     0     0     0    12  23.1  117.
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
#>  1     5     0     1   200     0     0  18.1  115.
#>  2     5     0     0     0     0     0  18.1  115.
#>  3     5     4     0     0     0     4  18.1  115.
#>  4     5     5     1   200     0     0  18.1  115.
#>  5     5     8     0     0     0     3  18.1  115.
#>  6     5    12     1   200     0     0  18.1  115.
#>  7     5    12     0     0     0     0  18.1  115.
#>  8     5    16     0     0     0     4  18.1  115.
#>  9     5    20     0     0     0     8  18.1  115.
#> 10     5    24     0     0     0    12  18.1  115.
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
#> # A tibble: 489 x 14
#>       ID  TIME   AMT    DV  EVID   MDV  ADDL    II    SS   TAD   SEX   AGE    WT
#>    <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
#>  1     1   0      25  0        1     1     0    12     1   0       2   121  27.3
#>  2     1  12      25  0        1     1     0     0     0   0       2   121  27.3
#>  3     1  24      25  0        1     1     0     0     0   0       2   121  27.3
#>  4     1  36      25  0        1     1     0     0     0   0       2   121  27.3
#>  5     1  48.0    25  0        1     1     0     0     0   0       2   121  27.3
#>  6     1  60.0    25  0        1     1     0     0     0   0       2   121  27.3
#>  7     1  71.7     0  1.31     0     0     0     0     0  11.6     2   121  27.3
#>  8     1  71.9    25  0        1     1     0     0     0   0       2   121  27.3
#>  9     1  72.9     0  2.60     0     0     0     0     0   1       2   121  27.3
#> 10     1  73.9     0  3.31     0     0     0     0     0   2       2   121  27.3
#> # … with 479 more rows, and 1 more variable: HT <dbl>

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
#> # A tibble: 489 x 14
#>       ID  TIME   AMT    DV  EVID   MDV  ADDL    II    SS   TAD   SEX   AGE    WT
#>    <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
#>  1     1   0      25  0        1     1     0    12     1   0       2   121  27.3
#>  2     1  12      25  0        1     1     0     0     0   0       2   121  27.3
#>  3     1  24      25  0        1     1     0     0     0   0       2   121  27.3
#>  4     1  36      25  0        1     1     0     0     0   0       2   121  27.3
#>  5     1  48.0    25  0        1     1     0     0     0   0       2   121  27.3
#>  6     1  60.0    25  0        1     1     0     0     0   0       2   121  27.3
#>  7     1  71.7     0  1.31     0     0     0     0     0  11.6     2   121  27.3
#>  8     1  71.9    25  0        1     1     0     0     0   0       2   121  27.3
#>  9     1  72.9     0  2.60     0     0     0     0     0   1       2   121  27.3
#> 10     1  73.9     0  3.31     0     0     0     0     0   2       2   121  27.3
#> # … with 479 more rows, and 1 more variable: HT <dbl>
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
