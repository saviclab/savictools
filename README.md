# savictools

## Overview
savictools is a collection of functions designed to make the pharmacometrics 
workflow faster, simpler, and more intuitive.

##### Data preparation
- `cohort()`
- `curve()`
- `expwt()`
- `sparse()`
- `tad()`
- `zscores()`

##### Visualization
- `cr_plot()`
- `etacorr_cat()`
- `etacorr_cont()`
- `plot_pk()`
- `VPC()`

##### Workflow optimization
- `param_table()`
- `parse_all_sse()`
- `parse_sse()`


## Installation
To install the development version from GitHub:
```R
# install.packages("devtools")  
devtools::install_github("saviclab/savictools")
``` 

## Usage
```R
library(savictools)
```

### Data preparation

#### `cohort()` is used to generate NONMEM-ready datasets for clinical PK trials, either by sampling from real datasets or generating synthetic data.

```R

pop_example

# 1. Sampling 20 individuals, above 10kg and below 120cm, with a fixed dose of 
# 200mg, observing every 4 hours for one day and dosing at times 0, 5, and 12. 
# Note that the data has columns called "WT" and "HT".


inc <- "WT > 10 & HT < 120"
ot <- seq(0, 24, by = 4)
dt <- c(0, 5, 12)

df1 <- cohort(pop_example, include = inc, n = 20, obs_times = ot,
              dose_times = dt, amt = 200)
df1
```

