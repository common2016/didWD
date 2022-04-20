
<!-- README.md is generated from README.Rmd. Please edit that file -->

# didWD

<!-- badges: start -->
<!-- badges: end -->

The goal of didWD is to make a DID regression with staggered treatment
based on Wooldridge (2021).

## Installation

You can install the development version of didWD from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("common2016/didWD")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(didWD)
library(plm)
## basic example code
fit <- didWD(stg6, id = 'id', year = 'year', y = 'logy', w = 'w')
lmtest::coeftest(fit$fit, vcov. = vcovHC, method = 'white2')
#> 
#> t test of coefficients:
#> 
#>             Estimate Std. Error t value  Pr(>|t|)    
#> d2014:f2014 0.179982   0.020625  8.7263 < 2.2e-16 ***
#> d2014:f2015 0.175937   0.020839  8.4428 < 2.2e-16 ***
#> f2015:d2015 0.098697   0.037009  2.6668 0.0077027 ** 
#> d2014:f2016 0.184339   0.020934  8.8057 < 2.2e-16 ***
#> d2015:f2016 0.128068   0.037063  3.4554 0.0005579 ***
#> f2016:d2016 0.094358   0.053921  1.7499 0.0802449 .  
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```
