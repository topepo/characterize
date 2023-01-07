
<!-- README.md is generated from README.Rmd. Please edit that file -->

# characterize

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![CRAN
status](https://www.r-pkg.org/badges/version/characterize)](https://CRAN.R-project.org/package=characterize)
[![Codecov test
coverage](https://codecov.io/gh/topepo/characterize/branch/main/graph/badge.svg)](https://app.codecov.io/gh/topepo/characterize?branch=main)
[![R-CMD-check](https://github.com/topepo/characterize/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/topepo/characterize/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

The goal of characterize is to report data driven characteristics of a
model fit that describe the model. For example:

- the number of predictors used,
- the number of terminal nodes,

and so on. These are interesting characteristics of the model that often
reflect complexity of the model.

## Installation

You can install the development version of characterize like so:

``` r
require(remotes)
remotes::install_github("topepo/characterize")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(characterize)
library(rpart)

cart_mod <- rpart(mpg ~ ., data = mtcars)
cart_mod
#> n= 32 
#> 
#> node), split, n, deviance, yval
#>       * denotes terminal node
#> 
#> 1) root 32 1126.04700 20.09062  
#>   2) cyl>=5 21  198.47240 16.64762  
#>     4) hp>=192.5 7   28.82857 13.41429 *
#>     5) hp< 192.5 14   59.87214 18.26429 *
#>   3) cyl< 5 11  203.38550 26.66364 *

characterize(cart_mod)
#> # A tibble: 2 × 3
#>   .metric             .estimator .estimate
#>   <chr>               <chr>          <dbl>
#> 1 num_active_features model              2
#> 2 num_term_nodes      model              3

# Also some low-level functions: 
.pluck_active_features(cart_mod) %>% 
  tidyr::unnest(value)
#> # A tibble: 2 × 2
#>   statistic       value
#>   <chr>           <chr>
#> 1 active_features cyl  
#> 2 active_features hp
```

Objects with at least one method: `bagger`, `bart`, `brulee_mlp`,
`C5.0`, `cforest`, `cubist`, `earth`, `glmnet`, `ksvm`, `lgb.Booster`,
`mixo_pls`, `mixo_plsda`, `mixo_spls`, `mixo_splsda`, `multinom`,
`nnet`, `party`, `partynode`, `ranger`, `rpart`, `terms`, `xgb.Booster`,
`xrf`.

## Code of Conduct

Please note that the characterize project is released with a
[Contributor Code of
Conduct](https://contributor-covenant.org/version/2/0/CODE_OF_CONDUCT.html).
By contributing to this project, you agree to abide by its terms.
