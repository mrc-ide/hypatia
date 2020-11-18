
<!-- README.md is generated from README.Rmd. Please edit that file -->

# hypatia

<!-- badges: start -->

[![R build
status](https://github.com/mrc-ide/hypatia/workflows/R-CMD-check/badge.svg)](https://github.com/mrc-ide/hypatia/actions)
[![CodeFactor](https://www.codefactor.io/repository/github/mrc-ide/hypatia/badge)](https://www.codefactor.io/repository/github/mrc-ide/hypatia)
[![codecov.io](https://codecov.io/github/mrc-ide/hypatia/coverage.svg?branch=main)](https://codecov.io/github/mrc-ide/hypatia?branch=main)
<!-- badges: end -->

The goal of hypatia is to enable SQUIRE to be run on an individual basis
rather than aggregate

## Installation

You can install the released version of hypatia from
[CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("hypatia")
devtools::install_github("ggplot2")
```

And the development version from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("mrc-ide/hypatia")
```

and packages:

``` r
options(warn = - 1) 
library('remotes')
install_github('mrc-ide/individual')
library(individual)
library(ggplot2)
library(reshape2)
```

## Example

This is an example showing you an hypatia run for Afghanistan on all 22
states but with a simple model for the infection

<img src="man/figures/README-hypatia_example-1.png" width="100%" />

## License

MIT © Imperial College of Science, Technology and Medicine
