
<!-- README.md is generated from README.Rmd. Please edit that file -->

# m5 <img src='man/figures/logo-small.png' align="right" height="139" />

<!-- badges: start -->

[![CRAN
status](https://www.r-pkg.org/badges/version/m5)](https://CRAN.R-project.org/package=m5)
[![Buy hex
stciker](https://img.shields.io/badge/buy%20hex-m5-green?style=flat&logo=redbubble)](https://www.redbubble.com/i/sticker/m5-R-package-hex-by-krzjoa/122633859.EJUG5)
<!-- badges: end -->

> M5 Walmart Challenge Data

## Installation

You can install the development version of m5 from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("krzjoa/m5")
```

## Usage

``` r
library(m5)
library(zeallot)

DIR <- 'sample/directory'

# Downloading the data
m5_download(DIR)

# Loading the data
c(sales_train,
   sales_test,
   sell_prices,
   calendar) %<-% m5_get_raw_evaluation(DIR)
   
# Preparing the data
m5_data  <-
   m5_prepare(sales_train, sales_test, calendar, sell_prices)
```
