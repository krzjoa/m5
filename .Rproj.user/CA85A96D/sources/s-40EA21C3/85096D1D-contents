# m5
> M5 Walmart Challenge Data

## Installation

For the moment, this package can be installed using:

```r
devtools::install_github("krzjoa/m5")
```

## Usage

```r
library(m5)
library(zeallot)

# Downloading the data
m5_download('sample/directory')

# Loading the data
c(sales_train,
   sales_test,
   sell_prices,
   calendar) %<-% m5_get_raw_evaluation('data')
   
# Preparing the data
> m5_data  <-
   m5_prepare(sales_train, sales_test, calendar, sell_prices)
```
