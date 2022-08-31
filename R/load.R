#' @name m5_get_raw_*
#' Load raw CSV files using data.table::fread function
#'
#' @param path The directory with the unzipped M5 data files
#'
#' @returns
#' The function returns a list of five data.tables:
#'
#' * sales_train (evaluation/validation)
#' * sales_test (evaluation/validation)
#' * sell_prices
#' * calendar
#' * weights (evaluation/validation)
#'
#' @references
#' [m5-forecasts repo by Nixtla](https://github.com/Nixtla/m5-forecasts)
#'
#' @examples
#' library(m5)
#' library(zeallot)
#'
#' m5_download('data')
#' c(sales_train,
#'   sales_test,
#'   sell_prices,
#'   calendar) %<-% m5_get_raw_evaluation('data')
NULL

#' @rdname m5_get_raw_*
#' @export
m5_get_raw_evaluation <- function(path){
  files <- c(
    'sales_train_evaluation.csv',
    'sales_test_evaluation.csv',
    'sell_prices.csv',
    'calendar.csv',
    'weights_evaluation.csv'
  )
  files <- file.path(path, files)
  Map(data.table::fread, files)
}

#' @rdname m5_get_raw_*
#' @export
m5_get_raw_validation <- function(path){
  files <- c(
    'sales_train_validation.csv',
    'sales_test_validation.csv',
    'sell_prices.csv',
    'calendar.csv',
    'weights_validation.csv'
  )
  files <- file.path(path, files)
  Map(data.table::fread, files)
}