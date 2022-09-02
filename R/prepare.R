#' Prepare the ready-to-use M5 data in one data.frame
#'
#' @param sales_train A data.frame with M5 train data
#' @param sales_test A data.frame with M5 test data
#' @param calendar A data.frame with M5 calendar
#' @param sell_prices A data.frame with M5 sell_prices
#'
#' @examples
#' library(m5)
#' library(zeallot)
#' \dontrun{
#' m5_download('data')
#' c(sales_train,
#'   sales_test,
#'   sell_prices,
#'   calendar) %<-% m5_get_raw_evaluation('data')
#'
#'
#' m5_data  <-
#'    m5_prepare(sales_train, sales_test, calendar, sell_prices)
#' }
#'
#' @import dplyr lubridate stringi data.table
#' @export
m5_prepare <- function(sales_train, sales_test,
                       calendar, sell_prices){

  sales_train_long <- sales_train %>%
    tidyr::pivot_longer(cols = starts_with("d_"))

  sales_test_long <- sales_test %>%
    tidyr::pivot_longer(cols = starts_with("d_"))

  setnames(sales_train_long, "name", "d")
  setnames(sales_test_long, "name", "d")
  setDT(sales_train_long)
  setDT(sales_test_long)

  sales_train_long[, d := as.integer(d)]
  sales_test_long[, d := as.integer(d)]

  # Prepare calendar
  calendar <-
    calendar %>%
    mutate(date = lubridate::as_date(date)) %>%
    tidyr::pivot_longer(starts_with("snap")) %>%
    rename(state_id = name,  snap = value) %>%
    mutate(across(where(is.factor), as.character)) %>%
    mutate(state_id = stringi::stri_sub(state_id, 6)) %>%
    mutate(d = 1:n())

  # Prepare sales
  sales <-
    bind_rows(
      sales_train_long,
      sales_test_long
    )%>%
    mutate(across(where(is.factor), as.character))

  rm(sales_test_long)
  rm(sales_train_long)

  m5_data <-
    sales  %>%
    left_join(calendar, by = c("d", "state_id")) %>%
    left_join(sell_prices, by = c("store_id", "item_id", "wm_yr_wk")) %>%
    select(-d)

  m5_data %>%
    select(item_id, dept_id, cat_id, store_id,
           state_id, date, value, everything())
}
