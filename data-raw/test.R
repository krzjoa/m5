library(dplyr)
library(data.table)
library(m5)
library(zeallot)

m5_download('data-test')

c(sales_train,
  sales_test,
  sell_prices,
  calendar,
  weights) %<-% m5_get_raw_evaluation('data-test')


m5_data <- m5_prepare(
  sales_train, sales_test, calendar, sell_prices
)


m5_demand <- m5_demand_type(m5_data)



.cols <- colnames(sales_train)[6:length(colnames(sales_train))]
sales_train_long <- melt(sales_train, measure.vars=.cols)

.cols <- colnames(sales_test)[6:length(colnames(sales_test))]
sales_test_long  <- melt(sales_test, measure.vars=.cols)

setDT(sales_train_long)
setDT(sales_test_long)

sales_train_long[, variable := as.integer(variable)]
sales_test_long[, variable := as.integer(variable)]

# Prepare calendar
calendar[, date := lubridate::as_date(date)]
.cols <- colnames(calendar)[startsWith(colnames(calendar), 'snap')]
calendar <- melt(calendar, measure.vars=.cols)
calendar[, variable := stringi::stri_sub(variable, 6)]
setnames(calendar, 'variable', 'state_id')
setnames(calendar, 'value', 'snap')
calendar[, d := 1:.N, by=state_id]

# Prepare sales
m5_data <-
  rbindlist(list(
    sales_train_long,
    sales_test_long
  ))
setnames(m5_data, 'variable', 'd')

rm(sales_train_long)
rm(sales_test_long)
gc()

m5_data[calendar, on = .(d, state_id),
        `:=`(wm_yr_wk=wm_yr_wk,
             weekday=weekday,
             wday=wday,
             month=month,
             year=year,
             event_name_1=event_name_1,
             event_type_1=event_type_1,
             event_name_2=event_name_2,
             event_type_2=event_type_2,
             state_id=state_id,
             snap=snap)]
gc()
m5_data[sell_prices, on = .(store_id, item_id, wm_yr_wk),
        `:=`(sell_price=sell_price)]
gc()

# Optimize
# https://stackoverflow.com/questions/34598139/left-join-using-data-table
m5_data <- merge(m5_data, calendar, by=c("d", "state_id"), all.x = TRUE)
gc()
m5_data <-
  merge(m5_data, sell_prices,
        by=c("store_id", "item_id", "wm_yr_wk"),
        all.x = TRUE)
gc()

m5_data <-
  sales  %>%
  left_join(calendar, by = c("store_id", "item_id", "wm_yr_wk")) %>%
  left_join(sell_prices, by = c("store_id", "item_id", "wm_yr_wk")) %>%
  select(-d)

m5_data %>%
  select(item_id, dept_id, cat_id, store_id,
         state_id, date, value, everything())
