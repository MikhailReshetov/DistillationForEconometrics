library(tidyverse)
library(seasonal)

# growth rates
growth_rate <- function (data) {
  for (col in colnames(data)[2:length(colnames(data))]) {
    # extracting actual series for growth rate
    series <- data %>%
      select(date, col) %>%
      drop_na(col)
   
    series <- series %>%
      mutate(!!sym(col) := !!sym(col) / lag(!!sym(col)) - 1)
    
    data <- data %>%
      select(-col) %>%
      left_join(series, by="date")
  }
  return (data)
}

# reading data
read_delim("russia_check/data/russian_data_raw.csv", delim = ",")
data <- read_csv("russia_check/data/russian_data_raw.csv")
data$date = as.Date(data$date)
data$oil_price = as.numeric(data$oil_price)

# decomposing data (making SA), using X-13 Arima Seats
colnames(data)[2:length(colnames(data))]
# ibved - monthly, 1 lag
seasonally_adjusted = seas(ts(data$ibved, start = 2004, frequency = 12))
data$ibved = c(seasonally_adjusted$series$s11, NA)
plot(seasonally_adjusted)
# cpi - monthly, 0 lag
seasonally_adjusted = seas(ts(data$cpi, start = 2004, frequency = 12))
data$cpi = seasonally_adjusted$series$s11
plot(seasonally_adjusted)
# oil_price - monthly, 0 lag
seasonally_adjusted = seas(ts(data$oil_price, start = 2004, frequency = 12))
data$oil_price = seasonally_adjusted$series$s11
plot(seasonally_adjusted)
# rate - monthly, 0 lag
# we do not change rate
seasonally_adjusted = seas(ts(data$rate, start = 2004, frequency = 12))
plot(seasonally_adjusted)
# exchange_rate - monthly, 0 lag
seasonally_adjusted = seas(ts(data$exchange_rate, start = 2004, frequency = 12))
data$exchange_rate = seasonally_adjusted$series$s11
plot(seasonally_adjusted)
# m2 - monthly, 0 lag
seasonally_adjusted = seas(ts(data$m2, start = 2004, frequency = 12))
data$m2 = seasonally_adjusted$series$s11
plot(seasonally_adjusted)
# us_cpi - monthly, 0 lag
seasonally_adjusted = seas(ts(data$us_cpi, start = 2004, frequency = 12))
data$us_cpi = seasonally_adjusted$series$s11
plot(seasonally_adjusted)
# PMI - monthly, 0 lag, since 2011
seasonally_adjusted = seas(ts(data$PMI, start = 2011, frequency = 12))
data$PMI = c(rep(NA, 7*12), seasonally_adjusted$series$s11)
plot(seasonally_adjusted)
# unemployment - monthly, 1 lag
seasonally_adjusted = seas(ts(data$unemployment, start = 2004, frequency = 12))
data$unemployment = c(seasonally_adjusted$series$s11, NA)
plot(seasonally_adjusted)
# gdp - quarterly - 
seasonally_adjusted = seas(ts(na.omit(data$gdp), start = 2004, frequency = 4))
pure_GDP = seasonally_adjusted$series$s11
for (i in 1:length(pure_GDP)){
  data$gdp[(i-1)*3+1] = NA
  data$gdp[(i-1)*3+2] = NA
  data$gdp[(i-1)*3+3] = pure_GDP[i]
}
plot(seasonally_adjusted)

# writing SA data
write_csv(data, "russia_check/data/russian_data_raw_SA.csv")

# taking growth rates
data = data %>%
  arrange(date) %>%
  growth_rate()


write_csv(data, "russia_check/data/russian_data_tf.csv")
