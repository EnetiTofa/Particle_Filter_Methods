# R/data_processing.R
library(readxl)
library(tidyverse)
library(zoo)

load_and_prep_exchange_data <- function(filepath) {
  # Read raw excel file
  exchange_data <- read_excel(filepath, skip = 4)
  
  # Select, rename, and compute log-returns
  exchange_usd_data <- exchange_data %>%
    select("Series Id", "EXR.DS11.D06") %>%
    rename(
      Date = "Series Id",
      NZD_USD = "EXR.DS11.D06"
    ) %>%
    mutate(
      NZD_USD = as.numeric(NZD_USD),
      y_t = 100 * log(NZD_USD / lag(NZD_USD)),
      Date = as.Date(Date)
    ) %>%
    drop_na(y_t)
  
  return(exchange_usd_data)
}