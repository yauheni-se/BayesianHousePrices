#https://archive.ics.uci.edu/ml/datasets/Real+estate+valuation+data+set

library(dplyr)
library(stringr)
library(magrittr)
library(readxl)
library(readr)

add_leading_zero <- function(x) {ifelse(x %in% 1:9, paste0("0", x), as.character(x))}

data_cl <- read_excel("data/data.xlsx") %>% 
  select(-No) %>% 
  `colnames<-`(c("date", "age", "dist_to_mrt", "no_stores", "long", "lat", "price")) %>% 
  mutate(
    date = as.character(date),
    year = vapply(str_split(.$date, "\\."), function(x) {x[1]}, character(1)),
    month = vapply(str_split(.$date, "\\."), function(x) {paste0("0.", x[2])}, character(1)),
    month = 12*as.numeric(month),
    month = ifelse(is.na(month), 12, month),
    month = round(month, 0)
  ) %>% 
  arrange(year, month) %>% 
  mutate(
    month = vapply(month, add_leading_zero, character(1)),
    year_month = paste0(year, "-", month)
  ) %>% 
  select(-date, -long, -lat, -month, -year) %>% 
  select(year_month, price, everything())
write_csv(data_cl, "data/data_cleared.csv")