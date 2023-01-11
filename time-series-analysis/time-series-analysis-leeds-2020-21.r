## necessary libraries
library(ggsoccer)
library(here)
library(tidyverse)
library(ggplot2)
library(zoo)
library(pracma)
library(RcppRoll)
library(lubridate)
library(readr)

## read data
data = read_csv("r-data/less-headers.csv")

# selected columns for analysis
data_select = data %>%
  select(c(1,5,6,7,8,9,10))

# rolling avg
data_rcpp = data_select %>%
  mutate(xG_rolling_5_game_avg = roll_mean(xG, 5, fill = 0),
         xGA_rolling_5_game_avg = roll_mean(xGA, 5, fill = 0))

plot_rcpp = data_rcpp %>%
  ggplot(aes(x = Date)) +
  geom_line(aes(y = xG_rolling_5_game_avg), colour="blue") +
  geom_line(aes(y = xGA_rolling_5_game_avg), colour="red")

plot_rcpp

