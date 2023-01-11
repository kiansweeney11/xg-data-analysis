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
# use pracma library (pracma) as this doesnt allow for zero values
# zeros would occur at first 2 and last 2 instances otherwise
data_rcpp = data_select %>%
  mutate(xG_rolling_5_game_avg = movavg(xG, 5, type="s"),
         xGA_rolling_5_game_avg = movavg(xGA, 5, type="s"))

plot_rcpp = data_rcpp %>%
  ggplot(aes(x = Date)) +
  geom_line(aes(y = xG_rolling_5_game_avg), colour="blue") +
  geom_line(aes(y = xGA_rolling_5_game_avg), colour="red")

plot_rcpp

## same for expected points
data_xp = data_select %>%
  mutate(xP_rolling_5_game_avg = movavg(xPLeeds, 5, type="s"),
         xPOpp_rolling_5_game_avg = movavg(xPOpp, 5, type="s"))

plot_xp = data_xp %>%
  ggplot(aes(x = Date)) +
  geom_line(aes(y = xP_rolling_5_game_avg), colour="blue") +
  geom_line(aes(y = xPOpp_rolling_5_game_avg), colour="red")

plot_xp

