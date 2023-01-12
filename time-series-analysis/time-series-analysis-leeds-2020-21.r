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
  geom_line(aes(y = xG_rolling_5_game_avg, colour="xG")) +
  geom_line(aes(y = xGA_rolling_5_game_avg, colour="xGA")) +
  ggtitle("5 Point MA Leeds United xG VS xGA 2020/21") +
  ylab("Goals") +
  scale_color_manual(values = c(xG = "blue", xGA = "red"),
                     labels = c(xG = "xG", xGA = "xGA"))

plot_rcpp

## same for expected points
data_xp = data_select %>%
  mutate(xP_rolling_5_game_avg = movavg(xPLeeds, 5, type="s"),
         xPOpp_rolling_5_game_avg = movavg(xPOpp, 5, type="s"))

plot_xp = data_xp %>%
  ggplot(aes(x = Date)) +
  geom_line(aes(y = xP_rolling_5_game_avg, colour="xP")) +
  geom_line(aes(y = xPOpp_rolling_5_game_avg, colour="xPOpp")) +   
  ggtitle("5 Point MA Leeds United xP VS Opponent xP 2020/21") +
  ylab("Points") +
  scale_color_manual(values = c(xP = "blue", xPOpp = "red"),
                     labels = c(xP = "ExpectedPoints", xPOpp = "xPOpp"))

plot_xp

### 5 game average xG vs Actual

data_xg_actual = data_rcpp %>%
  mutate(GF_rolling_5_game_avg = movavg(GF, 5, type="s"),
         GA_rolling_5_game_avg = movavg(GA, 5, type="s"))

plot_xg_actual = data_xg_actual %>%
  ggplot(aes(x = Date)) +
  geom_line(aes(y = GF_rolling_5_game_avg, colour="GF")) +
  geom_line(aes(y = xG_rolling_5_game_avg, colour="xG")) +
  ggtitle("5 Point MA Leeds United xG VS GF 2020/21") +
  ylab("Goals") +
  scale_color_manual(values = c(GF = "blue", xG = "red"),
                     labels = c(GF = "Goals Scored", xG = "xG"))

plot_xg_actual

### 5 game average xGA vs Actual conceded

plot_against_xGA = data_xg_actual %>%
  ggplot(aes(x = Date)) +
  geom_line(aes(y = GA_rolling_5_game_avg, colour="GA")) +
  geom_line(aes(y = xGA_rolling_5_game_avg, colour="xGA")) +
  ggtitle("5 Point MA Leeds United xGA VS GA 2020/21") +
  ylab("Goals") +
  scale_color_manual(values = c(GA = "blue", xGA = "red"),
                     labels = c(GA = "Goals Against", xGA = "xGA"))
  #theme_classic()

plot_against_xGA

