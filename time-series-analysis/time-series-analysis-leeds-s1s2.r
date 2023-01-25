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
library(plotly)
library(gridExtra)
library(patchwork)

## read data
file1 <- "less-headers.csv"
dir <- "r-data/"

data = read_csv(paste0(dir,file1))

file2 <- "less-headers-2122-lee.csv"

data1 = read_csv(paste0(dir,file2))

# selected columns for analysis
data_select = data %>%
  select(c(1,5,6,7,8,9,10))
### s2 data
data_select_s2 = data1 %>%
  select(c(1,5,6,7,8,9,10))

data_rcpp_s2 = data_select_s2 %>%
  mutate(xG_rolling_5_game_avg = movavg(xG, 5, type="s"),
         xGA_rolling_5_game_avg = movavg(xGA, 5, type="s"))

data_xg_actual_s2 = data_rcpp_s2 %>%
  mutate(GF_rolling_5_game_avg = movavg(GF, 5, type="s"),
         GA_rolling_5_game_avg = movavg(GA, 5, type="s"))

######

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
  ggplot(aes(x = Date, GA = GA_rolling_5_game_avg, xGA = xGA_rolling_5_game_avg)) +
  geom_line(aes(y = GA_rolling_5_game_avg, colour="GA")) +
  geom_line(aes(y = xGA_rolling_5_game_avg, colour="xGA")) +
  ggtitle("5 Point MA Leeds United xGA VS GA 2020/21") +
  ylab("Goals") + 
  scale_color_manual(values = c(GA = "blue", xGA = "red"),
                     labels = c(GA = "Goals Against", xGA = "xGA"))

plt1 <- ggplotly(plot_against_xGA, tooltip = c("x", "GA", "xGA"))

## season 2

plot_against_xGA_s2 = data_xg_actual_s2 %>%
  ggplot(aes(x = Date, GA = GA_rolling_5_game_avg, xGA = xGA_rolling_5_game_avg)) +
  geom_line(aes(y = GA_rolling_5_game_avg, colour="GA")) +
  geom_line(aes(y = xGA_rolling_5_game_avg, colour="xGA")) +
  ggtitle("5 Point MA Leeds United xGA VS GA 2021/22") +
  ylab("Goals") + 
  scale_color_manual(values = c(GA = "blue", xGA = "red"),
                     labels = c(GA = "Goals Against", xGA = "xGA"))

plt2 <- ggplotly(plot_against_xGA_s2, tooltip = c("x", "GA", "xGA"))

fig <- subplot(plt1, plt2) %>% 
  layout(title = 'Leeds United S1 VS S2 xGA/GA rolling Averages Comparison')
fig



