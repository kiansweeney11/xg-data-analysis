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

### name variables
team <- "Wolves"
s1_year <- "2018-19"
s2_year <- "2019-20"

## read data
file1 <- "wolves-matches-s1.csv"
dir <- "r-data/"

data = read_csv(paste0(dir,file1))

file2 <- "wolves-matches-s2.csv"

data1 = read_csv(paste0(dir,file2))

# selected columns for analysis
data_select = data %>%
  select(c(1,5,6,7,8,9,10,11))
### s2 data
data_select_s2 = data1 %>%
  select(c(1,5,6,7,8,9,10,11))

### make rolling average cols

data_xg_actual_s1 = data_select %>%
  mutate(GF_rolling_5_game_avg = movavg(GF, 5, type="s"),
         GA_rolling_5_game_avg = movavg(GA, 5, type="s"),
         Points_rolling_5_game_avg = movavg(ActualPoints, 5, type="s"),
         xP_rolling_5_game_avg = movavg(xPLeeds, 5, type="s"),
         xPOpp_rolling_5_game_avg = movavg(xPOpp, 5, type="s"),
         xG_rolling_5_game_avg = movavg(xG, 5, type="s"),
         xGA_rolling_5_game_avg = movavg(xGA, 5, type="s"))

data_xg_actual_s2 = data_select_s2 %>%
  mutate(GF_rolling_5_game_avg = movavg(GF, 5, type="s"),
         GA_rolling_5_game_avg = movavg(GA, 5, type="s"),
         Points_rolling_5_game_avg = movavg(ActualPoints, 5, type="s"),
         xP_rolling_5_game_avg = movavg(xPLeeds, 5, type="s"),
         xPOpp_rolling_5_game_avg = movavg(xPOpp, 5, type="s"),
         xG_rolling_5_game_avg = movavg(xG, 5, type="s"),
         xGA_rolling_5_game_avg = movavg(xGA, 5, type="s"))

data_select = data_xg_actual_s1 %>%
  select(c(1,8,9,10,11,12,13,14))
### s2 data
data_select_s2 = data_xg_actual_s2 %>%
  select(c(1,8,9,10,11,12,13,14))

#data_select$GoalsDiff <- (data_select$GF_rolling_5_game_avg - data_select$xG_rolling_5_game_avg)
#data_select
######

# rolling avg
# use pracma library (pracma) as this doesnt allow for zero values
# zeros would occur at first 2 and last 2 instances otherwise

### GF VS xG S1 and S2

plot_against_xG = data_select %>%
  ggplot(aes(x = Date, GF = GF_rolling_5_game_avg, xG = xG_rolling_5_game_avg)) +
  geom_line(aes(y = GF_rolling_5_game_avg, colour="GF")) +
  geom_line(aes(y = xG_rolling_5_game_avg, colour="xG")) +
  ggtitle("Season 1") +
  ylab("Goals") +
  scale_color_manual(values = c(GF = "blue", xG = "red"),
                     labels = c(GF = "GF", xG = "xG"))

plt11 <- ggplotly(plot_against_xG, tooltip = c("x", "GF", "xG"))

## season 2

plot_against_xG_s2 = data_select_s2 %>%
  ggplot(aes(x = Date, GF = GF_rolling_5_game_avg, xG = xG_rolling_5_game_avg)) +
  geom_line(aes(y = GF_rolling_5_game_avg, colour="GF")) +
  geom_line(aes(y = xG_rolling_5_game_avg, colour="xG")) +
  ggtitle("Season 2") +
  ylab("Goals") + 
  scale_color_manual(values = c(GF = "blue", xG = "red"),
                     labels = c(GF = "GF", xG = "xG"))

plt22 <- ggplotly(plot_against_xG_s2, tooltip = c("x", "GF", "xG"))

fig2 <- subplot(plt11, plt22) %>% 
  layout(title = paste0(team,' S1 VS S2 Actual VS XG rolling Averages Comparison'))
fig2

## S1 VS S2 - 5 game average xP vs xPOpp
# s1
plot_against_xP = data_select %>%
  ggplot(aes(x = Date, xP = xP_rolling_5_game_avg, xPOpp = xPOpp_rolling_5_game_avg)) +
  geom_line(aes(y = xP_rolling_5_game_avg, colour="xP")) +
  geom_line(aes(y = xPOpp_rolling_5_game_avg, colour="xPOpp")) +
  ggtitle("Season 1") +
  ylab("Points") + 
  scale_color_manual(values = c(xP = "blue", xPOpp = "red"),
                     labels = c(xP = "forestgreen", xPOpp = "orangered1"))

plt1 <- ggplotly(plot_against_xP, tooltip = c("x", "xP", "xPOpp"))

## season 2

plot_against_xP_s2 = data_select_s2 %>%
  ggplot(aes(x = Date, xP = xP_rolling_5_game_avg, xPOpp = xPOpp_rolling_5_game_avg)) +
  geom_line(aes(y = xP_rolling_5_game_avg, colour="xP")) +
  geom_line(aes(y = xPOpp_rolling_5_game_avg, colour="xPOpp")) +
  ggtitle("Season 2") +
  ylab("Points") + 
  scale_color_manual(values = c(xP = "blue", xPOpp = "red"),
                     labels = c(xP = "Points For", xPOpp = "xPOpp"))

plt2 <- ggplotly(plot_against_xP_s2, tooltip = c("x", "xP", "xPOpp"))

fig1 <- subplot(plt1, plt2) %>% 
  layout(title = paste0(team,' S1 VS S2 xP/xPOpp rolling Averages Comparison'))
fig1


### S1 VS S2 - 5 game average xGA vs Actual conceded

plot_against_xGA = data_xg_actual_s1 %>%
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
  layout(title = paste0(team,' S1 VS S2 xGA/GA rolling Averages Comparison'))
fig


## S1 VS S2 - 5 game average - Actual points VS xP
# s1
plot_against_actualpts = data_select %>%
  ggplot(aes(x = Date, xP = xP_rolling_5_game_avg, ActualPoints = Points_rolling_5_game_avg)) +
  geom_line(aes(y = xP_rolling_5_game_avg, colour="xP")) +
  geom_line(aes(y = Points_rolling_5_game_avg, colour="ActualPoints")) +
  ggtitle("Season 1") +
  ylab("Points") + 
  scale_color_manual(values = c(xP = "red", ActualPoints = "blue"),
                     labels = c(xP = "ExpectedPoints", ActualPoints = "ActualPoints"))

plt6 <- ggplotly(plot_against_actualpts, tooltip = c("x", "xP", "ActualPoints"))

## season 2

plot_against_actualpts_s2 = data_select_s2 %>%
  ggplot(aes(x = Date, xP = xP_rolling_5_game_avg, ActualPoints = Points_rolling_5_game_avg)) +
  geom_line(aes(y = xP_rolling_5_game_avg, colour="xP")) +
  geom_line(aes(y = Points_rolling_5_game_avg, colour="ActualPoints")) +
  ggtitle("Season 2") +
  ylab("Points") + 
  scale_color_manual(values = c(xP = "red", ActualPoints = "blue"),
                     labels = c(xP = "ExpectedPoints", ActualPoints = "ActualPoints"))

plt7 <- ggplotly(plot_against_actualpts_s2, tooltip = c("x", "xP", "ActualPoints"))

fig5 <- subplot(plt6, plt7) %>% 
  layout(title = paste0(team,' S1 VS S2 xP VS Actual Points rolling Averages Comparison'))
fig5

