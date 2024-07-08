#' @name 02_stats_in_paper.R
#' @author Tim Fraser
#' @description

# This script calculates quantities of interest for a state run of scenarios.

library(dplyr)
library(readr)
library(scales)
rm(list = ls())
setwd(paste0(rstudioapi::getActiveProject(), "/v1"))
devtools::load_all(".")

# State statistics ##############
read_csv("paper/state_run_1.csv") %>%
  mutate(geoid = stringr::str_pad(geoid, width = 5, pad= "0", side = "left"))  %>%
  report_stats(pollutant = 98) %>%
  write_csv("paper/stats_in_paper.csv")

read_csv("paper/stats_in_paper.csv")

# Bencharking #####################
result = read_csv("paper/benchmarking.csv")

# Total test runtime
# result = read_csv("paper/benchmarking.csv") %>%
#   summarize(seconds = sum(seconds))

# Average runtime
result %>%
  lm(formula = seconds ~ 1)

result %>%
  summarize(mean = mean(seconds),
            sd = sd(seconds),
            se = sd(seconds) / sqrt(n()))

# Estimate effect of year
result %>%
  lm(formula = seconds ~ end_year) %>% 
  broom::tidy()

# result %>%
#   group_by(end_year) %>%
#   summarize(mean = mean(seconds),
#             sd = sd(seconds),
#             se = sd(seconds) / sqrt(n())) %>%
#   lm(formula = mean ~ end_year) %>%
#   broom::tidy()

result %>%
  lm(formula = seconds ~ n_policies) %>%
  broom::tidy()
# read_csv("paper/benchmarking.csv") %>%
#   group_by(n_policies) %>%
#   summarize(mean = mean(seconds),
#             sd = sd(seconds),
#             se = sd(seconds) / sqrt(n())) %>%
#   lm(formula = mean ~ n_policies)  %>%
#   broom::tidy()

result %>%
  lm(formula = seconds ~ I(max_abs_diff * 100) ) %>%
  broom::tidy()

result %>%
  group_by(max_abs_diff) %>%
  summarize(mean = mean(seconds),
            sd = sd(seconds),
            se = sd(seconds) / sqrt(n())) 

result %>%
  lm(formula = seconds ~ factor(geoid) ) %>%
  broom::tidy()


result %>%
  group_by(pollutants) %>%
  summarize(  mean = mean(seconds),
              sd = sd(seconds),
              se = sd(seconds) / sqrt(n())) 
result %>%
  lm(formula = seconds ~ factor(pollutants)) %>%
  broom::tidy()
  
read_csv("paper/benchmarking.csv")
seq(from =2025, to = 2050, by =1) %>% length()