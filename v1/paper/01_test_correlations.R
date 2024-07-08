#' @name test_correlations.R
#' @author Tim Fraser
#' @description
#' Script to test strength of correlations
#' between pollutants in US counties

# setup ####################################
library(dplyr)
library(tidyr)
library(readr)
library(ggplot2)

setwd(paste0(rstudioapi::getActiveProject(), "/v1"))

# table_A1_correlations ####################################

data = read_rds("data_raw/catserver_stats.rds")

cdata = data %>%
  filter(year == 2020) %>%
  mutate(across(e2:e110, .fns = ~log(.x + 1))) %>%
  select(any_of(c(CO2 = "e98", CO = "e2",  
                  `PM2.5` = "e110", `PM10` = "e100", 
                  NO2 = "e33", SO2 = "e31", NOx = "e3",  VOC = "e87")) ) %>% 
  cor() %>%
  as_tibble() %>%
  mutate(from = names(.))  %>%
  mutate(across(c(c(everything(), -from)), .fns = ~round(.x, 2))) %>%
  write_csv("paper/table_A1_correlations.csv")

data %>%
  filter(year == 2020) %>%
  count()
  # VOC = 87
  # 110 = PM2.5
  # 100 = PM10
  # 2 = CO
  # 98 = CO2
  # 3 = NOx
  # 33 = NO2 
  # 31 = SO2
# viz = cdata %>%
#   pivot_longer(
#     cols = c(contains("e")), 
#     names_to = "to", 
#     values_to = "r")
# 
# # This is hilarious figure. Just print it as a table.
# ggplot() +
#   geom_tile(
#     data = viz, 
#     mapping = aes(x = from, y = to, fill = r)) +
#   shadowtext::geom_shadowtext(
#     data = viz, 
#     mapping = aes(x = from, y = to, label = round(r, 2) ), 
#     bg.r = 0.1, bg.color = "#373737", color = "white") +
#   scale_fill_gradient2(low = "#648FFF", high = "#DC267F", mid = "white", midpoint = 0)
#   
rm(list = ls()); gc()


# correlation_vars.rds ####################################
# Evaluate strength of relationship between vmt and emissions
# across counties

library(dplyr)
library(readr)
library(purrr)
library(broom)

#setwd(paste0(rstudioapi::getActiveProject(), "/v1"))
#setwd("optimizer")

# Let's write a quick function for extracting model summary statistics
get_stats = function(y, x, constant = 1){
  m = lm(formula = log(y + constant) ~ log(x) - 1)
  # Get the model statistics
  output = broom::glance(m);
  # Capture the beta coefficient too
  output$beta = m$coefficients
  return(output)
}

# Get a set of strata id
strata = read_rds("data_raw/catserver_stats_with_vmt.rds") %>%
  select(geoid, pollutant) %>%
  distinct() %>%
  mutate(group = 1:n())

# Add a group id into data
data = read_rds("data_raw/catserver_stats_with_vmt.rds") %>%
  left_join(
    by = c("geoid", "pollutant"), 
    y = strata, 
    multiple = "all")

# Small example of what we are about to do.
# data %>%
#   filter(group == 1) %>%
#   lm(formula = log(vmt + 1) ~ log(emissions) - 1) %>%
#   with(coefficients)

# This will take 15~30 seconds
stat_vmt = data %>%
  #filter(group %in% 1:1000) %>%
  split(.$group) %>%
  map(.f = possibly(~get_stats(y = .$vmt, x = .$emissions), otherwise = NULL), 
          .id = "group") %>%
  bind_rows(.id = "group") 

cat("\nModeling vmt~emissions complete...")

# This will take 15~30 seconds
stat_ef = data %>%
  mutate(ef = emissions / vmt) %>%
  #filter(group %in% 1:1000) %>%
  split(.$group) %>%
  map(.f = possibly(~get_stats(y = .$ef, x = .$emissions, constant = 0.001), otherwise = NULL), 
      .id = "group") %>%
  bind_rows(.id = "group") 

cat("\nModeling EF~emissions complete...")

bind_rows(
  stat_vmt %>% mutate(type = "vmt"),
  stat_ef %>% mutate(type = "ef")
) %>%
  mutate(group = as.integer(group)) %>%
  left_join(by = "group", y = strata) %>%
  saveRDS("data_raw/correlations_vars.rds")

# Clean up
rm(list = ls())
# Is this a reasonable assumption? 
# How correlated *are* emissions and vmt, or emissions and emissions factors?

# Let's get some good summary statistics.
stats = read_rds("data_raw/correlations_vars.rds") %>%
  mutate(pollutant = pollutant %>% recode_factor(
    "98" = "CO2", "2" = "CO",  
      "110" = "PM2.5", "100" = "PM10", 
      "33" = "NO2", "31" = "SO2", "3" = "NOx",  "87" = "VOC")) %>%
  mutate(type = type %>% recode_factor(
    "vmt" = "VMT",
    "ef" = "Emissions Factor"
  ))


# table_A3 ####################################  
stats %>%
  # For each pollutant, do these variables correlate appropriately?
  group_by(type, pollutant) %>%
  # Yes. Dramatically so. Pollutant 31 and PM10 (100) are big outliers.
  summarize( 
    n = n(),
    q0 = quantile(r.squared, probs = 0, na.rm = TRUE),
    q25 = quantile(r.squared, probs = 0.25, na.rm = TRUE),
    q50 = median(r.squared),
    q75 = quantile(r.squared, probs = 0.75, na.rm = TRUE),
    q100 = quantile(r.squared, probs = 1, na.rm = TRUE)
  ) %>%
  mutate(across(contains("q"), .fns = ~round(.x, 2))) %>%
  write_csv("paper/table_A3.csv")

# table_A4 ####################################
# Using this mondo-sample, let's calculate some solid QIs
stats %>%
  group_by(type, pollutant) %>%
  summarize(
    estimate = mean(beta, na.rm = TRUE),
    se = sd(beta, na.rm = TRUE) / sqrt(n()),
    lower = quantile(beta, probs = 0.025, na.rm = TRUE),
    upper = quantile(beta, probs = 0.975, na.rm = TRUE),
    n = n()
  ) %>%
  mutate(across(c("estimate", "se", "lower", "upper"), .fns = ~round(.x, 3))) %>%
  write_csv("paper/table_A4.csv")
  
# clean up!
rm(list = ls()); gc()
  



