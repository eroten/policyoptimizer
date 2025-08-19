#' @name workflow.R
#' @title Installation and Workflow Script for `policyoptimizer` package functions
#' @description Script for test install of `policyoptimizer` package functions.

# Set working directory anywhere
# setwd(rstudioapi::getActiveProject())

# You can download the package from github using the remotes package.
# Be sure to request to install all dependencies.
# install.packages("remotes")
# remotes::install_github(repo = "Gao-Labs/policyoptimizer/v1", dependencies = "always")

# If you have downloaded the repository, then you can install it from source:
# install.packages("v1/policyoptimizer_1.0.tar.gz", type = "source")

# Load policy optimizer package
library(policyoptimizer)

# Having any issues? These are the main packages that are dependencies.
# They should have installed by default, but just in case, here they are.
# library(dplyr)
# library(httr)
# library(tidyr)
# library(readr)
# library(purrr)
# library(stringr)
# library(viridis)
# library(ggplot2)
# library(ROI, warn.conflicts = FALSE, quietly = TRUE)
# library(ROI.plugin.glpk, warn.conflicts = FALSE, quietly = TRUE)
# library(ompr, warn.conflicts = FALSE, quietly = TRUE)
# library(ompr.roi, warn.conflicts = FALSE, quietly = TRUE)

# First, let's check and make sure that CATSERVER is on and running.
# If you get a response back that says `online = TRUE`, we're in business.
# Remember that sometiems CATSERVER is either (1) down for maintenance or (2) down during nights and weekends.
# We aim to increase access to CATSERVER as usage increases.
check_status()


# Run optimizer
output <- policy_optimizer(
  # Basic Metadata
  geoid = "27053",
  pollutant = 98, 
  start_year = 2015, end_year = 2035,
  # policies = 1:10,
  n_scales = 100, units = 30000, max_abs_diff = 1, 
  min_annual_cost = 10000, wr = 0.5, wc = 0.5,
  # Cost Effectiveness Stats
  var = "epd", # measure cost effectiveness as tons of emissions per dollar
  prob = 0.50, # median cost effectiveness
  range = 5, # take five year average
  # Any prior cumulative costs / reductions
  last_cost = 0, last_reduction98 = 0, target_year = 2005, target_change = 0.50
)


# Were all runs a success?
unique(output$status)


# View output
output

# Columns 1-10 (or n policies) show adoption levels, from 0 to the max level of adoption, n_scales
# Check status of run with 'success' column.
# Key annual metrics include:
# - cost (rescaled as icost)
# - reduction98 (rescaled as ireduction98)
# - z
# - diversification

# 98 refers to the EPA pollutant code for CO2 equivalent emissions

# Report key statistics
report_stats(output, pollutant = 98)

# Get summary tables for visuals
tab_policy <- summarize_by_policy(output)
tab_year <- summarize_by_year(output)

# Visualize key impacts by policy
visualize_by_policy(data = tab_policy, var = "effect_cost")
visualize_by_policy(data = tab_policy, var = "effect_emissions98")
visualize_by_policy(data = tab_policy, var = "effect_vmt")
visualize_by_policy(data = tab_policy, var = "effect_ef98")

# Visualize key outcomes over time
visualize_by_year(data = tab_year, var = "cost")
visualize_by_year(data = tab_year, var = "emissions98")
visualize_by_year(data = tab_year, var = "vmt")
visualize_by_year(data = tab_year, var = "ef98")



# Multi-Pollutant Usage (experimental - still under development) - takes a lot longer
output2 <- policy_optimizer(
  geoid = "27053", pollutant = c(98, 2, 3), start_year = 2025, end_year = 2050, policies = 1:10,
  n_scales = 100, units = 30000, max_abs_diff = 1, min_annual_cost = 10000, wr = 0.5, wc = 0.5,
  # Cost Effectiveness Stats
  var = "epd", prob = 0.50, range = 5,
  # Any prior cumulative costs / reductions
  last_cost = 0, last_reduction98 = 0, target_year = 2005, target_change = 0.50
)

# Did they work out okay?
unique(output2$status)

# View output
output2

# Columns 1-10 (or n policies) show adoption levels, from 0 to the max level of adoption, n_scales
# Check status of run with 'success' column.
# Key annual metrics include:
# - cost (rescaled as icost)
# - reduction98 (rescaled as ireduction...)
# - reduction3 (rescaled as ireduction...)
# - reduction2 (rescaled as ireduction...)
# - reduction87 (rescaled as ireduction...)
# - z
# - diversification

# 98 refers to the EPA pollutant code for CO2e - carbon dioxide equivalent emissions
# 2 refers to the EPA pollutant code for CO - Carbon Monoxide
# 3 refers to the EPA pollutant code for NOx - Oxides of Nitrogren

report_stats(output2, pollutant = 98)
report_stats(output2, pollutant = 2)
report_stats(output2, pollutant = 3)


# Get summarized data for visualization
tab_policy2 <- summarize_by_policy(output2)
tab_year2 <- summarize_by_year(output2)


# Visualize key impacts by policy
visualize_by_policy(data = tab_policy2, var = "effect_cost")
visualize_by_policy(data = tab_policy2, var = "effect_emissions98")
visualize_by_policy(data = tab_policy2, var = "effect_emissions2")
visualize_by_policy(data = tab_policy2, var = "effect_emissions3")
visualize_by_policy(data = tab_policy2, var = "effect_vmt")
visualize_by_policy(data = tab_policy2, var = "effect_ef98")
visualize_by_policy(data = tab_policy2, var = "effect_ef2")
visualize_by_policy(data = tab_policy2, var = "effect_ef3")

# Visualize key outcomes over time
visualize_by_year(data = tab_year2, var = "cost")
visualize_by_year(data = tab_year2, var = "emissions98")
visualize_by_year(data = tab_year2, var = "emissions2")
visualize_by_year(data = tab_year2, var = "emissions3")
visualize_by_year(data = tab_year2, var = "vmt")
visualize_by_year(data = tab_year2, var = "ef98")
visualize_by_year(data = tab_year2, var = "ef2")
visualize_by_year(data = tab_year2, var = "ef3")



# Clean up
rm(list = ls())
gc()
