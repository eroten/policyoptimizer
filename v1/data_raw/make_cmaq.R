#' @name make_cmaq.R
#' @description
#' Script to turn the latest CMAQ dataset into a refined dataset for policy effects


# 0. MOST RECENT CMAQ ######################################

#' @name cmaq_most_recent
#' @author Tim Fraser
#' @description
#' Function to get most recent CMAQ dataset file path
setwd(paste0(rstudioapi::getActiveProject(), "/v1"))

cmaq_most_recent = function(folder = "data_raw"){
  # Load packages
  library(dplyr)
  library(stringr)
  library(readr)
  library(lubridate)
  
  # Find all cmaq files
  f = tibble(file = dir(folder, full.names = TRUE, pattern = "cmaq_project_data")) %>%
    # Extract data from file format
    mutate(date = str_extract(file, "[0-9]{4}[_][0-9]{2}_[0-9]{2}") %>% str_replace_all("[_]", "-"),
           date = as.Date(date))
  
  # Find the most recent file and get its file path
  path = f %>%
    filter(date == max(date, na.rm = TRUE)) %>%
    with(file)
  
  return(path)
}

#path %>% read_csv()


#' @name cmaq
#' @title CMAQ Project Data Investigations
#' @author Tim Fraser
#' @description R Script for viewing and quick reformatting of CMAQ data. 

# Load core packages
library(dplyr)
library(readr)


# Load packages
library(dplyr)
library(readr)
library(tidyr)
library(purrr)
library(stringr)

# Let's generate some important metadata!


### 1. CMAQ TYPES ##########################################################

#' @name types
#' @title CMAQ Type Table
#' @author Tim Fraser
#' @description Table of original CMAQ type names and simplified code-friendly labels
#' @importFrom dplyr tribble `%>%`
#' @importFrom readr write_csv read_csv

setwd(paste0(rstudioapi::getActiveProject(), "/v1"))
# Add categories and what we rename them to here as a tibble
dplyr::tribble(
  ~id, ~original, ~short, ~simple, ~affect_vmt, ~affect_ef,
  1, "Congestion Reduction and Traffic Flow Improvements", "congestion", "Congestion/Traffic", 1, 0,
  2, "Bicycle and Pedestrian Facilities and Programs", "pedestrian", "Bicycle/Pedestrians", 1, 0,
  3, "STP/CMAQ", "stp_cmaq", "STP", 1, 0,
  4, "Ride Sharing", "ridesharing", "Ridesharing", 1, 0,
  5, "Travel Demand Management",  "travel_demand", "Travel Demand", 1, 0,
  6, "Inspection/Maintenance(I/M) Programs", "im", "Inspect/Maintenance", 0, 1,
  7, "Alternative Fuels and Vehicles", "alt_fuels", "Alt. Fuels & Vehicles", 0, 1,
  8, "Transit Improvements", "transit_improv", "Transit Improvements", 1,0,
  9, "Freight/Intermodal",   "freight_intermodal", "Freight/Intermodal", 1,1,
  10, "Advanced Diesel Truck / Engine Technologies", "diesel_engine", "Advanced Diesel", 0, 1,
  11, "Other", "other", "Other", 1, 1) %>%
  readr::write_csv("data_raw/cmaq_types.csv")

#### policies ######################################

policies_data = read_csv("data_raw/cmaq_types.csv") %>%
  mutate(id = as.integer(id))
save(policies_data, file = "data/policies_data.rda")



remove(policies_data)

# view it
#readr::read_csv("data_raw/policies_old.csv") %>% View()
#readr::read_csv("data_raw/cmaq_types.csv") %>% tail(20)


# setwd(rstudioapi::getActiveProject())
# setwd("optimizer")
# library(dplyr)
# library(readr)
# Here are all the CMAQ policies supported in the most recent estimate.
# See more details here
# https://docs.google.com/spreadsheets/d/1xrdatEmgfh-WuZbzikXU719d7SDwadtluQA3mETh9pI/edit?usp=sharing
# policies = read_csv("data_raw/policies_old.csv") %>%
#   mutate(policy_id = 1:n())

# save(policies, file = "data/policies.rda")



### 2. CMAQ VARIABLES ##########################################################

#' @name get_cmaq_variables
#' @title CMAQ Variable Table Generator
#' @author Tim Fraser
#' @description Function to get variables from a CMAQ raw dataset
#' @importFrom dplyr `%>%` bind_rows summarize across mutate everything filter
#' @importFrom readr read_csv
#' @importFrom tidyr pivot_wider pivot_longer

get_cmaq_variables = function(path = "data_raw/cmaq_project_data_2023_04_06.csv"){
  # Make a table of variables available from standard cmaq data
  readr::read_csv(file = path) %>%
    # Quick function {...} that where '.' refers to the data piped in from above
    { 
      dplyr::bind_rows(
        # Get all total available observations per variable, in one row
        dplyr::summarize(
          .data = ., 
          dplyr::across(.cols = dplyr::everything(), .fns = ~sum(!is.na(.x)))) %>% 
          dplyr::mutate(name = "available"),
        # Get all total unique observations per variabel, in one row
        dplyr::summarize(
          .data = ., 
          dplyr::across(.cols = dplyr::everything(), .fns = ~unique(.x) %>% length())) %>% 
          dplyr::mutate(name = "unique")
      ) 
    } %>%
    # Pivot to tall data.frame with original and values columns
    tidyr::pivot_longer(cols = -c(name), names_to = "original", values_to = "value") %>%
    # Pivot wider, where each original variable name gets its own row
    tidyr::pivot_wider(id_cols = c(original), names_from = name, values_from = value) %>%
    # create a nicer 'var' column that's all lower case
    dplyr::mutate(var = original %>% tolower()) %>%
    # Filter to only variables that have at least 1 non-NA value, dropping all useless column
    dplyr::filter(available > 0) %>%
    # Filter out any variables that only have 1 unique value (so are not worth keeping) 
    dplyr::filter(unique > 1) %>%
    # Return result
    return()
}


# Run function and write variables to file
cmaq_most_recent("data_raw") %>%
  get_cmaq_variables() %>% 
  readr::write_csv("data_raw/cmaq_variables.csv")


#' @name variables
#' @title CMAQ Variables Table
#' @author Tim Fraser
#' @description Table of original CMAQ variable names and simplified labels, by valid data cells
#' @importFrom readr read_csv

#' # view it!
readr::read_csv("data_raw/cmaq_variables.csv")

### 3. INFLATION ADJUSTMENT TABLE ##########################################

# You will need this table to proceed below
readr::read_csv("data_raw/cpi_inflation.csv")


### 4. CLEAN CMAQ ##########################################################

#' @name clean_cmaq
#' @title CMAQ Dataset Cleaner Function
#' @author Tim Fraser
#' @description Function to clean a CMAQ dataset into code-friendly formats
#' @importFrom dplyr `%>%` mutate across case_when  rename select left_join recode
#' @importFrom readr read_csv
#' @importFrom purrr set_names
#' @importFrom stringr str_sub


clean_cmaq = function(.path_data, .path_variables, .path_types, .path_inflation){
  # Read in...
  readr::read_csv(
    # This file
    file = .path_data, 
    # Import only these variables and make their names lower case to match 'var' in 'data/vars.rds'
    col_select = readr::read_csv(file = .path_variables) %>% 
      with(purrr::set_names(original, var))
  ) %>%
    # Recode a few values
    dplyr::mutate(
      # Make year an integer
      year = as.integer(year),
      # Recode a few values to be standard binary values for easy R use
      dplyr::across(.cols = c(is_outreach_activity, is_tcm, is_congestion_reduction, 
                              is_include_assistance, is_deobligate, is_subproject,
                              voc_qa, nox_qa, co_qa, pm10_qa, pm2_5qa, 
                              has_nonatt_area, ppp),
                    .fns = ~dplyr::case_when(
                      .x == "N" | .x == "No" ~ FALSE,
                      .x == "Y" | .x == "Yes" ~ TRUE,
                      .x == NA ~ NA))
    ) %>%
    # Make project_category_desc into more r-friendly variables
    left_join(by = c("project_category_desc" = "original"),
              y = read_csv(file = .path_types) %>%
                select(original, category_id = id, category = short)) %>%
    #   project_category_desc = project_category_desc %>% dplyr::recode(
    #     # Take as an input a named vector of the old and new category names
    #     !!!readr::read_csv(file = .path_types) %>% with(purrr::set_names(short, original))
    #   )
    # ) %>%
    # Cut this one. Don't need it.
    select(-project_category_desc) %>%
    #dplyr::rename(category = project_category_desc) %>%
    # Extract state from the id
    dplyr::mutate(state = stringr::str_sub(id, 1,2)) %>%
    dplyr::rename(cost = total_proj_amount) %>%
    # Clean Data
    dplyr::mutate(cost = dplyr::case_when(
      cost < 0 ~ abs(cost),
      cost == "999999999" ~ NA_real_,
      cost == 0 ~ NA_real_,
      # cost == "1000000000" ~ NA_real_,
      TRUE ~ cost)) %>%
    dplyr::left_join(by = c("year"), y = readr::read_csv(file = .path_inflation) %>% purrr::set_names(names(.) %>% tolower())) %>%
    dplyr::mutate(year = as.integer(year)) %>%
    dplyr::mutate(cost2022 = cost * inflation) %>%
    # return this data.frame
    return()
}

# read_csv("data_raw/cmaq.csv")
# read_csv("data_raw/cmaq_types.csv") %>% select(category_id = id, category = short)
# 
cmaq_most_recent("data_raw") %>%
  clean_cmaq(
    .path_data = .,
    .path_variables = "data_raw/cmaq_variables.csv",
    .path_types = "data_raw/cmaq_types.csv",
    .path_inflation = "data_raw/cpi_inflation.csv") %>%
  # Write cleaned dataset to file
  readr::write_csv("data_raw/cmaq.csv")



# Write this to rda as well.
cmaq = read_csv("data_raw/cmaq.csv")
save(cmaq, file = "data/cmaq.rda")




### 5. VIEW CMAQ DATA ##########################################################

#' @name cmaq
#' @title Cleaned CMAQ Dataset
#' @author Tim Fraser
#' @importFrom readr read_csv
#' 

# readr::read_csv("data_raw/cmaq.csv") %>%
#   filter(!is.na(co2_benefit)) %>%
#   View()

# readr::read_csv("data_raw/cmaq.csv") %>%
#   filter(!is.na(co2_benefit))  %>%
#   ggplot(mapping = aes(x = cost2022, y = co2_benefit)) +
#   geom_point() +
#   scale_x_continuous(trans = "log") + 
#   scale_y_continuous(trans = "log")
# 
# readr::read_csv("data_raw/cmaq.csv") %>%
#   filter(!is.na(co2_benefit))  %>%
#   lm(formula = log(cost2022 + 1) ~ log(co2_benefit + 0.001)) %>%
#   broom::glance()




### 6. GEOID-CODING ############################################################
# Read in sheet

# Update geoid_coding2 in this sheet
# https://docs.google.com/spreadsheets/d/1mFF2EqXRJb5FtbC7Ps-Jc-eqvbMxoI4Ym_vuR57UIvk/edit#gid=788965441
url = 'https://docs.google.com/spreadsheets/d/1mFF2EqXRJb5FtbC7Ps-Jc-eqvbMxoI4Ym_vuR57UIvk/export?format=csv&gid=788965441'
read_csv(url) %>%
  # pad with zeros
  mutate(mpo_id = stringr::str_pad(mpo_id, width = 8, side = "left", pad = "0")) %>%
  mutate(geoid = stringr::str_pad(geoid, width = 5, side = "left", pad = "0")) %>%
  mutate(state_id = stringr::str_pad(state, width = 2, side = "left", pad = "0")) %>%
  write_csv("data_raw/geoid_coding.csv")



# 7. AREA SPECIFIC ESTIMATES ############################
# We're going to try to reverse engineer the CMAQ policy cost effectiveness estimates
library(readr)
library(dplyr)
library(tidyr)



# We developed a script to make auto-updating cost-effectiveness statistics,
# which can be re-run when new CMAQ projects are added.
# Our raw data produces 828 valid median cost-effectiveness estimates
# for specific CMAQ types for specific pollutants in specific years.
# However, in reality, the full grid of category-pollutant-year combinations 
# we want cost-effectiveness statistics for is much larger,
# spanning 1320 possible combinations 
# over a 24-year timespan from 1999 to 2022,
# for 5 pollutants (CO2, CO, NOX, VOC, and PM10) 
# over 11 CMAQ policy types.



# Count Combinations
expand_grid(
  category_id = raw_data %>% with(category_id) %>% unique(),
  pollutant = raw_data %>% with(pollutant) %>% unique(),
  year = seq(from = raw_data %>% with(year) %>% min(na.rm = TRUE), 
             to = raw_data %>% with(year) %>% max(na.rm = TRUE), 
             by = 1)
) %>% 
  count()

# Count unique types
tibble(
  category_id = raw_data %>% with(category_id) %>% unique() %>% length(),
  pollutant = raw_data %>% with(pollutant) %>% unique() %>% length(),
  year = seq(from = raw_data %>% with(year) %>% min(na.rm = TRUE), 
             to = raw_data %>% with(year) %>% max(na.rm = TRUE), 
             by = 1) %>% 
    length()
)


raw_data = cmaq_to_percentile(prob = 0.5)
# So, to improve our coverage, we calculated various 
# multi-year averages of the median cost-effectiveness.

raw_data %>%
  get_avg(range = 1) %>%
  get_diagnostics()
# 1-year estimates cover 828 out of 1,320 sets (63%).

raw_data %>%
  get_avg(range = 1) %>%
  group_by(category_id) %>%
  get_diagnostics() %>%
  left_join(by = c("category_id" = "id"), 
            y = read_csv("data_raw/cmaq_types.csv") %>% select(id, short))
# This covers upwards of >75% of sets for each CMAQ category,
# with the exception of alternative fuels (28%),
# diesel engine retrofit (23%), 
# freight/intermodal policies (18%),
# and other policies (44%).

raw_data %>%
  get_avg(range = 1) %>%
  filter(year == 2021) %>% 
  group_by(category_id) %>%
  get_diagnostics() %>%
  left_join(by = c("category_id" = "id"), 
            y = read_csv("data_raw/cmaq_types.csv") %>% select(id, short))

# By 2021, all CMAQ policy types have 100% coverage, 
# except for freight/intermodal policies (40%) and STP policies (80%).


raw_data %>%
  get_avg(range = 1) %>%
  group_by(pollutant) %>%
  get_diagnostics()
# This covers 70% of sets for CO, NOx, VOC, and PM10, compared to 25% for CO2.


raw_data %>%
  get_avg(range = 1) %>%
  filter(pollutant == 98) %>%
  group_by(year) %>%
  get_diagnostics() %>%
  filter(valid > 0)
# CO2e statistics are not available until 2011; 
# starting in 2011, we have coverage for 27.3% of desired combinations,
# but by 2021, we have 82% coverage for CO2e-related figures.


# Ultimately, we selected 5 year averages
get_avg(data = raw_data, range = 5) %>%
  write_csv("data_raw/ce_averages.csv")




## Table Z1 ####################################

# To improve our coverage, we compared 1-year estimates 
# against 3-year averages and 5-year averages.
manyavg = bind_rows(
  get_avg(data = raw_data, range = 1),
  get_avg(data = raw_data, range = 3),
  get_avg(data = raw_data, range = 5),
  get_avg(data = raw_data, range = 7),
  get_avg(data = raw_data, range = 9),
  get_avg(data = raw_data, range = 11),
  get_avg(data = raw_data, range = 13),
  get_avg(data = raw_data, range = 15),
  .id = "range"
)  %>%
  mutate(range = (as.integer(range) - 1)*2 + 1 )


manyavg %>%
  group_by(range) %>%
  get_diagnostics() %>%
  write_csv("paper/table_A1.csv")

## Figure Z1 ####################################
library(ggplot2)
data = read_csv("paper/table_A1.csv")


gg = ggplot() +
  geom_line(data = data, 
            mapping = aes(x = percent, y = n_years, color = range),
            color = "#648FFF", linewidth = 2) +
  geom_label(data = data,
             mapping = aes(x = percent, y = n_years, label = paste0(range, "-yrs")),
             color = "#648FFF") +
  geom_text(data = data,
            mapping = aes(x = percent, y = n_years, label = paste0(range, "-yrs")),
            color = "#373737") +
  labs(x = "Percentage of Coverage of Combinations\n of Pollutants, Policy Types, and Years",
       y = "# of Years Covered",
       title = "Relative Coverage of 1- to 15-year averages\n of CMAQ Median Cost Effectiveness Statistics") +
  theme_bw(base_size = 14) +
  theme(plot.title = element_text(hjust = 0.5))

ggsave(gg, filename = "paper/figure_A1.png", dpi = 200, width = 6, height = 6)


## Alternative percentiles #########################

data = cmaq_to_percentile(prob = 0.50, min_cost = 0)


data %>%
  get_avg(var = "ce", range = 5) %>%
  filter(pollutant == 2) %>%
  filter(year == 2020)

data %>%
  get_avg(var = "ce", range = 5) %>%
  filter(pollutant == 98) %>%
  arrange(desc(year))

data %>%
  get_avg(var = "epd", range = 5) %>%
  filter(pollutant == 98) %>%
  arrange(desc(year))




# 8. Cost Curve ######################################


library(dplyr)
library(readr)
library(ggplot2)
library(ggtext)
library(shadowtext)

# The process above gave us cost-effectiveness estimates for past years,
# but to estimate cost in the future, we need to build a simple cost curve function over time.
read_csv("data_raw/ce_averages.csv") %>%
  group_by(pollutant, category_id) %>%
  summarize(valid = sum(is.na(estimate))) %>%
  ungroup() %>%
  filter(valid > 0) %>%
  with(valid) %>%
  summary()


# For each pollutant-policy type pair, 
# our process constructs a linear model of the natural log of cost-effectiveness and the year of the estimate.
# Each pairing of a pollutant and policy type has between 3 and 20 observations to model,
# 75% of pairs have at least 9 observations.

# View policy categories
read_csv("data_raw/cmaq_types.csv") %>% select(id, short)

# For each pollutant and CMAQ policy category, 
# we can use the existing cost estimates
# to estimate a cost curve over time, which will help us estimate costs into the future.

read_csv("data_raw/ce_averages.csv") %>%
  filter(pollutant == 98) %>%
  filter(category_id == 1) %>%
  lm(formula = log(estimate) ~ year) %>%
  broom::glance()

# Make many cost curve models
data = read_csv("data_raw/ce_averages.csv")

frame = data %>%
  select(pollutant, category_id) %>%
  distinct() %>%
  mutate(id = 1:n())


stats1 = frame %>%
  split(.$id) %>%
  # Approximate into the future
  map_dfr(
    .f = possibly(
      ~data %>%
        filter(pollutant == .x$pollutant, category_id == .x$category_id) %>%
        lm(formula = log(estimate) ~ year) %>%
        broom::glance(), 
      other = tibble()), 
    .id = "id")  %>%
  mutate(id = as.integer(id)) %>%
  left_join(by = 'id', y = frame) 


frame2 = data %>%
  select(pollutant) %>%
  distinct() %>%
  mutate(category_id = NA) %>%
  mutate(id = 1:n())  

stats2 = frame2 %>%
  split(.$id) %>%
  # Approximate into the future
  map_dfr(
    .f = possibly(
      ~data %>%
        filter(pollutant == .x$pollutant) %>%
        lm(formula = log(estimate) ~ year + factor(category_id) ) %>%
        broom::glance(), 
      other = tibble()), 
    .id = "id") %>%
  mutate(id = as.integer(id)) %>%
  left_join(by = 'id', y = frame2)   

# Bundle statistics
bind_rows(stats1, stats2) %>%
  select(id, pollutant, category_id, r.squared, sigma, statistic, df.residual) %>%
  # Recode in pollutant 
  mutate(pollutant_label = pollutant %>% recode_factor(
    "98" = "CO<sub>2</sub>",
    "2" = "CO",
    "3" = "NO<sub>x</sub>",
    "87" = "VOCs",
    "100" = "PM<sub>10</sub>"
  )) %>%
  left_join(by = c("category_id" = "id"), y = read_csv("data_raw/cmaq_types.csv") %>% select(id, category = simple)) %>%
  mutate(category = case_when(is.na(category) ~ "Overall", TRUE ~ category)) %>%
  write_csv("paper/data_accuracy_ce_by_pollutant_category.csv")


# This produces 54 valid models out of 55 pairings, with varying (but typically high) predictive power.
# We visualize each model's predictive accuracy with the R2 statistic in the figure below.
# Out of all 54 models, 12 had excellent accuracy (>=90%), 24 had good accuracy (>=80%), and 39 had fine accuracy (>=50%)
# This left 16 pollutant-policy type pairings whose cost curve cannot be modeled well based on existing data.

# Whenever a pairing is poorly modeled (R2<50%), we backstep to a model of all natural-logged estimates for that pollutant, predicted by year and policy type. 
# This increases the sample size available, and tends to result in suitable accuracy over 50% (albeit not always as high accuracy as the pollutant-policy type-specific models). 

read_csv("paper/data_accuracy_ce_by_pollutant_category.csv") %>% 
  filter(category != "Overall") %>%
  summarize(
    # Count total valid models
    total = n(),
    excellent = sum(r.squared >= 0.9),
    good = sum(r.squared >= 0.8),
    fine = sum(r.squared >= 0.5),
    )


## Figure Z2 ###################################
data = read_csv("paper/data_accuracy_ce_by_pollutant_category.csv") %>%
  mutate(pollutant_label = factor(pollutant_label, levels = c("CO<sub>2</sub>", "CO", "NO<sub>x</sub>", "VOCs", "PM<sub>10</sub>")))

gg = ggplot() +
  geom_col(
    data = data,
    mapping = aes(
      x = pollutant_label,
      y = r.squared,
      fill = pollutant_label)
  ) +
  shadowtext::geom_shadowtext(
    data = data,
    mapping = aes(x = pollutant_label, y = r.squared,
                  label = paste0(round(r.squared*100,0), "")), 
    bg.r = 0.3, bg.color = "white", color = "#373737", nudge_y = 0.08, size = 5,
  ) +
  facet_wrap(~reorder(category, category_id))  +
  viridis::scale_fill_viridis(option = "plasma", discrete = TRUE, begin = 0.1, end = 0.8) +
  theme_classic(base_size = 16)  +
  theme(
    
    panel.border = element_rect(fill = NA, color = "#373737"),
        strip.background = element_rect(fill = "#373737"),
        strip.text = element_text(color = "white"),
        axis.text.x = ggtext::element_markdown(),
        axis.title.y = ggtext::element_markdown(),
        plot.title = element_text(hjust = 0.5)
        ) +
  guides(fill = "none") + 
  scale_y_continuous(labels = scales::label_percent(),
                     breaks = c(0, .25, .50, .75, 1.0),
                     limits = c(0, 1.08), expand = expansion(c(0,0.1))) +
  labs(x = "Pollutant", y = "Model Accuracy (R<sup>2</sup>)",
       title = "Cost-Effectiveness Model Accuracy by Pollutant and Policy Type")

ggsave(gg, filename = "paper/figure_A2.png", dpi = 200, width = 11, height = 8)

# browseURL("paper/figure_A2.png")

## cost_effectiveness ########################
cost_effectiveness = read_csv("data_raw/ce_averages.csv") %>%
  mutate(pollutant = as.integer(pollutant),
         category_id = as.integer(category_id),
         year = as.integer(year))
save(cost_effectiveness, file = "data/cost_effectiveness.rda")

## process ##########################
library(dplyr)
library(readr)
library(broom)

source("R/get_cost_model.R")
data = read_csv("data_raw/ce_averages.csv") 

save(data, file = cost_effectiveness)
m = get_cost_model(data = data, .pollutant = 98, .category_id = 1)
remove(data, m)

m = get_cost_model(data = data, .pollutant = 98, .category_id = 2)
m %>% broom::glance()


# 9. DESCRIPTIVES ##########################
library(dplyr)
library(readr)
data = read_csv("data_raw/cmaq.csv") 
  select(state, year, category_id,
         contains("_benefit"),
         cost2022) 

# Projects available
data %>%
  count()
data %>% glimpse()
data %>%
  summarize(min = min(year),
            max = max(year))

data %>%
  filter(!is.na(year),
         !is.na(cost2022),
         !is.na(category_id)) %>%
  count()

data %>%
  filter(!is.na(year),
         !is.na(cost2022),
         !is.na(category_id)) %>%
  filter(
    !is.na(co2_benefit) |
      !is.na(voc_benefit) |
      !is.na(nox_benefit) |
      !is.na(pm10_benefit) |
      !is.na(co_benefit)
  ) %>%
  summarize(min = min(year))


data %>%
  filter(!is.na(year),
         !is.na(cost2022),
         !is.na(category_id)) %>%
  filter(
         !is.na(co2_benefit) |
         !is.na(voc_benefit) |
         !is.na(nox_benefit) |
         !is.na(pm10_benefit) |
         !is.na(co_benefit)
        ) %>%
  count()

data %>%
  filter(!is.na(year),
         !is.na(cost2022),
         !is.na(category_id)) %>%
  summarize(
    co2 = sum(!is.na(co2_benefit)),
    co = sum(!is.na(co_benefit)),
    pm10 = sum(!is.na(pm10_benefit)),
    nox = sum(!is.na(nox_benefit)),
    voc = sum(!is.na(voc_benefit))
  )
  
