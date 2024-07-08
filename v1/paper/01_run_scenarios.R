#' @name run_scenarios.R
#' @author Tim Fraser
#' @description
#' Script to run optimizer to generate optimal scenarios used in the paper.


# load packages ###################################

# First let's load packages and setup.

library(dplyr)
library(readr)
library(tidyr)
library(ompr)
library(ompr.roi, warn.conflicts = FALSE, quietly = TRUE)
library(ROI)
library(ROI.plugin.glpk)

rm(list = ls())

setwd(paste0(rstudioapi::getActiveProject(), "/v1"))
devtools::load_all(".")
# Read environmental variables
readRenviron(".Renviron")

# run optimizer ###########################

## by diversification ####################################
# Main scenario, stratified by level of max_abs_diff 

strata = tribble(
  ~geoid,  ~max_abs_diff,
  "36109", 1,
  "36109", 0.5,
  "36109", 0.25,
  "36109", 0.1,
  "36109", 0.05,
  "36109", 0.01,
  "36109", 0.001
) %>%
  mutate(id = as.character(1:n())) 

strata %>%
  split(.$id) %>%
  map_dfr(~policy_optimizer(
    geoid = .$geoid,  pollutant = 98, start_year = 2025, end_year = 2050, policies = 1:10,
    var = "epd", prob = 0.50, range = 5, cmaq_path = "data_raw/cmaq.csv",
    last_cost = 0, last_reduction98 = 0, 
    target_year = 2005, target_change = 0.50,
    n_scales = 100, units = 30000, min_annual_cost = 10000, 
    wc = 0.5, wr = 0.5, max_abs_diff = .$max_abs_diff
  ), .id = "id") %>%
  left_join(by = "id", y = strata %>% select(id, max_abs_diff)) %>%
  write_csv("paper/scenarios_by_diversification.csv")


# View results
# read_csv("paper/scenarios_by_diversification.csv") %>% View()

rm(list = ls()); gc()

## single county, tradespace ###############################################

# Let's first randomly sample the tradespace....

# Testing values
n = 5000; n_scales = 1e2; units = 30000; 
training = get_training(.geoid = "36109", .pollutant = 98, .by = 16, test = FALSE)
frame = get_frame(data = training)
record = get_record(geoid = "36109", pollutants = 98, start_year = 2025, end_year = 2050, policies = c(1:10), last_cost = 0, last_reduction98 = 0)
effects = get_effects(.geoid = "36109", .pollutants = 98, .policy = c(1:10),  .start_year = 2025, .end_year = 2050, .units = units, var = "epd", path = "data_raw/cmaq.csv", prob = 0.50, min_cost = 0, range = 5)
constants = get_constants(effects = effects, training = training, n_scales = n_scales, .year = 2005, .percent_change = 0.50)
# ~6 seconds
system.time({
  scenario_sampler(n = n, n_scales = n_scales, record = record, effects = effects, constants = constants, wc = 0.5, wr = 0.5) %>%
    saveRDS("paper/metrics_units.rds")
})

# Unfortunately, we will probably end up with a lot of points that are
# quite far from the optimal solution.

# Instead, let's take our optimal solution given three different levels of diversification,
# and we'll randomly adjust their input values until we get a fuller picture of the tradespace.

# Now let's repeat, but this time, sample outwards from an optimal seed.
seed = read_csv("paper/scenarios_by_diversification.csv") %>%
  filter(max_abs_diff %in% c(1))  %>%
  select(p1:p10)
system.time({
  scenario_sampler(n = n, n_scales = n_scales, record = record, effects = effects, constants = constants, wc = 0.5, wr = 0.5, seed = seed) %>%
    saveRDS("paper/metrics_units_seeded_1.rds")
})
# Now let's repeat, but this time, sample outwards from an optimal seed.
seed = read_csv("paper/scenarios_by_diversification.csv") %>%
  filter(max_abs_diff %in% c(.1))  %>%
  select(p1:p10)
system.time({
  scenario_sampler(n = n, n_scales = n_scales, record = record, effects = effects, constants = constants, wc = 0.5, wr = 0.5, seed = seed) %>%
    saveRDS("paper/metrics_units_seeded_0_1.rds")
})
# Now let's repeat, but this time, sample outwards from an optimal seed.
seed = read_csv("paper/scenarios_by_diversification.csv") %>%
  filter(max_abs_diff %in% c(.01))  %>%
  select(p1:p10)
system.time({
  scenario_sampler(n = n, n_scales = n_scales, record = record, effects = effects, constants = constants, wc = 0.5, wr = 0.5, seed = seed) %>%
    saveRDS("paper/metrics_units_seeded_0_01.rds")
})

# Let's bundle them together
bind_rows(
  read_rds("paper/metrics_units.rds") %>%
    mutate(seed = "None"),
  read_rds("paper/metrics_units_seeded_1.rds") %>%
    mutate(seed = "1"),
  read_rds("paper/metrics_units_seeded_0_1.rds") %>%
    mutate(seed = "0.1"),
  read_rds("paper/metrics_units_seeded_0_01.rds") %>%
    mutate(seed = "0.01")
) %>%
  saveRDS("paper/metrics_units_seeded.rds")

#

## by county, whole state ################################################

rm(list = ls())

# Identify eligible cases
db = connect("granddata")
eligible = tibble(table = db %>% dbListTables()) %>%
  mutate(geoid = stringr::str_remove(table, "d")) 
dbDisconnect(db); remove(db)

# Get geoids of all cases
cases = tigris::fips_codes %>%
  filter(state_code == "36") %>%
  mutate(geoid = paste0(state_code, county_code)) %>%
  select(geoid, county) %>%
  left_join(by = "geoid", y = eligible)


# Run policy optimizer for each area over time
cases %>%
  split(.$geoid) %>%
  map_dfr(~policy_optimizer(
    # Basic Metadata
    geoid = .x$geoid,  pollutant = 98, by = 16, start_year = 2025, end_year = 2050, policies = 1:10,
    n_scales = 100, units = 30000, max_abs_diff = 1, min_annual_cost = 10000, wr = 0.5, wc = 0.5, 
    # Cost Effectiveness Stats
    var = "epd", prob = 0.50, range = 5, cmaq_path = "data_raw/cmaq.csv",
    # Any prior cumulative costs / reductions
    last_cost = 0, last_reduction98 = 0, target_year = 2005, target_change = 0.50
  ),
  .id = "geoid") %>%
  write_csv("paper/state_run_1.csv")


# geographic bounds ##################################################
# Next, let's get spatial polygons

library(sf)
library(dplyr)
library(readr)
library(DBI)
library(RMySQL)

#' @name set_geo
#' @title Set Geometry Function
#' @author Tim Fraser
#' @description Reformats a character geometry vector object into sf format
#' @param .geo A Well-Known-Text formatted geometry, meaning a character string like `"POINT (1, 1)"`.
#' @importFrom sf st_as_sfc st_make_valid
#' @importFrom dplyr tibble
#' @importFrom purrr map_dfr possibly
#' @export
set_geo = function(.geo){
  # Reformat the text as geometry
  make_mini = function(.geo){ sf::st_as_sfc(.geo, crs = 4326) }
  # If that doesn't work, make it be an empty multipolygon
  try_make_mini = purrr::possibly(.f = make_mini, otherwise = sf::st_as_sfc("MULTIPOLYGON EMPTY", crs = 4326), quiet = TRUE)
  # For each cell, make a geometry value, that are bound together into a tibble
  output = purrr::map_dfr(.x = .geo, .f = ~tibble(geometry = try_make_mini(.)))
  # Extract just the geometry as a vector
  # Try to make the output valid
  output = sf::st_make_valid(output$geometry)
  # Return output
  return(output)
}

# Download county boundaries
geo = connect("geo")

bounds = geo %>%
  tbl("g36") %>%
  filter(type == "low") %>%
  collect() %>%
  mutate(geom = set_geo(geom)) %>%
  sf::st_as_sf(sf_column_name = 'geom', crs = 4326)  %>%
  select(fid, geom, geoid, name) %>%
  saveRDS("paper/bounds.rds")

dbDisconnect(geo); remove(geo)

# benchmarking #############################

# Get geoids of all cases
cases = tigris::fips_codes %>%
  filter(state_code == "36") %>%
  mutate(geoid = paste0(state_code, county_code)) %>%
  select(geoid, county)

grid = expand_grid(
  cases %>% sample_n(size = 3),
  n_policies = c(10,5),
  end_year = c(2050, 2030),
  max_abs_diff = c(1,0.1,0.01),
  pollutants = c("98",  "98,2,3")
) %>%
  mutate(id = 1:n())


benchmark = function(.geoid = "36109", .start_year = 2025, .end_year,.n_policies, .pollutants, .max_abs_diff){
  # Testing Values
  # .end_year = 2030; .n_policies = 2; .max_abs_diff = 1
  timer = system.time({
    
    
    #.pollutants = c("98,2,3")
    pollutants = as.vector(str_split(.pollutants, pattern = ",", simplify = TRUE))
    
    result = policy_optimizer(
      geoid = .geoid,  pollutant = pollutants, start_year = 2025, end_year = .end_year, policies = 1:.n_policies,
      n_scales = 100, units = 30000, max_abs_diff = .max_abs_diff, wr = 0.5, wc = 0.5, min_annual_cost = 10000,
      target_year = 2005, target_change = 0.50,
      var = "epd", prob = 0.50, range = 5, cmaq_path = "data_raw/cmaq.csv"
    )
  })
  
  output = tibble(seconds = timer[[3]],
                  start_year = .start_year,
                  end_year = .end_year,
                  n_policies = .n_policies,
                  max_abs_diff = .max_abs_diff,
                  pollutants = .pollutants,
                  geoid = .geoid)
  return(output)
}
#benchmark(.start_year = 2025, .end_year = 2030, .n_policies = 10,.max_abs_diff = 1)

grid %>%
  split(.$id) %>%
  map_dfr(~benchmark(.geoid = .$geoid, .start_year = 2025, .end_year = .$end_year,
                     .n_policies = .$n_policies, .pollutants = .$pollutants, .max_abs_diff = .$max_abs_diff), 
          .id = "id") %>%
  write_csv("paper/benchmarking.csv")

