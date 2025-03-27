#' @name correlations
#' @description
#' This script evaluates the correlations
#' between criteria air pollutants and CO2e
#' in a random sample of US Counties.

library(dplyr)
library(readr)
library(DBI)
library(RMySQL)
library(stringr)
library(purrr)
library(tidyr)

setwd(paste0(rstudioapi::getActiveProject(), "/v1"))
readRenviron(".Renviron")

connect <- function() {
  dbConnect(
    drv = RMySQL::MySQL(),
    user = Sys.getenv("CATSERVER_USERNAME"),
    password = Sys.getenv("CATSERVER_PASSWORD"),
    host = Sys.getenv("CATSERVER_HOST"),
    port = as.integer(Sys.getenv("CATSERVER_PORT")),
    dbname = "granddata"
  )
}

# connect to database
db <- connect()
# Get tables
tab <- tibble(table = db %>% dbListTables()) %>%
  # Keep just counties
  filter(str_detect(table, "d[0-9]{5}")) %>%
  # Give each a numeric id
  mutate(id = 1:n()) %>%
  # Split counties into 15 groups
  mutate(group = ntile(id, 15))
# Disconnect
dbDisconnect(db)

# Create a home for tables
dir.create("data_raw/emissions")


# Write a function to get the data per group
get_data <- function(data) {
  # Connect to database
  db <- connect()
  # Start with a blank table
  q <- db %>%
    tbl(data$table[1]) %>%
    head(0)

  # Make a list of queries
  n <- length(data$table)
  # Get group id
  g <- data$group[1]
  # For each table...
  for (i in 1:n) {
    q <- db %>%
      tbl(data$table[i]) %>%
      filter(pollutant %in% c(98, 100, 110, 2, 3, 87, 33, 31), by == 16) %>%
      select(year, geoid, pollutant, emissions, vmt) %>%
      union(q)
    # Completion message
    cat(paste0("\ngroup: ", g, " - ", i, " / ", n))
  }
  # Every 200 counties, we'll collect and save the data.
  path <- paste0("data_raw/emissions/correlations_", g, ".rds")
  q %>%
    collect() %>%
    saveRDS(path)

  if (file.exists(path)) {
    cat("\nsaved successfully.\n")
  } else {
    stop("\nFile did not save successfully.")
  }


  # Disconnect
  dbDisconnect(db)
  remove(db)
}


# VOC = 87
# 110 = PM2.5
# 100 = PM10
# 2 = CO
# 98 = CO2
# 3 = NOx
# 33 = NO2
# 31 = SO2

# Run it all in a loop
tab %>%
  split(.$group) %>%
  walk(~ get_data(data = .))


dbDisconnect(db)
remove(db)


rm(list = ls())


# Combine the files
dir("data_raw/emissions", full.names = TRUE) %>%
  map_dfr(~ read_rds(.)) %>%
  select(year, geoid, pollutant, emissions) %>%
  tidyr::pivot_wider(
    id_cols = c(geoid, year),
    names_from = pollutant,
    names_prefix = "e",
    values_from = emissions
  ) %>%
  saveRDS("data_raw/catserver_stats.rds")



dir("data_raw/emissions", full.names = TRUE) %>%
  map_dfr(~ read_rds(.)) %>%
  select(year, geoid, pollutant, emissions, vmt) %>%
  saveRDS("data_raw/catserver_stats_with_vmt.rds")
