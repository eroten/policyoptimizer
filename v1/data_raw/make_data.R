#' @name make_data.R
#' @author Tim Fraser
#' @title A script for making our package data.
#' @description
#' Let's make a series of objects for testing purposes
#' which will serve as 'archetypal' input parameters for our functions. 

setwd(rstudioapi::getActiveProject())
setwd("optimizer")
# cattest #####################################

setwd(rstudioapi::getActiveProject())
setwd("optimizer")


# Read Environmental Variables
readRenviron(".Renviron")
# Load packages
library(dplyr)
library(DBI)
library(RMySQL)

# Connect
db = dbConnect(
  drv = RMySQL::MySQL(),
  user = Sys.getenv("CATSERVER_USERNAME"),
  password = Sys.getenv("CATSERVER_PASSWORD"),
  host = Sys.getenv("CATSERVER_HOST"),
  port = as.integer(Sys.getenv("CATSERVER_PORT")),
  dbname = "granddata"
)

cattest = db %>% 
  tbl("d36109") %>%
  filter(by == 16) %>% 
  # Get the main pollutants
  filter(pollutant %in% c(98, 2, 3, 87, 31,33,110,100)) %>%
  select(by, year, geoid, pollutant, emissions, vmt) %>%
  collect()

save(cattest, file = "data/cattest.rda")

dbDisconnect(db)


# Clear environment
rm(list = ls())

