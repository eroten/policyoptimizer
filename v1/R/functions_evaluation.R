#' @title functions_evaluation.R
#' @description
#' This script develops the policy optimizer's functions for evaluating metrics.
#' This produces the starter effects data used.


# EVALUATION ################################

#' @name connect
#' @title connect
#' @description
#' Internal function for connecting to databases.
#' @param type (character) database name. Choose from `"policies"`, `"cost_effectiveness"`, `"granddata"`, `"geo"`
#' @import DBI
#' @import RMySQL
connect <- function(type = "policies") {
  if (type == "policies") {
    # Currently, return the policies database,
    # but in the future, but in the future, we'll pipe in from a database.
    data("policies_data", envir = environment())
    output <- policies_data
    # remove(policies_data)
  } else if (type == "cost_effectiveness") {
    # Currently, return the pre-generated cost-effectiveness dataset,
    # but in the future, we'll pipe in from a database.
    output <- cost_effectiveness
  } else if (type == "granddata") {
    # Otherwise, if it is the name of a catserver table, return a connection to catserver
    output <- DBI::dbConnect(
      drv = RMySQL::MySQL(),
      username = Sys.getenv("CATSERVER_USERNAME"),
      password = Sys.getenv("CATSERVER_PASSWORD"),
      host = Sys.getenv("CATSERVER_HOST"),
      port = as.integer(Sys.getenv("CATSERVER_PORT")),
      dbname = "granddata"
    )
  } else if (type == "geo") {
    # Otherwise, if it is the name of a catserver table, return a connection to catserver
    output <- DBI::dbConnect(
      drv = RMySQL::MySQL(),
      username = Sys.getenv("CATSERVER_USERNAME"),
      password = Sys.getenv("CATSERVER_PASSWORD"),
      host = Sys.getenv("CATSERVER_HOST"),
      port = as.integer(Sys.getenv("CATSERVER_PORT")),
      dbname = "geo"
    )
  }


  return(output)
}


#' @name what_level
#' @title Check What Level is my geoid
#' @author Tim Fraser
#' @param geoid (character) unique census geographic id for each county, state, county subdivision/municipality, nation, etc.
#' @export
# Short function to check what level is the data you are analyzing, based on your input$geoid.
what_level <- function(geoid) {
  .geoid <- unique(geoid)
  n_geoid <- length(.geoid)
  if (n_geoid > 1) {
    stop("what_level() only works when geoid is length 1.")
  }
  # Get total characters
  n <- nchar(.geoid)
  # If 00, that's nation.
  if (.geoid %in% "00") {
    "nation"
    # If 2 digits and not 00, that's a state
  } else if (n == 2) {
    "state"
    # If 5 digits, that a county
  } else if (n == 5) {
    "county"
    # If 11 digits, that's a county subdivision
  } else if (n == 11) {
    "muni"
  } else {
    stop("level not known.")
  }
}

#' @name cmaq_to_percentile
#' @title cmaq_to_percentile
#' @importFrom dplyr `%>%` if_else select arrange contains mutate filter group_by summarize
#' @importFrom tidyr pivot_longer
#' @importFrom readr read_csv
cmaq_to_percentile <- function(path = "data_raw/cmaq.csv", prob = 0.50, min_cost = 0) {
  # Check if the path to new cmaq data exists
  file_good <- if (!is.null(path)) {
    file.exists(path)
  } else {
    FALSE
  }

  if (file_good == TRUE) {
    # Load cmaq from file
    cmaq <- read_csv(path, show_col_types = FALSE)
  } else if (file_good == FALSE) {
    # Load cmaq data from package
    data("cmaq", envir = environment())
  } else {
    stop("message: fatal error. cmaq cost effectiveness data could not be sourced from package.")
  }

  data <- cmaq %>%
    select(id, cmaq_project_id, category_id, year, contains("_benefit"), cost2022) %>%
    arrange(cmaq_project_id, year) %>%
    # Let's start with just overall by category
    # We can only estimate using observations of BOTH benefit AND cost
    pivot_longer(
      cols = c(contains("_benefit")),
      names_to = "pollutant", values_to = "effect_emissions",
      values_drop_na = TRUE
    ) %>%
    # Recode Pollutant
    mutate(
      pollutant = pollutant %>% dplyr::recode(
        "pm10_benefit" = 100,
        "co_benefit" = 2,
        "co2_benefit" = 98,
        "nox_benefit" = 3,
        "voc_benefit" = 87
      ) %>% as.integer(),
      category_id = as.integer(category_id)
    ) %>%
    # Drop the NAs - we can't evalute those rows
    filter(!is.na(effect_emissions) & !is.na(cost2022)) %>%
    # Emissions benefit is measured in kg per day.
    # We want it in tons / year
    # 1 kg = 0.00110231 tons
    # Upscale to tons from kg, upscale from per day to per 1 year
    # Effect_emissions = X kg / 1 day
    # 0.00110231 tons / 1 kg
    # 365.25 days / 1 year
    # E_em = X kg / 1 day * .00110231 tons / 1 kg *  (1 day /  (1/365.25) years)
    mutate(
      effect_emissions = effect_emissions * 0.00110231 * 365.25,
      cost2022 = cost2022
    )


  if (!is.null(min_cost)) {
    data <- data %>%
      # Sometimes, folks report a cost of zero. We're going to bump that up to a minimum cost threshold
      # to make sure these values don't get excluded.
      mutate(cost2022 = if_else(cost2022 == 0, true = min_cost, false = cost2022))

    # IF min_cost is NULL, we'll just drop these zero-cost observations
  } else if (is.null(min_cost)) {
    data <- data %>%
      filter(cost2022 > 0)
  }

  data <- data %>%
    # For each pollutant-year-category,
    group_by(category_id, year, pollutant) %>%
    # Get the AVERAGE cost effectiveness in terms of $ spent per 1 ton of emissions reduced,
    summarize(
      # median cost per ton of emissions
      ce = quantile(cost2022 / effect_emissions, prob = prob),
      # median emissions per dollar
      epd = quantile(effect_emissions / cost2022, prob = prob),
      .groups = "drop"
    )

  return(data)
}



#' @name get_avg
#' @title get_avg
#' @description
#' Get n-year averages for cost effectiveness statistics or emissions per dollar statistics.
#' @importFrom dplyr `%>%` select distinct reframe group_by left_join summarize n filter
#' @importFrom tidyr expand_grid
get_avg <- function(data, var = "ce", range = 5) {
  # Testing values
  # data = raw_data
  # range = 5

  # Compute for a N-year average,
  # how many extra values on either side of the estimate year are needed
  # Eg. a 3 year average for 2022 requires 2021, 2022, and 2023;
  # in other words, 1 year more and less than the year of interest
  # Test it
  # range = c(1,3,5,7,9)
  extra <- ceiling(range / 2) - 1
  # Equivalent to:
  # extra %>% dplyr::recode(
  #        "9"= 4,
  #        "7"= 3,
  #        "5" = 2,
  #        "3" = 1,
  #        "1" = 0)
  # Get multi-year frame of data, for making multi-year averages
  avg <- data %>%
    select(year) %>%
    distinct() %>%
    # Get full timespan
    reframe(year = seq(
      from = min(year, na.rm = TRUE),
      to = max(year, na.rm = TRUE), by = 1
    )) %>%
    # For each year, get a multi-year range
    group_by(year) %>%
    reframe(yearrange = seq(from = year - extra, to = year + extra, by = 1)) %>%
    # Expand this grid for every category
    expand_grid(
      .,
      category_id = data %>% with(category_id) %>% unique(),
      pollutant = data %>% with(pollutant) %>% unique()
    ) %>%
    # Join in the observed data to this range,
    # for each yearrange-category pair
    left_join(
      by = c("yearrange" = "year", "category_id", "pollutant"),
      y = data, multiple = "all"
    ) %>%
    # The result will be a tidy-dataset of median cost-effectiveness
    # that we can average across multiple years per pollutant
    #
    # Many of the values for 'ce' in the joined dataset will remain NA.
    # That's intended - we know that estimates are often not available for some specific combinations of year, category, and pollutant
    # That's one of the reasons why we are averaging.
    #
    # Now, for each year, category, and pollutant,
    group_by(year, category_id, pollutant) %>%
    # Average the N-years worth of cost effectiveness statistics that we repeated
    summarize(
      # Calculate an N-year mean
      estimate = mean(!!sym(var), na.rm = TRUE),
      # Calculate a standard error too for the N-year mean.
      se = sd(!!sym(var), na.rm = TRUE) / sqrt(n()),
      # Record how many observations
      n_obs = n(),
      .groups = "drop"
    )

  # Naturally, if we are taking multi-year averages,
  # we are no longer able to provide as many estimates on the edges of the time-frame.
  # For example, a 3-year average of 2022 data will no longer be possible;
  # Instead, we can provide a 3-year average of 2021, using 2020, 2021, and 2022.

  lower_year <- avg %>%
    with(year) %>%
    min(na.rm = TRUE) + extra
  upper_year <- avg %>%
    with(year) %>%
    max(na.rm = TRUE) - extra

  avg <- avg %>%
    filter(year >= lower_year & year <= upper_year)

  # Let's add a short code to trim off these invalid years
  # avg = avg  %>%
  #  filter( year >= (min(year) + extra) & year <= (max(year) + extra) )

  # # Keep only rows where
  # filter(
  #   # Eg. if 3-year average, then extra is +/-1,
  #   # so drop min year 1999, for which we don't have 3 observations,
  #   # but keep 2000, for which we could possibly have 1999, 2000, and 2001.
  #   year >= min(year) + extra,
  #   # Eg. if 3-year average, then
  #   # drop max year 2022, for which we don't have 3 observations,.
  #   # but keep 2021, for which we could possibly have 2020, 2021, and 2022.
  #   year <= max(year) - extra)
  #
  #

  return(avg)
}

#' @name get_diagnostics
#' @title get_diagnostics
#' @importFrom dplyr `%>%` summarize n
get_diagnostics <- function(data) {
  # Diagnostics - how much did the 5-year averages improve our coverage?
  data %>%
    summarize(
      missing = sum(is.nan(estimate)),
      valid = sum(!is.nan(estimate)),
      total = n(),
      percent = round(valid / total * 100, 1),
      n_years = length(unique(year[!is.nan(estimate)]))
    )
}


#' @name get_cost_model_simple
#' @title get_cost_model_simple
#'
#' @description
#' A cost model where we take the five year average
#'
#' @importFrom dplyr `%>%` filter summarize
get_cost_model_simple <- function(data = read_csv("data_raw/ce_averages.csv"),
                                  .pollutant = 98, .category_id = 1) {
  # data = read_csv("data_raw/ce_averages.csv"); .pollutant = 98; .category_id = 1

  # For a given pollutant, first, get that subset.

  subset_general <- data %>%
    filter(!is.na(estimate)) %>%
    filter(pollutant %in% .pollutant)

  subset_specific <- subset_general %>%
    filter(category_id %in% .category_id)

  # Check availability of data
  n_general <- nrow(subset_general)
  n_specific <- nrow(subset_specific)

  # If any observations, grab the most recent cost effectiveness stat
  if (n_specific > 0) {
    # Eg. an intercept model
    model_specific <- subset_specific %>%
      filter(year == max(year)) %>%
      lm(formula = estimate ~ 1)
    output <- model_specific
    # If no specific observations but some general observations,
  } else if (n_specific == 0 & n_general > 0) {
    # Make an intercept model from the mean 5-year average cost-effectiveness
    # from the most recent year
    # across policy types
    model_general <- subset_general %>%
      filter(year == max(year)) %>%
      summarize(estimate = mean(estimate)) %>%
      lm(formula = estimate ~ 1)
    output <- model_general
    # If there is no information for that policy OR that pollutant in general
  } else if (n_specific == 0 & n_general == 0) {
    # return NULL
    output <- NULL
  }

  return(output)
}


#' @name get_cost_model
#' @title get_cost_model
#'
#' @importFrom broom glance
#' @importFrom dplyr `%>%` filter
get_cost_model <- function(data = read_csv("data_raw/ce_averages.csv"),
                           .pollutant = 98, .category_id = 1) {
  # Testing Valeus
  # data = read_csv("data_raw/ce_averages.csv"); .pollutant = 98; .category_id = 1

  # For a given pollutant, first, get that subset.

  subset_general <- data %>%
    filter(!is.na(estimate)) %>%
    filter(pollutant %in% .pollutant)

  subset_specific <- subset_general %>%
    filter(category_id %in% .category_id)

  # Check availability of data
  n_general <- nrow(subset_general)
  n_specific <- nrow(subset_specific)

  # If the specific subset has no available data, make the accuracy 0
  if (n_specific == 0) {
    accuracy_specific <- 0

    # Otherwise, fit the model
  } else if (n_specific > 0) {
    # Try making the specific model first
    model_specific <- subset_specific %>%
      lm(formula = log(estimate) ~ year)

    # Get accuracy
    accuracy_specific <- broom::glance(model_specific)$r.squared
  }

  # If the accuracy of this model is less than 50%,
  # and general data is available, try the general model
  if (accuracy_specific < 0.50 & n_general > 0) {
    # Get model
    model_general <- subset_general %>%
      lm(formula = log(estimate) ~ year + factor(category_id))
    # Get accuracy
    accuracy_general <- glance(model_general)$r.squared
    # But if no general data is available...
  } else if (n_general == 0) {
    # Make the general model accuracy 0
    accuracy_general <- 0
    # But if accuracy specific performs well anyways, we don't need the general accuracy, so set to 0.
  } else if (accuracy_specific >= 0.5) {
    accuracy_general <- 0
  }


  # If the general model performs better, make it the output
  if (accuracy_general == 0 & accuracy_specific == 0) {
    # Return null
    output <- NULL

    # If general model is more accurate, return that
  } else if (accuracy_general > accuracy_specific) {
    output <- model_general
    # Otherwise, use the specific model
  } else if (accuracy_specific >= accuracy_general) {
    output <- model_specific
  }

  return(output)
}

#' @name get_cost_effectiveness
#' @title get_cost_effectiveness
#'
#' @description
#' For any pollutant,
#' For any number of policies,
#' For any year range,
#' Get cost effectiveness with the cost model
#'
#' @importFrom tidyr expand_grid
#' @importFrom dplyr `%>%` mutate select distinct n filter group_by reframe bind_rows
get_cost_effectiveness <- function(
    data = read_csv("data_raw/ce_averages.csv"),
    .pollutant = 98, .category_id = 1,
    .start_year = 2020, .end_year = 2025) {
  # Testing values
  # data = read_csv("data_raw/ce_averages.csv"); .pollutant = 98; .category_id = 1; .start_year = 2020; .end_year = 2025
  # library(dplyr)
  # library(tidyr)

  # Get a full grid of predictors
  # grid = expand_grid(
  #   pollutant = .pollutant,
  #   category_id = .category_id,
  #   year = seq(from = .start_year, to = .end_year, by = 1)
  # )

  # Get the distinct pollutant policy pairs
  frame <- expand_grid(pollutant = as.integer(.pollutant), category_id = as.integer(.category_id)) %>%
    select(pollutant, category_id) %>%
    distinct() %>%
    mutate(id = 1:n())

  # Make a blank tibble to hold the results
  grid <- tibble()


  # For each pollutant policy pair,
  for (i in frame$id) {
    # Get best cost model available
    # If log model
    # model = get_cost_model(data = data, .pollutant = frame$pollutant[i], .category_id = frame$category_id[i])
    # If intercept model
    model <- get_cost_model_simple(data = data, .pollutant = frame$pollutant[i], .category_id = frame$category_id[i])

    # If no valid model, leave the insert blank
    if (is.null(model)) {
      insert <- frame %>%
        filter(id == i) %>%
        mutate(value = NA_real_)
      # If valid model, fill it
    } else if (!is.null(model)) {
      # Make predictions
      insert <- frame %>%
        filter(id == i) %>%
        group_by(pollutant, category_id) %>%
        reframe(year = seq(from = .start_year, to = .end_year, by = 1)) %>%
        # Log-model
        # mutate(ce = predict(object = model, newdata = .) %>% exp())
        # Intercept model
        mutate(value = predict(object = model, newdata = .))
    }
    # Bind the insert into the grid
    grid <- bind_rows(grid, insert)
    # Clear the insert and repeat iteratively
    remove(insert)
  }

  grid <- grid %>%
    mutate(pollutant = as.integer(pollutant), category_id = as.integer(category_id), year = as.integer(year))

  return(grid)
}



#' @name check_status
#' @title check_status()
#' @author Tim Fraser
#' @description
#' Function to query the CAT Public API and check the status of CATSERVER.
#' Handy helper function that, if you run before using the optimizer,
#' will ensure that the API is warmed up and ready to go for you.
#' @importFrom httr GET add_headers config timeout
#' @importFrom readr read_csv
#' @export
check_status <- function() {
  base <- "https://api.cat-apps.com/"
  endpoint <- "status/"
  # Build full URL
  url <- paste0(base, endpoint)
  # Add Header
  headers <- add_headers("Content-Type" = "text/csv")
  # Send it!
  # Make the request timeout after 10 seconds.
  result <- GET(url = url, headers, encode = "json", config(timeout(10)))
  # Convert from raw to character
  output <- rawToChar(result$content)
  # Parse as csv
  output <- read_csv(output, show_col_types = FALSE)
  # Return
  return(output)
}

#' @name get_training
#' @title get_training()
#' @author Tim Fraser
#' @description Function to query catserver using CAT Public API.
#' @param .geoid (character) Unique 2 or 5 digit state or county FIPS code. Can only do 1 geoid per request.
#' @param .pollutant (integer) Unique integer EPA pollutant ID code. 98 is CO2 equivalent emissions. Can handle multiple values as a vector of inputs.
#' @param .by (integer) aggregation ID. Defaults to 16 (overall). Now deprecated.
#' @param test (logical) Use test data? Defaults to FALSE. Now deprecated.
#' @importFrom httr GET add_headers
#' @importFrom readr read_csv
get_training <- function(.geoid = "36109", .pollutant = 98, .by = 16, test = FALSE) {
  # Display a warning message
  if (.by != 16) {
    warning("Optimizer currently only supports emissions aggregated overall (aggregation ID .by = 16). You selected a different aggregation level. Defaulting to overall (.by = 16)")
  }
  if (test == TRUE) {
    warning("test argument is deprecated. Ignoring...")
  }

  # Get a training dataset from CATSERVER using CAT Public API

  # Get base URL for api and endpoint
  base <- "https://api.cat-apps.com/"
  endpoint <- "policyoptimizer/v1/retrieve_data/"

  # Build the query
  query <- paste0(
    "?",
    "geoid=", .geoid, "&",
    paste0(paste0("pollutant=", .pollutant), collapse = "&")
  )

  # Build full URL
  url <- paste0(base, endpoint, query)

  # Add Header
  headers <- add_headers("Content-Type" = "text/csv")

  # Send it! Make the request time out after 10 seconds.
  result <- GET(url = url, headers, encode = "json", config(timeout(10)))

  # Convert from raw to character
  output <- rawToChar(result$content)
  # Parse as csv
  output <- read_csv(output, show_col_types = FALSE)

  # Return
  return(output)
}


#' @name get_frame
#' @title `get_frame()`
#' @author Tim Fraser
#' @description
#' Function to get `catdata` interpolated to cover every year in the year-range supplied
#' @importFrom dplyr `%>%`  group_by any_of reframe rename
#' @export
get_frame <- function(data) {
  frame <- data %>%
    group_by(across(any_of(c("pollutant", "geoid")))) %>%
    reframe(
      year_range = seq(from = min(year, na.rm = TRUE), to = max(year, na.rm = TRUE),
                       by = 1),
      base_emissions = approx(x = year, y = emissions, xout = year_range)$y,
      base_vmt = approx(x = year, y = vmt, xout = year_range)$y,
      base_ef = base_emissions / base_vmt
    ) %>%
    rename(year = year_range)

  return(frame)
}


#' @name sim_diff
#' @title sim_diff
#' @description
#' For a given estimation, get back differences with simulated standard errors
#' @importFrom dplyr `%>%` tibble n mutate sym  group_by reframe ungroup summarize left_join select
#' @importFrom broom glance
sim_diff <- function(x0, x1, var = "proxy", model, adjust = -1, n = NULL, fundamental = FALSE) {
  # Bundle data
  data <- tibble(x0, x1) %>%
    mutate(.id = 1:n())
  # Get outcome y0 under baseline conditions x0, in model-transformed units
  data <- data %>% mutate(y0 = predict(object = model, newdata = tibble(!!sym(var) := x0)))
  # Get outcome y1 under new conditions x1, in model-transformed units
  data <- data %>% mutate(y1 = predict(object = model, newdata = tibble(!!sym(var) := x1)))

  # Back transform y1 and y0 and calculate the estimated difference in original units
  data <- data %>%
    mutate(effect = (exp(y1) + adjust) - (exp(y0) + adjust))

  # If n is null, just return the estimate
  if (is.null(n)) {
    output <- data %>% with(estimate)
    return(output)
    # If n is not null, calculate uncertainty
  } else if (is.integer(as.integer(n))) {
    # Get residual standard error
    sigma <- glance(model)$sigma

    # If we are not concerned with fundamental uncertainty,
    # just take a bunch of draws
    if (fundamental == FALSE) {
      # For each item provided, simulate
      sims <- data %>%
        group_by(.id) %>%
        # Get n monte carlo samples
        # from a distribution centered on the prediction,
        # with a standard deviation of the RSME
        reframe(
          sim0 = rnorm(n = n, mean = y0, sd = sigma),
          sim1 = rnorm(n = n, mean = y1, sd = sigma)
        )

      # If we ARE concerned with fundamental uncertainty,
      # Take the average of a bunch of draws
    } else if (fundamental == TRUE) {
      sims <- data %>%
        expand_grid(rep = 1:n) %>%
        group_by(.id, rep) %>%
        reframe(
          sim0 = rnorm(n = n, mean = y0, sd = sigma) %>% median(),
          sim1 = rnorm(n = n, mean = y1, sd = sigma) %>% median(),
        ) %>%
        ungroup()
    }

    # For each item provided, simulate
    uncertainty <- sims %>%
      # Calculate estimated simulated difference in original units
      mutate(simdiff = (exp(sim1) + adjust) - (exp(sim0) + adjust)) %>%
      # For each item...
      group_by(.id) %>%
      # Return uncertainty measures
      summarize(effect_se = sd(simdiff, na.rm = TRUE))

    # Join in the uncertainty statistics
    output <- data %>%
      left_join(by = ".id", y = uncertainty) %>%
      select(.id, effect, effect_se)
    return(output)
  }
}

#' @name get_effects
#' @title get_effects
#' @importFrom dplyr `%>%` select reframe mutate filter left_join contains arrange group_by summarize n rename
#' @importFrom tidyr pivot_wider expand_grid
#' @importFrom purrr map_dfr
get_effects <- function(.geoid = "36109", .pollutants = c(2, 3, 87, 98, 100),
                        var = "epd", path = NULL, prob = 0.50, min_cost = 0,
                        range = 5, .policy = c(1, 2), .start_year = 2020, 
                        .end_year = 2025, .units = 30000, fundamental = FALSE, n = 1000) {
  # .geoid = "36109"; .pollutants = c( 100); .policy = c(1:10); .start_year = 2020; .end_year = 2025; .units = 30000; fundamental = FALSE; n = 1000;
  # path = "data_raw/cmaq.csv"; var = "epd"; prob = 0.50; min_cost = 0; range = 5;

  # Get all cost-effectiveness estimates (usually as emissions per dollar) for each pollutant
  cost_effectiveness <- cmaq_to_percentile(path = path, prob = prob, min_cost = min_cost) %>%
    get_avg(var = var, range = range)

  allpolicies <- connect("policies")

  # ce = get_cost_effectiveness(
  #   data = cost_effectiveness,
  #   .pollutant = .pollutant, .category_id = .policy,
  #   .start_year = .start_year, .end_year = .end_year) %>%
  #   rename(policy = category_id) %>%
  #   # Join in the attributes of those policies,
  #   # namely how much of that policy's effect on emissions
  #   # comes from activity versus ef
  #   left_join(by = c("policy" = "id"),
  #             y = allpolicies %>% select(id, contains("affect_")))

  # Get valid pollutants from CMAQ
  valid_pollutants <- unique(cost_effectiveness$pollutant)
  # Refine these to just valid pollutants that were selected by the user
  valid_pollutants <- valid_pollutants[valid_pollutants %in% .pollutants]

  ce <- valid_pollutants %>%
    map_dfr(~ get_cost_effectiveness(
      data = cost_effectiveness,
      .pollutant = .x, .category_id = .policy,
      .start_year = .start_year, .end_year = .end_year
    )) %>%
    rename(policy = category_id) %>%
    # Join in the attributes of those policies,
    # namely how much of that policy's effect on emissions
    # comes from activity versus ef
    left_join(
      by = c("policy" = "id"),
      y = allpolicies %>% select(id, contains("affect_"))
    )

  # Get training data
  training <- get_training(.geoid = .geoid, .by = "16", .pollutant = valid_pollutants, test = FALSE)
  frame <- get_frame(training)


  # Overwrite the frame
  frame <- frame %>%
    pivot_wider(
      id_cols = c(geoid, year, base_vmt),
      names_from = pollutant, values_from = base_emissions,
      names_prefix = "base_emissions"
    )

  if (2 %in% valid_pollutants) {
    frame <- frame %>% mutate(base_ef2 = base_emissions2 / base_vmt)
  }
  if (3 %in% valid_pollutants) {
    frame <- frame %>% mutate(base_ef3 = base_emissions3 / base_vmt)
  }
  if (87 %in% valid_pollutants) {
    frame <- frame %>% mutate(base_ef87 = base_emissions87 / base_vmt)
  }
  if (98 %in% valid_pollutants) {
    frame <- frame %>% mutate(base_ef98 = base_emissions98 / base_vmt)
  }
  if (100 %in% valid_pollutants) {
    frame <- frame %>% mutate(base_ef100 = base_emissions100 / base_vmt)
  }

  # If the variable of interest is emissions per dollar, do this
  if (var == "epd") {
    ce <- ce %>%
      mutate(effect_emissions = -1 * value) %>%
      mutate(cost = 1) %>%
      select(-value)

    # Otherwise if the variable of interest is cost per ton of emissions, do this
  } else if (var == "ce") {
    ce <- ce %>%
      rename(cost = value) %>%
      mutate(effect_emissions = -1)
  }

  # Pivot out emissions stats into multiple columns
  ce <- ce %>%
    pivot_wider(
      id_cols = c(policy, year, affect_vmt, affect_ef, cost),
      names_from = pollutant, values_from = effect_emissions,
      names_prefix = "effect_emissions"
    )


  # Convert 2022-estimated cost into future cost
  inflation_rate <- 0.033
  start_year <- 2022 # cmaq data was translated into 2022 dollars

  ce <- ce %>%
    # We're going to account for inflation by taking the estimate, which was translated into 2022 dollars,
    # and calculate forward the expected increase in cost due to inflation
    mutate(
      cost = cost * (1 + inflation_rate)^(year - start_year)
    )


  # Create a model of vmt using the desired pollutant's emissions
  # Add 1 mile to suite the scale, so no 0s for natural logs
  m_vmt <- training %>%
    lm(formula = log(vmt + 1) ~ log(emissions) - 1)
  
  get_vmt <- function(x, model) {
    predict(object = model, newdata = tibble(emissions = x)) %>% exp() - 1
  }

  m_ef <- training %>%
    lm(formula = log(ef + 0.001) ~ log(emissions) - 1)
  
  get_ef <- function(x, model) {
    predict(object = model, newdata = tibble(emissions = x)) %>% exp() - 0.001
  }




  # For a given policy in a given year at a given scale...
  # We're going to transform that policy's cost-effectiveness for the proxy pollutant
  # into effect per unit cost in terms of pollutant X

  # Let's do this just one time, for all policies under consideration.
  pdata0 <- expand_grid(
    policy = .policy %>% as.integer(),
    year = seq(from = .start_year, to = .end_year, by = 1) %>% as.integer()
  ) %>%
    # Arrange by year then policy id
    arrange(year, policy) %>%
    # Add an item id
    mutate(.id = 1:n()) %>%
    # join in the cost effectiveness statistics
    left_join(
      by = c("policy" = "policy", "year"),
      y = ce
    ) %>%
    # Let's join some base emissions data in
    left_join(by = c("year"), y = frame)

  # pdata1 = pdata0 %>% mutate(policy_effect =effect_emissions, policy_cost = cost)


  # Get policy effects on vmt and uncertainty
  valid_pollutants

  var_base <- paste0("base_emissions", valid_pollutants)
  var_effect <- paste0("effect_emissions", valid_pollutants)

  # Get estimated effects on VMT from each pollutant
  vmt_data <- 1:length(valid_pollutants) %>%
    map_dfr(~ pdata0 %>%
      select(base_emissions = var_base[.x], effect_emissions = var_effect[.x]) %>%
      with(sim_diff(
        x0 = base_emissions, x1 = base_emissions + effect_emissions, 
        var = "emissions", model = m_vmt,
        adjust = -1, n = n, fundamental = fundamental
      )) %>%
      mutate(pollutant = valid_pollutants[.x]))

  # Pool the VMT estimates across pollutants
  pooled_vmt_data <- vmt_data %>%
    ungroup() %>%
    group_by(.id) %>%
    summarize(
      # Estimate average effect on VMT across pollutants
      effect_vmt = mean(effect),
      # Estimate average standard error across pollutants
      effect_vmt_se = sqrt(sum(effect_se^2))
    )


  # Get estimated effects on EF from each pollutant
  ef_data <- 1:length(valid_pollutants) %>%
    map_dfr(~ pdata0 %>%
      select(base_emissions = var_base[.x], effect_emissions = var_effect[.x]) %>%
      with(sim_diff(
        x0 = base_emissions, x1 = base_emissions + effect_emissions,
        var = "emissions", model = m_ef,
        adjust = -0.01, n = n, fundamental = fundamental
      )) %>%
      mutate(pollutant = valid_pollutants[.x]))

  # Pivot values
  ef_data <- ef_data %>%
    rename(effect_ef = effect, effect_ef_se = effect_se) %>%
    pivot_wider(
      id_cols = c(.id),
      names_from = pollutant,
      values_from = c(effect_ef, effect_ef_se), names_sep = ""
    )

  # Get policy effects on EF and uncertainty
  # se3 = pdata1 %>%
  #   with(sim_diff(x0 = base_emissions, x1 = base_emissions + policy_effect, var = "emissions", model = m_ef, adjust = -1, n = n, fundamental = fundamental))

  # Join back in
  pdata2 <- pdata0 %>%
    # Join in vmt effects
    left_join(by = ".id", y = pooled_vmt_data) %>%
    # Join in emissions factor effects
    left_join(by = ".id", y = ef_data) %>%
    # Adjust for impactfulness of policy on activity vs. emissions factor
    mutate(
      across(.cols = contains("effect_ef"), .fns = ~ .x * affect_ef),
      across(.cols = contains("effect_vmt"), .fns = ~ .x * affect_vmt)
    )


  # The objective is to get a year-by-year effect that we can use and multiply.
  # A big assumption here is linearity, that as you pay more linearly, you will in fact get more direct changes in VMT.
  output <- pdata2 %>%
    select(
      .id, policy, year, affect_vmt, cost,
      contains("effect_emissions"), contains("effect_vmt"), contains("effect_ef"),
      contains("base_emissions"), contains("base_vmt"), contains("base_ef")
    )

  # Add units - most of these need to occur in larger increments to be meaningful.
  output <- output %>%
    mutate(units = .units)

  return(output)
}
