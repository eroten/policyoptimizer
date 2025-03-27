#' @title functions_optimization.R
#' @description
#' This script develops the policy optimizer's optimization functions.
#' It optimizes year by year, with constraints, with an updating tally of your metrics.
#' It allows you to chart out through space your improving objectives.


# OPTIMIZATION FUNCTIONS ############################################
#' @name get_cost
#' @title Cost Function
#' @author Tim Fraser
#'
#' @param policy_cost (numeric) value of policy costs, of length `n`
#' @param units (integer, length 1) constant value for up-scaling scenario implementation level. 30,000 seems to do well for counties.
#' @param d decision object from ompr
#' @param n (integer, length 1) number of policies under consideration.
#' @param prior_cost (double, length 1) cumulative cost from prior year.
#'
#' @importFrom ompr sum_over
get_cost <- function(policy_cost, units, d, n, prior_cost = 0) {
  # Get cost metric
  cost <- sum_over(policy_cost[i] * units[i] * d[i], i = 1:n)
  cost <- cost + prior_cost
  return(cost)
}

#' @name get_reduction
#' @title Emissions Reduction Function
#' @author Tim Fraser
#'
#' @param effect_emissions (numeric) value of policy effects on emissions, of length `n`
#' @param units (integer, length 1) constant value for up-scaling scenario implementation level. 30,000 seems to do well for counties.
#' @param d decision object from ompr
#' @param n (integer, length 1) number of policies under consideration.
#' @param prior_reduction (double, length 1) cumulative emissions reduction from prior year.
#'
#' @importFrom ompr sum_over
get_reduction <- function(effect_emissions, units, d, n, prior_reduction = 0) {
  reduction <- sum_over(
    # Reduction in emisssions minus lower reduction (eg. 25 - 0)
    -1 * (effect_emissions[i] * units[i] * d[i]),
    i = 1:n
  )
  reduction <- reduction + prior_reduction
  return(reduction)
}


#' @name get_effect_emission
#' @title Get Effect on Emissions in given year
#' @author Tim Fraser
#' @description
#' Simple function for getting annual effect on emissions, just for record keeping.
#' See `get_reduction()` for the sub-function used in the objective function.
#' @importFrom ompr sum_over
get_effect_emissions <- function(effect_emissions, units, d, n) {
  reduction <- sum_over(
    # Reduction in emissions minus lower reduction (eg. 25 - 0)
    -1 * (effect_emissions[i] * units[i] * d[i]),
    i = 1:n
  )
  return(reduction)
}

#' @name get_effect_vmt
#' @title Get Effect on VMT in given year
#' @author Tim Fraser
#' @description
#' Simple function for getting annual effect on VMT, just for record keeping.
#' @importFrom ompr sum_over
get_effect_vmt <- function(effect_vmt, units, d, n) {
  stat <- sum_over(
    # Reduction in vmt
    -1 * (effect_vmt[i] * units[i] * d[i]),
    i = 1:n
  )
  return(stat)
}
#' @name get_effect_ef
#' @title Get Effect on Emissions Factor (tons / mile) in given year
#' @author Tim Fraser
#' @description
#' Simple function for getting annual effect on EF, just for record keeping.
#' @importFrom ompr sum_over
get_effect_ef <- function(effect_ef, units, d, n) {
  stat <- sum_over(
    # Reduction in EF
    -1 * (effect_ef[i] * units[i] * d[i]),
    i = 1:n
  )
  return(stat)
}

#' @name scale_cost
#' @title Scale Cost Metric
#' @author Tim Fraser
#' @description
#' Function for rescaling cost metric using a lower cost and upper cost value.
#' Final metric will range from 0 (lowest cost) to 1 (highest cost)
scale_cost <- function(stat, lc, uc) {
  # cost minus lower cost bound
  cost_rescaled <- ((stat - lc) *
    # times range of new bounds (upper-minus-lower)
    (1 - 0) /
    # divided by range of old bounds (upper-min-lower cost)
    (uc - lc) +
    # plus new lower bound
    0)
  return(cost_rescaled)
}
#' @name scale_reduction
#' @title Scale Emissions Reduction Metric
#' @author Tim Fraser
#' @description
#' Function for rescaling reduction metric using a lower reduction and upper reduction value.
#' Final metric will range from 0 (greatest reduction / lowest emissions) to 1 (lowest reduction / highest emissions)
scale_reduction <- function(stat, lr, ur) {
  reduction_rescaled <- (
    (stat - lr) *
      # times range of new bounds
      (0 - 1) /
      # divided by range of old bounds (upper-minus-lower reduction)
      (ur - lr) +
      # plus new lower bound
      1)
  return(reduction_rescaled)
}

#' @name get_diversification
#' @title Diversification Function for 1 year
#' @author Tim Fraser
#' @description
#' Measure diversification in 1 year, by taking the sum of absolute deviations, normalized from 0 to 1.
#' @importFrom ompr sum_over
get_diversification <- function(absolute, n, width, prior_diversification = 0) {
  sum_over(absolute[i] / width, i = 1:n) + prior_diversification
}

#' @name get_z
#' @title Objective Function
#' @author Tim Fraser
#'
#' @description
#' Combine scaled cost and scaled emissions reduction metric using weights `wc` and `wr`.
#'
#' @param cost (numeric) cost metric, from 0 (best) to 1 (worst)
#' @param reduction (numeric) reduction metric, from 0 (best) to 1 (worst)
#' @param wc (percent) weight of cost metric, from 0 (least weight) to 1 (greatest weight)
#' @param wr (percent) weight of reduction metric, from 0 (least weight) to 1 (greatest weight)
get_z <- function(cost = 1, reduction = 1, wc = 0, wr = 0) {
  z <- cost * wc + reduction * wr
  return(z)
}

#' @name check_values_diversification
#' @title Helper Function: Check Values from Diversification Metric
#' @description
#' Takes many random samples of policy scales and computes diversification values for that compilation of values.
#' Allows you to see the range of diversification values.
check_values_diversification <- function(n = 1000, dmax = 5, n_policies = 3) {
  # dmax = 5; n_policies = 3; n = 5
  mat <- matrix(NA, nrow = n, ncol = n_policies)

  ideal <- dmax / n_policies

  for (i in 1:n_policies) {
    # mat[,i] = c(0,dmax,0,0,dmax)
    mat[, i] <- round(runif(n = n, min = 0, max = dmax))
  }
  # Get max width
  width <- (dmax - ideal) * n_policies

  rowSums(abs(mat - ideal) / width)
}

#' @name get_constants
#' @title Get Constants used in Optimization
#' @importFrom dplyr `%>%` filter summarize
#' @importFrom stringr str_detect str_remove
get_constants <- function(effects, training, n_scales = 100, 
                          .year = 2005, .percent_change = 0.50) {
  # Testing values
  # n_scales = 100; .year = 2005; .percent_change = 0.50;

  output <- list()
  # Get upper cost - max cost in ANY year
  output$uc <- sum(effects$cost * n_scales * effects$units)
  # Get lower cost - should usually be no cost
  output$lc <- 0

  # Get pollutant values
  pollutants <- names(effects)[str_detect(names(effects), "effect_emissions[0-9]+")] %>%
    str_remove("effect_emissions") %>%
    as.integer()

  if (98 %in% pollutants) {
    # Get upper emissions - max emissions over time for that county
    output$ue98 <- training %>%
      filter(pollutant == 98, year >= .year) %>%
      with(emissions) %>%
      max()
    # Get lower emissions - should be target a certain percent change less than in specified year
    output$le98 <- training %>%
      filter(pollutant == 98 & year == .year) %>%
      summarize(target = .percent_change * emissions) %>%
      with(target)
    # Max reduction possible is....
    output$ur98 <- output$ue98 - output$le98
    # Lowest reduction possible is...
    output$lr98 <- 0
  }

  if (100 %in% pollutants) {
    # For pollutant 100
    output$ue100 <- training %>%
      filter(pollutant == 100, year >= .year) %>%
      with(emissions) %>%
      max()
    output$le100 <- training %>%
      filter(pollutant == 100 & year == .year) %>%
      summarize(target = .percent_change * emissions) %>%
      with(target)
    output$ur100 <- output$ue100 - output$le100
    output$lr100 <- 0
  }

  if (2 %in% pollutants) {
    # For pollutant 2
    output$ue2 <- training %>%
      filter(pollutant == 2, year >= .year) %>%
      with(emissions) %>%
      max()
    output$le2 <- training %>%
      filter(pollutant == 2 & year == .year) %>%
      summarize(target = .percent_change * emissions) %>%
      with(target)
    output$ur2 <- output$ue2 - output$le2
    output$lr2 <- 0
  }

  if (3 %in% pollutants) {
    # For pollutant 3
    output$ue3 <- training %>%
      filter(pollutant == 3, year >= .year) %>%
      with(emissions) %>%
      max()
    output$le3 <- training %>%
      filter(pollutant == 3 & year == .year) %>%
      summarize(target = .percent_change * emissions) %>%
      with(target)
    output$ur3 <- output$ue3 - output$le3
    output$lr3 <- 0
  }
  if (87 %in% pollutants) {
    # Repeated
    output$ue87 <- training %>%
      filter(pollutant == 87, year >= .year) %>%
      with(emissions) %>%
      max()
    output$le87 <- training %>%
      filter(pollutant == 87 & year == .year) %>%
      summarize(target = .percent_change * emissions) %>%
      with(target)
    output$ur87 <- output$ue87 - output$le87
    output$lr87 <- 0
  }
  return(output)
}

#' @name get_record
#' @title Get Record data.frame used in Optimization
#' @author Tim Fraser
#' @description
#' Construct a data.frame `record` that records values over time.
#' Output has 1 row per year in the scenario, plus placeholders for metrics for each yer.
#' @importFrom dplyr `%>%` as_tibble left_join tibble mutate
#' @importFrom tidyr pivot_wider
get_record <- function(geoid = "36109", start_year = 2025, end_year = 2050, units = 30000,
                       policies = 1:10, pollutants = c(2, 3, 87, 98, 100),
                       last_cost = 0, last_reduction98 = 0, last_reduction2 = 0,
                       last_reduction3 = 0, last_reduction87 = 0, last_reduction100 = 0) {
  # Get training data
  training <- get_training(.geoid = geoid, .by = "16", 
                           .pollutant = pollutants, test = FALSE)
  frame <- get_frame(training)

  # Overwrite the frame
  frame <- frame %>%
    pivot_wider(
      id_cols = c(geoid, year, base_vmt),
      names_from = pollutant, values_from = base_emissions,
      names_prefix = "base_emissions"
    )


  years <- seq(from = start_year, to = end_year, by = 1)
  n_years <- length(years)
  n_policies <- length(policies)
  # Let's make a record over time
  record <- tibble(
    # First comes the matrix of decisions over time, written as p1, p2, p3, etc.
    # number matches policy id number.
    # Always takes up columns 1 to input$n_policies
    matrix(data = NA_real_, nrow = n_years, ncol = n_policies) %>%
      as.data.frame() %>%
      setNames(paste0("p", policies)),
    # Next, metadata.
    year = years,
    units = units,
    prior_cost = c(last_cost, rep(NA_real_, n_years - 1)),
    cost = rep(NA_real_, n_years),
    icost = rep(NA_real_, n_years),
    z = rep(NA_real_, n_years),
    diversification = rep(NA_real_, n_years),
    status = rep(NA_character_, n_years)
  )

  if (98 %in% pollutants) {
    frame <- frame %>% mutate(base_ef98 = base_emissions98 / base_vmt)
    record$prior_reduction98 <- c(last_reduction98, rep(NA_real_, n_years - 1))
    record$reduction98 <- rep(NA_real_, n_years)
    record$ireduction98 <- rep(NA_real_, n_years)
  }

  if (2 %in% pollutants) {
    frame <- frame %>% mutate(base_ef2 = base_emissions2 / base_vmt)
    record$prior_reduction2 <- c(last_reduction98, rep(NA_real_, n_years - 1))
    record$reduction2 <- rep(NA_real_, n_years)
    record$ireduction2 <- rep(NA_real_, n_years)
  }
  if (3 %in% pollutants) {
    frame <- frame %>% mutate(base_ef3 = base_emissions3 / base_vmt)
    record$prior_reduction3 <- c(last_reduction98, rep(NA_real_, n_years - 1))
    record$reduction3 <- rep(NA_real_, n_years)
    record$ireduction3 <- rep(NA_real_, n_years)
  }
  if (87 %in% pollutants) {
    frame <- frame %>% mutate(base_ef87 = base_emissions87 / base_vmt)
    record$prior_reduction87 <- c(last_reduction98, rep(NA_real_, n_years - 1))
    record$reduction87 <- rep(NA_real_, n_years)
    record$ireduction87 <- rep(NA_real_, n_years)
  }
  if (100 %in% pollutants) {
    frame <- frame %>% mutate(base_ef100 = base_emissions100 / base_vmt)
    record$prior_reduction100 <- c(last_reduction98, rep(NA_real_, n_years - 1))
    record$reduction100 <- rep(NA_real_, n_years)
    record$ireduction100 <- rep(NA_real_, n_years)
  }

  record <- record %>%
    # Join in the frame for years in study period.
    left_join(by = "year", y = frame)

  return(record)
}

#' @name get_optimal
#' @title Get Optimal Policy Scenario Record
#' @description
#' Function that executes one slice of of the optimization procedure, updating `.record` with one year filled in.
#'
#' @note These packages must be imported for function to work.
#' @import ompr
#' @import ompr.roi
#' @import ROI
#' @import ROI.plugin.glpk
#'
#' @note Specifically, these functions are used:
#' @importFrom dplyr `%>%`
#' @importFrom stringr str_detect str_remove
#' @importFrom ompr MIPModel add_variable add_constraint set_objective sum_over solve_model
#' @importFrom ompr.roi with_ROI
#'
#' @export
get_optimal <- function(
    .record, .constants, .effects, debug = FALSE, type = "integer", wc = 0.33, wr = 0.33,
    max_annual_policy_scale = 1000, max_abs_diff = 1, min_spending_unit = 0.1,
    # Structural constraints
    min_total_policies = NULL, max_total_policies = NULL,
    min_annual_policies = NULL, max_annual_policies = NULL,
    min_annual_scale = NULL, max_annual_scale = NULL,
    # Metric constraints
    min_total_cost = NULL, max_total_cost = NULL,
    min_annual_cost = 10000, max_annual_cost = NULL,
    min_annual_vmt_chg = NULL,
    # Pollutant Constraints
    min_emissions_chg98 = NULL, min_annual_emissions_chg98 = NULL, min_annual_ef_chg98 = NULL,
    min_emissions_chg2 = NULL, min_annual_emissions_chg2 = NULL, min_annual_ef_chg2 = NULL,
    min_emissions_chg3 = NULL, min_annual_emissions_chg3 = NULL, min_annual_ef_chg3 = NULL,
    min_emissions_chg87 = NULL, min_annual_emissions_chg87 = NULL, min_annual_ef_chg87 = NULL,
    min_emissions_chg100 = NULL, min_annual_emissions_chg100 = NULL, min_annual_ef_chg100 = NULL) {
  # # Testing values
  # setwd(paste0(rstudioapi::getActiveProject(), "/v1"))
  # library(dplyr)
  # library(readr)
  # library(tidyr)
  # library(ompr)
  # library(ompr.roi, warn.conflicts = FALSE, quietly = TRUE)
  # library(ROI)
  # library(ROI.plugin.glpk)
  # devtools::load_all(".")
  #
  # .record = record; .constants = constants; .effects = effects %>% filter(year == min(year)); debug = FALSE; type = "integer"; wc = 0.33; wr = 0.33;
  # max_annual_policy_scale = 3; max_abs_diff = 1; min_spending_unit = 0.1;
  # # Structural constraints
  # min_total_policies = NULL; max_total_policies = NULL;
  # min_annual_policies = NULL; max_annual_policies = NULL;
  # min_annual_scale = NULL; max_annual_scale = NULL;
  # # Metric constraints
  # min_total_cost = NULL; max_total_cost = NULL;
  # min_annual_cost = 10000; max_annual_cost = NULL;
  # min_annual_vmt_chg = NULL;
  # # Pollutant Constraints
  # min_emissions_chg98 = NULL; min_annual_emissions_chg98 = NULL; min_annual_ef_chg98 = NULL;
  # min_emissions_chg2 = NULL; min_annual_emissions_chg2 = NULL; min_annual_ef_chg2 = NULL;
  # min_emissions_chg3 = NULL; min_annual_emissions_chg3 = NULL; min_annual_ef_chg3 = NULL;
  # min_emissions_chg87 = NULL; min_annual_emissions_chg87 = NULL; min_annual_ef_chg87 = NULL;
  # min_emissions_chg100 = NULL; min_annual_emissions_chg100 = NULL; min_annual_ef_chg100 = NULL
  #
  if (debug == TRUE) {
    verbose <- TRUE
  } else {
    verbose <- FALSE
  }

  # Check how many years are in your slice of effects data
  .year <- unique(.effects$year)
  n_years <- length(.year)
  .policies <- unique(.effects$policy)
  n_policies <- length(.policies)
  # If just 1, n should be n policies
  # If 2, n should be n policies * n_years, etc.
  n <- n_policies * n_years

  # Get index of current year in 'records'
  index <- .record$year %in% .year

  # Get record for just that year
  recordyear <- .record[index, ]

  # Get relevant values from this year's data
  .units <- .record[index, ]$units

  # .base_emissions98 = .record[ index, ]$base_emissions98
  # .base_vmt = .record[ index, ]$base_vmt
  # .base_ef = .record[ index, ]$base_ef

  # Get the full year range from the record object
  .yearrange <- unique(.record$year)
  n_yearrange <- length(.yearrange)


  # Rename the max annual policy scale to be dmax
  dmax <- max_annual_policy_scale
  # A balanced scale would be this ideal stat
  ideal <- dmax / n
  # As the denominator for diversification stat, we're going to say...
  # The max possible value in one year would be
  # dmax policy scale, eg. 5
  # for n_policies, eg. 3
  # ideal = 5/3
  # For n policies n in 1 year
  width <- (dmax - ideal) * n

  # Extract pollutant variable names
  # vars_base = names(.record)[str_detect(names(.record), "base_emissions")] %>% sort()
  vars_effect <- names(.effects)[str_detect(names(.effects), "effect_emissions")] %>% sort()
  # Extract pollutant ids
  pollutant <- str_remove(vars_effect, "effect_emissions") %>% as.integer()

  # Model
  m <- MIPModel() %>%
    # Decision variables
    add_variable(d[i], i = 1:n, type = "integer", lb = 0, ub = dmax) %>%
    add_variable(b[i], i = 1:n, type = "binary") %>%
    # Constraint to enforce b[i] is 1 if d[i] > 0
    add_constraint(d[i] <= dmax * b[i], i = 1:n) %>%
    add_constraint(d[i] >= 1 * b[i], i = 1:n) %>%
    # Constraint to limit the number of times d[i] is greater than 0 to less than or equal to the number of policies
    add_constraint(sum_expr(b[i], i = 1:n) <= n) %>%
    add_constraint(sum_expr(b[i], i = 1:n) >= 0) %>%
    # You should make SOME action each year
    add_constraint(sum_over(d[i], i = 1:n) >= 1) %>%
    # You must spend SOMETHING each year
    add_constraint(sum_over(d[i] * .effects$cost[i] * .units, i = 1:n) >= min_annual_cost) %>%
    # Make absolute variable 'absolute', representing the absolute difference between d[i] and a point ideal
    add_variable(absolute[i], i = 1:n, type = "continuous", lb = 0, ub = (dmax - ideal)) %>%
    # y >= x
    add_constraint(absolute[i] >= d[i] - ideal, i = 1:n) %>%
    # y >= -x
    add_constraint(absolute[i] >= -1 * (d[i] - ideal), i = 1:n) %>%
    # Try to reduce the absolute difference to within THIS threshold
    add_constraint(sum_over(absolute[i] / width, i = 1:n) <= max_abs_diff)

  # METRIC CONSTRAINTS ###########################

  # Get the prior cost and reduction values from preceding year
  .prior_cost <- .record[index, ]$prior_cost

  # If min total cost provided...
  if (!is.null(min_total_cost)) {
    # Should cost more than the min total cost
    m <- m %>% add_constraint(sum_over(d[i] * .effects$cost[i] * .units, i = 1:n) + .prior_cost >= min_total_cost)
  }

  # If max total cost provided
  if (!is.null(max_total_cost)) {
    # Should cost less than the max total cost
    m <- m %>% add_constraint(sum_over(d[i] * .effects$cost[i] * .units, i = 1:n) + .prior_cost <= max_total_cost)
  }

  # If min annual cost provided
  if (!is.null(min_annual_cost)) {
    # Should cost more than the min annual cost
    m <- m %>% add_constraint(sum_over(d[i] * .effects$cost[i] * .units, i = 1:n) >= min_annual_cost)
  }

  # If max annual cost provided
  if (!is.null(max_annual_cost)) {
    # Should cost no more than the max annual cost
    m <- m %>% add_constraint(sum_over(d[i] * .effects$cost[i] * .units, i = 1:n) <= max_annual_cost)
  }

  # If minimum annual vmt change provided...
  if (!is.null(min_annual_vmt_chg)) {
    # Should have an annual effect on vmt of N miles or greater
    m <- m %>% add_constraint(-1 * sum_over(.effects$effect_vmt[i] * d[i] * .units, i = 1:n) >= min_annual_vmt_chg)
  }

  # POLLUTANT CONSTRAINTS #########################

  # You should make SOME reduction each year
  if (98 %in% pollutant) {
    m <- m %>% add_constraint(sum_over(d[i] * .effects$effect_emissions98[i] * .units, i = 1:n) <= 1)
    .prior_reduction98 <- .record[index, ]$prior_reduction98

    # If min emissions reduction provided  (cumulative reduction)
    if (!is.null(min_emissions_chg98)) {
      # Should have an emissions reduction value of N tons or greater
      m <- m %>% add_constraint(-1 * sum_over(.effects$effect_emissions98[i] * d[i] * .units, i = 1:n) + .prior_reduction98 >= min_emissions_chg98)
    }
    # If min emissions reduction provided  (annual reduction)
    if (!is.null(min_annual_emissions_chg98)) {
      # Should have an emissions reduction value of N tons or greater
      m <- m %>% add_constraint(-1 * sum_over(.effects$effect_emissions98[i] * d[i] * .units, i = 1:n) >= min_annual_emissions_chg98)
    }
    # If minimum annual emissions factor change provided...
    if (!is.null(min_annual_ef_chg98)) {
      # Should have an effect on EF of N tons per mile or greater
      m <- m %>% add_constraint(-1 * sum_over(.effects$effect_ef98[i] * d[i] * .units, i = 1:n) >= min_annual_ef_chg98)
    }
  }


  if (2 %in% pollutant) {
    m <- m %>% add_constraint(sum_over(d[i] * .effects$effect_emissions2[i] * .units, i = 1:n) <= 1)
    .prior_reduction2 <- .record[index, ]$prior_reduction2

    # If min emissions reduction provided  (cumulative reduction)
    if (!is.null(min_emissions_chg2)) {
      # Should have an emissions reduction value of N tons or greater
      m <- m %>% add_constraint(-1 * sum_over(.effects$effect_emissions2[i] * d[i] * .units, i = 1:n) + .prior_reduction2 >= min_emissions_chg2)
    }
    # If min emissions reduction provided  (annual reduction)
    if (!is.null(min_annual_emissions_chg2)) {
      # Should have an emissions reduction value of N tons or greater
      m <- m %>% add_constraint(-1 * sum_over(.effects$effect_emissions2[i] * d[i] * .units, i = 1:n) >= min_annual_emissions_chg2)
    }
    # If minimum annual emissions factor change provided...
    if (!is.null(min_annual_ef_chg2)) {
      # Should have an effect on EF of N tons per mile or greater
      m <- m %>% add_constraint(-1 * sum_over(.effects$effect_ef2[i] * d[i] * .units, i = 1:n) >= min_annual_ef_chg2)
    }
  }

  if (3 %in% pollutant) {
    m <- m %>% add_constraint(sum_over(d[i] * .effects$effect_emissions3[i] * .units, i = 1:n) <= 1)
    .prior_reduction3 <- .record[index, ]$prior_reduction3
    # If min emissions reduction provided  (cumulative reduction)
    if (!is.null(min_emissions_chg3)) {
      # Should have an emissions reduction value of N tons or greater
      m <- m %>% add_constraint(-1 * sum_over(.effects$effect_emissions3[i] * d[i] * .units, i = 1:n) + .prior_reduction3 >= min_emissions_chg3)
    }
    # If min emissions reduction provided  (annual reduction)
    if (!is.null(min_annual_emissions_chg3)) {
      # Should have an emissions reduction value of N tons or greater
      m <- m %>% add_constraint(-1 * sum_over(.effects$effect_emissions3[i] * d[i] * .units, i = 1:n) >= min_annual_emissions_chg3)
    }
    # If minimum annual emissions factor change provided...
    if (!is.null(min_annual_ef_chg3)) {
      # Should have an effect on EF of N tons per mile or greater
      m <- m %>% add_constraint(-1 * sum_over(.effects$effect_ef3[i] * d[i] * .units, i = 1:n) >= min_annual_ef_chg3)
    }
  }

  if (100 %in% pollutant) {
    m <- m %>% add_constraint(sum_over(d[i] * .effects$effect_emissions100[i] * .units, i = 1:n) <= 1)
    .prior_reduction100 <- .record[index, ]$prior_reduction100
    # If min emissions reduction provided  (cumulative reduction)
    if (!is.null(min_emissions_chg100)) {
      # Should have an emissions reduction value of N tons or greater
      m <- m %>% add_constraint(-1 * sum_over(.effects$effect_emissions100[i] * d[i] * .units, i = 1:n) + .prior_reduction100 >= min_emissions_chg100)
    }
    # If min emissions reduction provided  (annual reduction)
    if (!is.null(min_annual_emissions_chg100)) {
      # Should have an emissions reduction value of N tons or greater
      m <- m %>% add_constraint(-1 * sum_over(.effects$effect_emissions100[i] * d[i] * .units, i = 1:n) >= min_annual_emissions_chg100)
    }
    # If minimum annual emissions factor change provided...
    if (!is.null(min_annual_ef_chg100)) {
      # Should have an effect on EF of N tons per mile or greater
      m <- m %>% add_constraint(-1 * sum_over(.effects$effect_ef100[i] * d[i] * .units, i = 1:n) >= min_annual_ef_chg100)
    }
  }

  if (87 %in% pollutant) {
    m <- m %>% add_constraint(sum_over(d[i] * .effects$effect_emissions87[i] * .units, i = 1:n) <= 1)
    .prior_reduction87 <- .record[index, ]$prior_reduction87
    # If min emissions reduction provided  (cumulative reduction)
    if (!is.null(min_emissions_chg87)) {
      # Should have an emissions reduction value of N tons or greater
      m <- m %>% add_constraint(-1 * sum_over(.effects$effect_emissions87[i] * d[i] * .units, i = 1:n) + .prior_reduction87 >= min_emissions_chg87)
    }
    # If min emissions reduction provided  (annual reduction)
    if (!is.null(min_annual_emissions_chg87)) {
      # Should have an emissions reduction value of N tons or greater
      m <- m %>% add_constraint(-1 * sum_over(.effects$effect_emissions87[i] * d[i] * .units, i = 1:n) >= min_annual_emissions_chg87)
    }
    # If minimum annual emissions factor change provided...
    if (!is.null(min_annual_ef_chg87)) {
      # Should have an effect on EF of N tons per mile or greater
      m <- m %>% add_constraint(-1 * sum_over(.effects$effect_ef87[i] * d[i] * .units, i = 1:n) >= min_annual_ef_chg87)
    }
  }



  # STRUCTURAL CONSTRAINTS ##########################

  # If min annual policies provided
  # If there's a minimum number of policy types we want to adopt in this time period...
  if (!is.null(min_annual_policies)) {
    # Should have at least N policies annually
    m <- m %>% add_constraint(sum_over(b[i], i = 1:n) >= min_annual_policies)
  }
  # If max annual policies provided
  if (!is.null(max_annual_policies)) {
    # Should have less than or equal to N policies annually
    # Eg. scale of implementation should be at least 1 or more
    m <- m %>% add_constraint(sum_over(b[i], i = 1:n) <= max_annual_policies)
  }

  # If min total policies provided
  if (!is.null(min_total_policies)) {
    # Get prior total policies to date
    .prior_total_policies <- sum(.record[, 1:n] > 0, na.rm = TRUE)
    # Should have at least N policies totla
    m <- m %>% add_constraint(sum_over(b[i], i = 1:n) + .prior_total_policies >= min_total_policies)
  }

  # If max total policies provided
  if (!is.null(max_total_policies)) {
    # Get prior total policies to date
    .prior_total_policies <- sum(.record[, 1:n] > 0, na.rm = TRUE)
    # Should have less than or equal to N policies totla
    m <- m %>% add_constraint(sum_over(b[i], i = 1:n) + .prior_total_policies <= max_total_policies)
  }

  # If a min annual scale is provided.
  if (!is.null(min_annual_scale)) {
    m <- m %>% add_constraint(sum_over(d[i], i = 1:n) >= min_annual_scale)
  }

  # Max annual scale
  if (!is.null(max_annual_scale)) {
    m <- m %>% add_constraint(sum_over(d[i], i = 1:n) <= max_annual_scale)
  }


  # OBJECTIVE #########################

  # If pollutant is JUST 98
  if (all(98 == pollutant)) {
    m <- m %>%
      set_objective(
        get_z(
          cost = get_cost(policy_cost = .effects$cost, units = .effects$units, d = d, n = n, prior_cost = .prior_cost) %>%
            scale_cost(lc = .constants$lc, uc = .constants$uc),
          wc = wc,
          reduction = get_reduction(effect_emissions = .effects$effect_emissions98, units = .effects$units, d = d, n = n, prior_reduction = .prior_reduction98) %>%
            scale_reduction(lr = .constants$lr98, ur = .constants$ur98) * 0.25 +
            get_reduction(effect_emissions = .effects$effect_emissions98, units = .effects$units, d = d, n = n, prior_reduction = .prior_reduction98) %>%
            scale_reduction(lr = .constants$lr98, ur = .constants$ur98),
          # Then weight the overall pollutant combo same as usual
          wr = wr
        ),
        sense = "min"
      )
    # If pollutant is JUST 2
  } else if (all(2 == pollutant)) {
    m <- m %>%
      set_objective(
        get_z(
          cost = get_cost(policy_cost = .effects$cost, units = .effects$units, d = d, n = n, prior_cost = .prior_cost) %>%
            scale_cost(lc = .constants$lc, uc = .constants$uc),
          wc = wc,
          reduction = get_reduction(effect_emissions = .effects$effect_emissions2, units = .effects$units, d = d, n = n, prior_reduction = .prior_reduction2) %>%
            scale_reduction(lr = .constants$lr2, ur = .constants$ur2),
          wr = wr
        ),
        sense = "min"
      )
    # If pollutant is JUST 3
  } else if (all(3 == pollutant)) {
    m <- m %>%
      set_objective(
        get_z(
          cost = get_cost(policy_cost = .effects$cost, units = .effects$units, d = d, n = n, prior_cost = .prior_cost) %>%
            scale_cost(lc = .constants$lc, uc = .constants$uc),
          wc = wc,
          reduction = get_reduction(effect_emissions = .effects$effect_emissions3, units = .effects$units, d = d, n = n, prior_reduction = .prior_reduction3) %>%
            scale_reduction(lr = .constants$lr3, ur = .constants$ur3),
          wr = wr
        ),
        sense = "min"
      )
    # If pollutant is JUST 100
  } else if (all(100 == pollutant)) {
    m <- m %>%
      set_objective(
        get_z(
          cost = get_cost(policy_cost = .effects$cost, units = .effects$units, d = d, n = n, prior_cost = .prior_cost) %>%
            scale_cost(lc = .constants$lc, uc = .constants$uc),
          wc = wc,
          reduction = get_reduction(effect_emissions = .effects$effect_emissions100, units = .effects$units, d = d, n = n, prior_reduction = .prior_reduction100) %>%
            scale_reduction(lr = .constants$lr100, ur = .constants$ur100),
          wr = wr
        ),
        sense = "min"
      )
    # If pollutant is JUST 87
  } else if (all(87 == pollutant)) {
    m <- m %>%
      set_objective(
        get_z(
          cost = get_cost(policy_cost = .effects$cost, units = .effects$units, d = d, n = n, prior_cost = .prior_cost) %>%
            scale_cost(lc = .constants$lc, uc = .constants$uc),
          wc = wc,
          reduction = get_reduction(effect_emissions = .effects$effect_emissions87, units = .effects$units, d = d, n = n, prior_reduction = .prior_reduction87) %>%
            scale_reduction(lr = .constants$lr87, ur = .constants$ur87),
          wr = wr
        ),
        sense = "min"
      )


    # Next, if pollutant has these 3 values...
    # For example
    # all(c(2,3,98) %in% c(2,3,98)) # should be true
    # all(c(2,3,98,100,87) %in% c(2,3,98)) # should be false
  } else if (all(c(2, 3, 98) %in% pollutant)) {
    m <- m %>%
      set_objective(
        get_z(
          cost = get_cost(policy_cost = .effects$cost, units = .effects$units, d = d, n = n, prior_cost = .prior_cost) %>%
            scale_cost(lc = .constants$lc, uc = .constants$uc),
          wc = wc,
          reduction = get_reduction(effect_emissions = .effects$effect_emissions98, units = .effects$units, d = d, n = n, prior_reduction = .prior_reduction98) %>%
            scale_reduction(lr = .constants$lr98, ur = .constants$ur98) * 1 / 3 +
            get_reduction(effect_emissions = .effects$effect_emissions2, units = .effects$units, d = d, n = n, prior_reduction = .prior_reduction2) %>%
            scale_reduction(lr = .constants$lr2, ur = .constants$ur2) * 1 / 3 +
            get_reduction(effect_emissions = .effects$effect_emissions3, units = .effects$units, d = d, n = n, prior_reduction = .prior_reduction3) %>%
            scale_reduction(lr = .constants$lr3, ur = .constants$ur3) * 1 / 3,
          # Then weight the overall pollutant combo same as usual
          wr = wr
        ),
        sense = "min"
      )

    # Shorter combos must always come BEFORE in the if-else statement
    # all(c(2,3,98) %in% c(2,3,98,100)) # also true...
    # all(c(2,3,98,100) %in% c(2,3,98,100)) # should be true
    # all(c(2,3,98,100,87) %in% c(2,3,98,100)) # should be false
  } else if (all(c(2, 3, 98, 100) %in% pollutant)) {
    m <- m %>%
      set_objective(
        get_z(
          cost = get_cost(policy_cost = .effects$cost, units = .effects$units, d = d, n = n, prior_cost = .prior_cost) %>%
            scale_cost(lc = .constants$lc, uc = .constants$uc),
          wc = wc,
          reduction = get_reduction(effect_emissions = .effects$effect_emissions98, units = .effects$units, d = d, n = n, prior_reduction = .prior_reduction98) %>%
            scale_reduction(lr = .constants$lr98, ur = .constants$ur98) * 0.25 +
            get_reduction(effect_emissions = .effects$effect_emissions2, units = .effects$units, d = d, n = n, prior_reduction = .prior_reduction2) %>%
            scale_reduction(lr = .constants$lr2, ur = .constants$ur2) * 0.25 +
            get_reduction(effect_emissions = .effects$effect_emissions3, units = .effects$units, d = d, n = n, prior_reduction = .prior_reduction3) %>%
            scale_reduction(lr = .constants$lr3, ur = .constants$ur3) * 0.25 +
            get_reduction(effect_emissions = .effects$effect_emissions100, units = .effects$units, d = d, n = n, prior_reduction = .prior_reduction100) %>%
            scale_reduction(lr = .constants$lr100, ur = .constants$ur100) * 0.25,
          # Then weight the overall pollutant combo same as usual
          wr = wr
        ),
        sense = "min"
      )

    # all(c(2,3,98) %in% c(2,3,98,87)) # also true...
    # all(c(2,3,98,87) %in% c(2,3,98,87)) # should be true
    # all(c(2,3,98,100,87) %in% c(2,3,98,87)) # should be false
  } else if (all(c(2, 3, 98, 87) %in% pollutant)) {
    m <- m %>%
      set_objective(
        get_z(
          cost = get_cost(policy_cost = .effects$cost, units = .effects$units, d = d, n = n, prior_cost = .prior_cost) %>%
            scale_cost(lc = .constants$lc, uc = .constants$uc),
          wc = wc,
          reduction = get_reduction(effect_emissions = .effects$effect_emissions98, units = .effects$units, d = d, n = n, prior_reduction = .prior_reduction98) %>%
            scale_reduction(lr = .constants$lr98, ur = .constants$ur98) * 0.25 +
            get_reduction(effect_emissions = .effects$effect_emissions2, units = .effects$units, d = d, n = n, prior_reduction = .prior_reduction2) %>%
            scale_reduction(lr = .constants$lr2, ur = .constants$ur2) * 0.25 +
            get_reduction(effect_emissions = .effects$effect_emissions3, units = .effects$units, d = d, n = n, prior_reduction = .prior_reduction3) %>%
            scale_reduction(lr = .constants$lr3, ur = .constants$ur3) * 0.25 +
            get_reduction(effect_emissions = .effects$effect_emissions87, units = .effects$units, d = d, n = n, prior_reduction = .prior_reduction87) %>%
            scale_reduction(lr = .constants$lr87, ur = .constants$ur87) * 0.25,
          # Then weight the overall pollutant combo same as usual
          wr = wr
        ),
        sense = "min"
      )

    # Must come last
    # all(c(2,3,98,87,100) %in% c(2,3,98) )
    # all(c(2,3,98,87,100) %in% c(2,3,98,87) )
    # all(c(2,3,98,87,100) %in% c(2,3,98,87,100) )
  } else if (all(c(2, 3, 98, 87, 100) %in% pollutant)) {
    m <- m %>%
      set_objective(
        get_z(
          cost = get_cost(policy_cost = .effects$cost, units = .effects$units, d = d, n = n, prior_cost = .prior_cost) %>%
            scale_cost(lc = .constants$lc, uc = .constants$uc),
          wc = wc,
          reduction = get_reduction(effect_emissions = .effects$effect_emissions98, units = .effects$units, d = d, n = n, prior_reduction = .prior_reduction98) %>%
            scale_reduction(lr = .constants$lr98, ur = .constants$ur98) * 0.20 +
            get_reduction(effect_emissions = .effects$effect_emissions2, units = .effects$units, d = d, n = n, prior_reduction = .prior_reduction2) %>%
            scale_reduction(lr = .constants$lr2, ur = .constants$ur2) * 0.20 +
            get_reduction(effect_emissions = .effects$effect_emissions3, units = .effects$units, d = d, n = n, prior_reduction = .prior_reduction3) %>%
            scale_reduction(lr = .constants$lr3, ur = .constants$ur3) * 0.20 +
            get_reduction(effect_emissions = .effects$effect_emissions100, units = .effects$units, d = d, n = n, prior_reduction = .prior_reduction100) %>%
            scale_reduction(lr = .constants$lr100, ur = .constants$ur100) * 0.20 +
            get_reduction(effect_emissions = .effects$effect_emissions87, units = .effects$units, d = d, n = n, prior_reduction = .prior_reduction87) %>%
            scale_reduction(lr = .constants$lr87, ur = .constants$ur87) * 0.20,
          # Then weight the overall pollutant combo same as usual
          wr = wr
        ),
        sense = "min"
      )
  }


  result <- m %>%
    solve_model(with_ROI(solver = "glpk", verbose = verbose))

  # RECORDING ################################
  # result$status
  # result$solution
  #
  # result$objective_value
  # result$solution
  # Record the model status in the requisite cells in the record data.frame
  .record[index, "status"] <- result$status

  # Record the solution (d values) in the requisite cells in the record data.frame
  .d <- result$solution[paste0("d[", 1:n, "]")]
  .record[index, 1:n_policies] <- t(.d)

  # Set the value for cost
  .cost <- get_cost(policy_cost = .effects$cost, units = .effects$units, d = .d, n = n, prior_cost = .prior_cost)
  .record[index, "cost"] <- .cost
  # Rescale cost
  .cost_rescaled <- scale_cost(stat = .cost, lc = .constants$lc, uc = .constants$uc)
  .record[index, "icost"] <- .cost_rescaled

  # As long as you're not yet to the end year, record the prior cost and reduction for next year
  next_index <- .record$year %in% (.year + 1)
  if (.year < max(.record$year)) {
    .record[next_index, "prior_cost"] <- .cost
  }

  # Set the value for reduction
  if (98 %in% pollutant) {
    .reduction98 <- get_reduction(effect_emissions = .effects$effect_emissions98, units = .effects$units, d = .d, n = n, prior_reduction = .prior_reduction98)
    .record[index, "reduction98"] <- .reduction98
    # Rescale reduction
    .reduction_rescaled98 <- scale_reduction(.reduction98, lr = .constants$lr98, ur = .constants$ur98)
    .record[index, "ireduction98"] <- .reduction_rescaled98

    # Estimate current emissions after accounting for cumulative reduction
    .record[index, "emissions98"] <- -1 * .reduction98 + .record[index, ]$base_emissions98
    # Estimate annual effect on emissions, noncumulative
    .record[index, "effect_emissions98"] <- get_effect_emissions(effect_emissions = .effects$effect_emissions98, units = .effects$units, d = .d, n = n)
    # Estimate annual effect on EF, noncumulative
    .record[index, "effect_ef98"] <- get_effect_ef(effect_ef = .effects$effect_ef98, units = .effects$units, d = .d, n = n)

    if (.year < max(.record$year)) {
      .record[next_index, "prior_reduction98"] <- .reduction98
    }
  }

  if (2 %in% pollutant) {
    .reduction2 <- get_reduction(effect_emissions = .effects$effect_emissions2, units = .effects$units, d = .d, n = n, prior_reduction = .prior_reduction2)
    .record[index, "reduction2"] <- .reduction2
    .reduction_rescaled2 <- scale_reduction(.reduction2, lr = .constants$lr2, ur = .constants$ur2)
    .record[index, "ireduction2"] <- .reduction_rescaled2

    # Estimate current emissions after accounting for cumulative reduction
    .record[index, "emissions2"] <- -1 * .reduction2 + .record[index, ]$base_emissions2
    # Estimate annual effect on emissions, noncumulative
    .record[index, "effect_emissions2"] <- get_effect_emissions(effect_emissions = .effects$effect_emissions2, units = .effects$units, d = .d, n = n)
    # Estimate annual effect on EF, noncumulative
    .record[index, "effect_ef2"] <- get_effect_ef(effect_ef = .effects$effect_ef2, units = .effects$units, d = .d, n = n)

    if (.year < max(.record$year)) {
      .record[next_index, "prior_reduction2"] <- .reduction2
    }
  }

  if (3 %in% pollutant) {
    .reduction3 <- get_reduction(effect_emissions = .effects$effect_emissions3, units = .effects$units, d = .d, n = n, prior_reduction = .prior_reduction3)
    .record[index, "reduction3"] <- .reduction3
    .reduction_rescaled3 <- scale_reduction(.reduction2, lr = .constants$lr3, ur = .constants$ur3)
    .record[index, "ireduction3"] <- .reduction_rescaled3

    # Estimate current emissions after accounting for cumulative reduction
    .record[index, "emissions3"] <- -1 * .reduction3 + .record[index, ]$base_emissions3
    # Estimate annual effect on emissions, noncumulative
    .record[index, "effect_emissions3"] <- get_effect_emissions(effect_emissions = .effects$effect_emissions3, units = .effects$units, d = .d, n = n)
    # Estimate annual effect on EF, noncumulative
    .record[index, "effect_ef3"] <- get_effect_ef(effect_ef = .effects$effect_ef3, units = .effects$units, d = .d, n = n)
    if (.year < max(.record$year)) {
      .record[next_index, "prior_reduction3"] <- .reduction3
    }
  }

  if (100 %in% pollutant) {
    .reduction100 <- get_reduction(effect_emissions = .effects$effect_emissions100, units = .effects$units, d = .d, n = n, prior_reduction = .prior_reduction100)
    .record[index, "reduction100"] <- .reduction100
    .reduction_rescaled100 <- scale_reduction(.reduction100, lr = .constants$lr100, ur = .constants$ur100)
    .record[index, "ireduction100"] <- .reduction_rescaled100

    # Estimate current emissions after accounting for cumulative reduction
    .record[index, "emissions100"] <- -1 * .reduction100 + .record[index, ]$base_emissions100
    # Estimate annual effect on emissions, noncumulative
    .record[index, "effect_emissions100"] <- get_effect_emissions(effect_emissions = .effects$effect_emissions100, units = .effects$units, d = .d, n = n)
    # Estimate annual effect on EF, noncumulative
    .record[index, "effect_ef100"] <- get_effect_ef(effect_ef = .effects$effect_ef100, units = .effects$units, d = .d, n = n)
    if (.year < max(.record$year)) {
      .record[next_index, "prior_reduction100"] <- .reduction100
    }
  }

  if (87 %in% pollutant) {
    .reduction87 <- get_reduction(effect_emissions = .effects$effect_emissions87, units = .effects$units, d = .d, n = n, prior_reduction = .prior_reduction87)
    .record[index, "reduction87"] <- .reduction87
    .reduction_rescaled87 <- scale_reduction(.reduction87, lr = .constants$lr87, ur = .constants$ur87)
    .record[index, "ireduction87"] <- .reduction_rescaled87

    # Estimate current emissions after accounting for cumulative reduction
    .record[index, "emissions87"] <- -1 * .reduction87 + .record[index, ]$base_emissions87
    # Estimate annual effect on emissions, noncumulative
    .record[index, "effect_emissions87"] <- get_effect_emissions(effect_emissions = .effects$effect_emissions87, units = .effects$units, d = .d, n = n)
    # Estimate annual effect on EF, noncumulative
    .record[index, "effect_ef87"] <- get_effect_ef(effect_ef = .effects$effect_ef87, units = .effects$units, d = .d, n = n)
    if (.year < max(.record$year)) {
      .record[next_index, "prior_reduction87"] <- .reduction87
    }
  }




  # Record the objective
  .record[index, "z"] <- result$objective

  # Calculate extra values

  # Set the value for diversification - 0 means excellently diversified; 1 means super not-diversified.
  .diversification <- sum(abs(.d - ideal) / width)
  .record[index, "diversification"] <- .diversification

  # Estimate annual effect on VMT, noncumulative
  .record[index, "effect_vmt"] <- get_effect_vmt(effect_vmt = .effects$effect_vmt, units = .effects$units, d = .d, n = n)


  # .record$z
  # .record  %>% View()
  if (debug == TRUE) {
    return(list(record = .record, result = result))
  } else if (debug == FALSE) {
    return(.record)
  }
}


#' @name policy_optimizer
#' @title Policy Optimizer Function
#' @author Tim Fraser
#' @description
#' Find the optimal policy scenario according to cost, emissions reduction, and related constraints.
#' @importFrom dplyr `%>%` left_join mutate n group_by summarize filter
#' @export
policy_optimizer <- function(
    # Basic Metadata
    geoid = "36109",
    pollutants = c(98, 2, 3, 100),
    start_year = 2025, end_year = 2035,
    policies = 1:5,
    n_scales = 1000,
    units = 1,
    wc = 0.5, wr = 0.5,
    target_year = 2005, target_change = 0.50,
    # Cost Effectiveness Stats
    var = "epd", prob = 0.50, range = 5, cmaq_path = NULL,
    # Any prior cumulative costs / reductions
    last_cost = 0, last_reduction98 = 0, last_reduction2 = 0,
    last_reduction3 = 0, last_reduction100 = 0, last_reduction87 = 0,
    # Mandatory constraining values. Must make sense.
    # Can be supplied as 1 value or n years worth of values
    max_abs_diff = 1,
    # Structural constraints
    min_total_policies = NULL, max_total_policies = NULL,
    min_annual_policies = NULL, max_annual_policies = NULL,
    min_annual_scale = NULL, max_annual_scale = NULL,
    # Metric constraints
    min_total_cost = NULL, max_total_cost = NULL,
    min_annual_cost = 10000, max_annual_cost = NULL,
    min_annual_vmt_chg = NULL,
    # Pollutant Constraints
    min_emissions_chg98 = NULL, min_annual_emissions_chg98 = NULL, min_annual_ef_chg98 = NULL,
    min_emissions_chg2 = NULL, min_annual_emissions_chg2 = NULL, min_annual_ef_chg2 = NULL,
    min_emissions_chg3 = NULL, min_annual_emissions_chg3 = NULL, min_annual_ef_chg3 = NULL,
    min_emissions_chg87 = NULL, min_annual_emissions_chg87 = NULL, min_annual_ef_chg87 = NULL,
    min_emissions_chg100 = NULL, min_annual_emissions_chg100 = NULL, min_annual_ef_chg100 = NULL,
    ...) {
  # Message
  # geoid = "36109"; pollutants = c(98, 100); start_year = 2025; end_year = 2030
  message <- paste0("\noptimizer | geoid: ", geoid, " | pollutant: ", paste0(get_pollutant(pollutants), collapse = ", "), " | years: ", start_year, "-", end_year, "\n")
  cat(message)

  # # #  Testing Values
  # geoid = "36109";
  # pollutant = 98; by = 16;
  # start_year = 2025; end_year = 2035;
  # policies = 1:10;
  # n_scales = 1000;
  # units = 1;
  # # Cost Effectiveness Stats
  # var = "ce";
  # prob = 0.50;
  # range = 5;
  # cmaq_path = "data_raw/cmaq.csv";
  # # Any prior cumulative costs / reductions
  # last_cost = 0;
  # last_reduction = 0;
  # last_diversification = 0;
  # target_year = 2005;
  # target_change = 0.50;
  #
  # # Mandatory constraining values. Must make sense.
  # # Can be supplied as 1 value or n years worth of values
  # min_total_cost = NULL;
  # max_total_cost = NULL;
  # min_annual_cost = NULL;
  # max_annual_cost = NULL;
  # min_annual_policies = NULL;
  # max_annual_policies = NULL;
  # min_total_policies = NULL;
  # max_total_policies = NULL
  # min_emissions_chg = NULL;
  # min_annual_emissions_chg = NULL;
  # wc = 0.33; wr = 0.33; wd = 0.33

  # Get the catdata sample for both the pollutant and the proxy
  # training = get_training(.geoid = geoid, .pollutant = pollutant, .by = by, test = FALSE)

  # Approximate over time
  # frame = get_frame(data = training)

  training <- get_training(.geoid = geoid, .pollutant = pollutants, .by = 16, test = FALSE)

  effects <- get_effects(
    .geoid = geoid, .pollutants = pollutants,
    .policy = policies,
    .start_year = start_year,
    .end_year = end_year,
    .units = units,
    var = var,
    prob = prob,
    min_cost = 0,
    range = range,
    path = cmaq_path
  )
  
  record <- get_record(
    geoid = geoid,
    pollutants = pollutants,
    policies = policies,
    start_year = start_year,
    end_year = end_year,
    units = units,
    last_cost = last_cost,
    last_reduction98 = last_reduction98,
    last_reduction2 = last_reduction2,
    last_reduction3 = last_reduction3,
    last_reduction100 = last_reduction100,
    last_reduction87 = last_reduction87
  )
  constants <- get_constants(effects = effects,
                             training = training,
                             n_scales = n_scales, 
                             .year = target_year, 
                             .percent_change = target_change)

  # Your min total cost must always be...

  # Some constraints can't play nice together. It's like, the constraints have constraints.
  # So, let's write a function to produce the appropriate constraints.

  # Let's source our constraints from the records dataset.
  # This means that in the future if we want to make time-variant constraints, we can.

  # These are the customizable values.
  record$units <- units


  # Join in helper data
  record <- record %>%
    # suppose we adopted EVERY policy in a given year - what would the total cost be?
    left_join(by = "year", y = effects %>% group_by(year) %>% summarize(total_cost = sum(cost))) %>%
    mutate(n_years = 1:n())


  # By default, the following traits should be set.

  # The max annual number of policies is the total number of policies under consideration
  if (is.null(max_annual_policies)) {
    record$max_annual_policies <- length(policies)
  } else {
    record$max_annual_policies <- max_annual_policies
  }
  # The minimum annual number of policies is 0
  if (is.null(min_annual_policies)) {
    record$min_annual_policies <- 0
  } else {
    record$min_annual_policies <- min_annual_policies
  }
  # The minimum number of total policies is 1
  if (is.null(min_total_policies)) {
    record$min_total_policies <- 1
  } else {
    record$min_total_policies <- min_total_policies
  }
  # The max total policies is
  if (is.null(max_total_policies)) {
    record <- record %>% mutate(max_total_policies = max_annual_policies * n_years)
  } else {
    record$max_total_policies <- min_total_policies
  }
  # The max annual policy scale adoption is n_scales
  record$max_annual_policy_scale <- n_scales

  # The max total cost should be n policies * n years * n policy scales * n units * ....
  record <- record %>%
    # suppose we adopted every policy in a given year to DATE at its max scale and units. What would the total cost be?
    mutate(max_total_cost = n_scales * n_years * units * total_cost)
  # If supplied, overwrite it.
  if (!is.null(max_total_cost)) {
    record$max_total_cost <- max_total_cost
  }

  # The max annual cost should be n scales * n units * total cost ( n_policies, 1 year)
  record <- record %>%
    mutate(max_annual_cost = n_scales * units * total_cost)
  # If suppplied, overwrite it.
  if (!is.null(max_annual_cost)) {
    record$max_annual_cost <- max_annual_cost
  }


  # Set minimum annual cost to 0
  record$min_annual_cost <- 0
  # If supplied, overwrite it
  if (!is.null(min_annual_cost)) {
    record$min_annual_cost <- min_annual_cost
  }

  # set minimum total cost to 0
  record$min_total_cost <- 0
  # If supplied, overwrite it.
  if (!is.null(min_total_cost)) {
    record$min_total_cost <- min_total_cost
  }

  # Minimum total cost by each year must always be less than or equal to the cumulative sum of the min annual cost
  check_cost <- all(cumsum(record$min_annual_cost) >= record$min_total_cost)
  if (check_cost == FALSE) {
    stop("cumulative sum of min_annual_cost does not add up at least min_total_cost for at least 1 year.")
  }


  if (98 %in% pollutants) {
    record$min_emissions_chg98 <- 0
    if (!is.null(min_emissions_chg98)) {
      record$min_emissions_chg98 <- min_emissions_chg98
    } else {   }
    record$min_annual_emissions_chg98 <- 0
    if (!is.null(min_annual_emissions_chg98)) {
      record$min_annual_emissions_chg98 <- min_annual_emissions_chg98
    }

    # Minimum total cost by each year must always be less than or equal to the cumulative sum of the min annual cost
    check_reduction98 <- all(cumsum(record$min_annual_emissions_chg98) >= record$min_emissions_chg98)
    if (check_reduction98 == FALSE) {
      stop("cumulative sum of min_emissions_chg98 does not add up at least min_emissions_chg98 for at least 1 year.")
    }
  }
  if (100 %in% pollutants) {
    record$min_emissions_chg100 <- 0
    if (!is.null(min_emissions_chg100)) {
      record$min_emissions_chg100 <- min_emissions_chg100
    }
    record$min_annual_emissions_chg100 <- 0
    if (!is.null(min_annual_emissions_chg100)) {
      record$min_annual_emissions_chg100 <- min_annual_emissions_chg100
    }

    # Minimum total cost by each year must always be less than or equal to the cumulative sum of the min annual cost
    check_reduction100 <- all(cumsum(record$min_annual_emissions_chg100) >= record$min_emissions_chg100)
    if (check_reduction100 == FALSE) {
      stop("cumulative sum of min_emissions_chg100 does not add up at least min_emissions_chg100 for at least 1 year.")
    }
  }
  if (2 %in% pollutants) {
    record$min_emissions_chg2 <- 0
    if (!is.null(min_emissions_chg2)) {
      record$min_emissions_chg2 <- min_emissions_chg2
    }
    record$min_annual_emissions_chg2 <- 0
    if (!is.null(min_annual_emissions_chg2)) {
      record$min_annual_emissions_chg2 <- min_annual_emissions_chg2
    }

    # Minimum total reduction by each year must always be less than or equal to the cumulative sum of the min annual reduction
    check_reduction2 <- all(cumsum(record$min_annual_emissions_chg2) >= record$min_emissions_chg2)
    if (check_reduction2 == FALSE) {
      stop("cumulative sum of min_emissions_chg2 does not add up at least min_emissions_chg2 for at least 1 year.")
    }
  }
  if (3 %in% pollutants) {
    record$min_emissions_chg3 <- 0
    if (!is.null(min_emissions_chg3)) {
      record$min_emissions_chg3 <- min_emissions_chg3
    }
    record$min_annual_emissions_chg3 <- 0
    if (!is.null(min_annual_emissions_chg3)) {
      record$min_annual_emissions_chg3 <- min_annual_emissions_chg3
    }

    # Minimum total reduction by each year must always be less than or equal to the cumulative sum of the min annual reduction
    check_reduction3 <- all(cumsum(record$min_annual_emissions_chg3) >= record$min_emissions_chg3)
    if (check_reduction3 == FALSE) {
      stop("cumulative sum of min_emissions_chg3 does not add up at least min_emissions_chg3 for at least 1 year.")
    }
  }
  if (87 %in% pollutants) {
    record$min_emissions_chg87 <- 0
    if (!is.null(min_emissions_chg87)) {
      record$min_emissions_chg87 <- min_emissions_chg87
    }
    record$min_annual_emissions_chg87 <- 0
    if (!is.null(min_annual_emissions_chg87)) {
      record$min_annual_emissions_chg87 <- min_annual_emissions_chg87
    }

    # Minimum total reduction by each year must always be less than or equal to the cumulative sum of the min annual reduction
    check_reduction87 <- all(cumsum(record$min_annual_emissions_chg87) >= record$min_emissions_chg87)
    if (check_reduction87 == FALSE) {
      stop("cumulative sum of min_emissions_chg87 does not add up at least min_emissions_chg87 for at least 1 year.")
    }
  }



  # After that, the actual scales get calculated

  # For each year in record table...
  for (i in 1:length(record$year)) {
    # i = 1
    # Update the records, by...
    record <- effects %>%
      # Filter the effects to that year...
      filter(year == record$year[i]) %>%
      # And running the optimization sequence on that subset
      get_optimal(
        .record = record, .constants = constants, .effects = .,
        debug = FALSE,
        type = "integer",
        wc = wc, wr = wr,
        # We only want to spend up to 10,000 scale on any one project
        max_annual_policy_scale = record$max_annual_policy_scale[i],
        # The smallest non-zero spending unit is 0.1
        min_spending_unit = 0.1,
        max_abs_diff = max_abs_diff,
        # You must adopt at least 0 policies
        min_total_policies = record$min_total_policies[i],
        max_total_policies = record$max_total_policies[i],
        min_annual_policies = record$min_annual_policies[i],
        max_annual_policies = record$max_annual_policies[i],
        # We have XXX dollars to spend over 10 years
        max_total_cost = record$max_total_cost[i],
        min_total_cost = record$min_total_cost[i],
        # We want to spend at least XXX each year
        max_annual_cost = record$max_annual_cost[i],
        min_annual_cost = record$min_annual_cost[i],
        # Whatever we do, we want to reduce emissions by at least X a year
        # Each of these conditions only get activated in get_optimal() if record actually contains pollutant data for those pollutants.
        # So if it doesn't, the optimization process remains unaffected.
        min_emissions_chg98 = record$min_emissions_chg98[i],
        min_annual_emissions_chg98 = record$min_annual_emissions_chg98[i],
        min_emissions_chg100 = record$min_emissions_chg100[i],
        min_annual_emissions_chg100 = record$min_annual_emissions_chg100[i],
        min_emissions_chg2 = record$min_emissions_chg2[i],
        min_annual_emissions_chg2 = record$min_annual_emissions_chg2[i],
        min_emissions_chg3 = record$min_emissions_chg3[i],
        min_annual_emissions_chg3 = record$min_annual_emissions_chg3[i],
        min_emissions_chg87 = record$min_emissions_chg87[i],
        min_annual_emissions_chg87 = record$min_annual_emissions_chg87[i]
      )
  }

  if (any("error" %in% record$status)) {
    message("Some years in scenario were not optimized successfully.")
  }
  return(record)
}
