#' @title functions_tradespace.R
#' @description
#' This script develops the policy optimizer's functions for sampling and visualizing the tradespace.
#' These do not directly affect the optimizer,
#' but are auxillary functions for comparison with the optimizer's findings.

# TRADESPACE ######################################

#' @name sample_outward
#' @title sample_outward
sample_outward = function(seed, n_policies = 10, n_years = 26, n_scales = 100, fuzz = NULL){
  # Take a seed of values and work outwards from it...
  
  # n = 1000; n_policies = 10; n_years = 26; n_scales = 100; fuzz = 5
  
  # Randomly sample a number of years
  num_of_years = sample(1:n_years, 1)
  # Randomly sample a certain random number of randomly selected years.
  sampled_years = sample(1:n_years, num_of_years)
  
  # Randomly sample a number of policies
  num_of_policies = sample(1:n_policies, 1)
  # Randomly sample a certain random number of randomly selected policies
  sampled_policies = sample(1:n_policies, num_of_policies)
  
  
  # If you provide a value for fuzziness, it will ADD 
  if(!is.null(fuzz)){
    # Extract cells for sampled indices
    vals = seed[ sampled_years, sampled_policies]
    # Adjust cell values 
    vals = vals + matrix(round(runif(n = num_of_policies * num_of_years, min = -1*fuzz, max = fuzz)), ncol = num_of_policies, nrow = num_of_years)
    # Set all cell values that are less than zero to 0.
    vals[vals < 0] <- 0
    # Reset values
    seed[ sampled_years, sampled_policies] <- vals
    
    # But if fuzz is not provided, jsut direct resample, 
  }else if(is.null(fuzz)){
    sim = round(runif(n = num_of_policies * num_of_years, min = 0, max = n_scales))
    
    # Resample those values  
    seed[ sampled_years, sampled_policies] <- matrix(sim, ncol = num_of_policies, nrow = num_of_years)
  }
  output = unlist(seed)
  return(output)
}

#' @name pareto_rank_me
#' @title pareto_rank_me
#' @importFrom eaf pareto_rank
#' @importFrom dplyr tibble
pareto_rank_me = function(a, b, maximize = FALSE){
  data = tibble(a,b)
  output = pareto_rank(data = data, maximise = maximize)
  return(output)
}


#' @name sample_scenario
#' @title Get a Sample Scenario via random sampling
#' @author Tim Fraser
#' @description
#' This function generates a scenario of policy adoption scales at random 
#' and calculates the resulting `record` object.
#' Useful for enumerating an approximation of the full tradespace of your county's metrics. 
#' @param n_scales (integer) max policy adoption scale
#' @param record (data.frame) a `record` data.frame object produced by `get_record()`. Gets filled in by this function.
#' @param effects (data.frame) an `effects` data.frame of policy effects over time corresponding to the years in your scenario. Produced by `get_effects()`.
#' @param constants (data.frame) a `constants` data.frame of constants produced by `get_constants()`. Used for rescaling metrics.
#' @param .id (integer) Default is NULL. If not null, the value to be printed at the conclusion of the sampling process. Used for progress reporting.
#' 
#' @importFrom dplyr `%>%` select group_by mutate left_join reframe ungroup right_join 
#' @importFrom tidyr expand_grid pivot_wider
#' 
#' @export
scenario_sampler = function(n = 1000, n_scales = 100,  record, effects, constants, wc = 0.5, wr = 0.5, seed = NULL, fuzz = NULL){
  
  policies = unique(effects$policy)
  years = unique(effects$year)
  n_years = length(years)
  n_policies = length(policies)
  
  # Ideal balance for each year in a diversified portfolio
  ideal = n_scales / n_policies
  width = n_scales * n_policies
  
  effect_data = effects %>% select(policy,year,cost, effect_emissions98, effect_vmt, effect_ef98, units)
  
  # If no seed provided sample normally...
  if(is.null(seed)){
    
    sims = expand_grid(
      rep = 1:n,
      year = years,
      policy = policies
    ) %>%
      group_by(rep) %>%
      mutate(
        scale = round(runif(n = n_policies * n_years, min = 0, max = n_scales))
      )
    # but if a seed IS provided...
  }else if(!is.null(seed)){
    
    sims = expand_grid(
      rep = 1:n,
      year = years,
      policy = policies
    ) %>%
      group_by(rep) %>%
      mutate(
        scale = sample_outward(seed = seed, n_policies = n_policies, n_years = n_years, n_scales = n_scales, fuzz = fuzz)
      ) 
  }
  
  sims = sims %>% 
    left_join(by = c("year", "policy"), y = effect_data, multiple = "all")
  
  annual_values = sims %>%
    group_by(rep, year) %>%
    reframe(cost = sum(cost * units * scale),
            effect_emissions98 = sum(effect_emissions98 * units * scale),
            reduction98 = -1*effect_emissions98,
            diversification = sum(abs(scale - ideal) ) / width,
            effect_vmt = sum(effect_vmt * units * scale),
            effect_ef98 = sum(effect_ef98 * units * scale)
    ) %>%
    ungroup() %>%
    group_by(rep) %>%
    # Get cumulative sum of each metric
    mutate(cost = cumsum(cost),
           reduction98 = cumsum(reduction98),
           effect_emissions98 = cumsum(effect_emissions98),
           effect_vmt = cumsum(effect_vmt),
           effect_ef98 = cumsum(effect_ef98)
    ) 
  
  record_data = record %>% select(year, units, base_emissions98, base_vmt, base_ef98)
  
  output = annual_values %>%  
    right_join(by = "year", x = record_data, multiple = "all") %>%
    # Calculate end quantities
    mutate(vmt = base_vmt + effect_vmt,
           ef98 = base_ef98 + effect_ef98,
           emissions98 = base_emissions98 + reduction98) %>%
    # Join in simulated valules
    left_join(
      by = c("rep", "year"), 
      x = sims %>%
        select(rep, policy, year, scale) %>%
        pivot_wider(id_cols = c(rep, year), names_from = policy, names_prefix = "p", values_from = scale))
  
  # Get scaled values
  output = output %>%
    # Rescale values
    mutate(icost = scale_cost(cost, lc = constants$lc, uc = constants$uc),
           ireduction98 = scale_reduction(reduction98, lr = constants$lr98, ur = constants$ur98),
           z = icost * wc +  ireduction98 * wr)
  
  return(output)
  
}

#' @name get_chull
#' @title get_chull
#' @description Wrapper function for `stats::chull`
get_chull = function(x,y){
  data = data.frame(x,y)
  indices = chull(data)
  data$flag = FALSE
  data[ indices, ]$flag = TRUE
  return(data$flag)
}


#' @name sample_chull
#' @title sample_chull
#' @importFrom dplyr `%>%` filter mutate ungroup bind_rows select
#' @importFrom purrr map_dfr
sample_chull = function(bundle, n = 10, fuzz = 5, n_scales = 100, record, effects, constants){
  # Start a bundle using your seed.
  #bundle = bundle %>% mutate(rep = as.integer(1))
  
  # Get some points to start around that seed
  # around_seed = bundle %>% 
  #   split(.$rep) %>%
  #   map_dfr(~scenario_sampler(n = n, n_scales = n_scales, record = record, effects = effects, constants = constants, wc = 0.5, wr = 0.5, seed = .x %>% select(p1:p10)) ) %>%
  #   mutate(rep = expand_grid(rep = 1:(n()/length(record$year)), year = 1:length(record$year)) %>% with(rep)) %>%
  #   # # Make the reps start from 1 + the max prior value
  #   mutate(rep = as.integer(rep) + max(bundle$rep))
  
  # Bundle them into the bundle
  #bundle = bind_rows(bundle, around_seed)
  
  # Get the convex hull around these points for the max year
  indices = bundle %>%
    filter(year == max(year)) %>%
    mutate(chull = get_chull(icost, ireduction)) %>%
    filter(chull == TRUE) %>%
    ungroup() %>%
    with(rep) %>% unique()
  
  # Take these outer scenarios and tweak them
  outer_bundle = bundle %>% 
    filter(rep %in% indices)
  
  
  outer_outer_bundle = outer_bundle %>%
    split(.$rep) %>%
    map_dfr(~scenario_sampler(n = n, n_scales = n_scales, record = record, effects = effects, constants = constants, wc = 0.5, wr = 0.5, 
                              seed = .x %>% select(p1:p10), fuzz = fuzz) ) %>%
    ungroup() %>%
    mutate(rep = expand_grid(rep = 1:(n()/length(record$year)), year = 1:length(record$year)) %>% with(rep)) %>%
    # # Make the reps start from 1 + the max prior value
    mutate(rep = as.integer(rep) + max(bundle$rep)) 
  
  
  # # Bundle them into the bundle
  bundle =  bind_rows(bundle, outer_outer_bundle)
  #bundle %>% group_by(rep) %>% count() %>% tail()
  return(bundle)
  
  
}
