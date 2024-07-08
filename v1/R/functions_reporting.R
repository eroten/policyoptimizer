#' @title functions_reporting.R
#' @description
#' This script develops the policy optimizer's functions for reporting outcomes.
#' This includes reporting statistics, visualizations, and summary tables.

# VISUALIZE #########################

#' @name get_pollutant
#' @title Get Pollutant
#' 
#' @param pollutant (integer) EPA pollutant code. `98` is CO2 equivalent emissions.
#'
#' @importFrom dplyr case_when
#' 
#' @export
get_pollutant = function(pollutant = 98){
  pollutant = as.character(pollutant)
  case_when(
    pollutant == "98" ~ "CO2e",
    pollutant == "3" ~ "NOx",
    pollutant == "2" ~ "CO",
    pollutant == "100" ~ "PM10",
    pollutant == "87" ~ "VOC",
    TRUE ~ NA_character_)
  
}

#' @name summarize_by_policy
#' @title summarize_by_policy
#' @description
#' Summarize an optimization scenario `record` data.frame by policy effects.
#' @param record (data.frame) optimization scenario record. Outputted by `policy_optimizer()`.
#' @param path (character) (Default = `NULL) (Optional) path to a cmaq `.csv` file. If not provided, use default. (Strongly advise you not change unless confident about it.)
#' 
#' @importFrom dplyr `%>%` group_by across any_of summarize  mutate left_join select contains
#' @importFrom tidyr pivot_longer
#' @importFrom stringr str_detect str_remove str_extract
#' 
#' @export
summarize_by_policy = function(record, path = NULL){
  
  .geoid = record$geoid[1]
  .units = record$units[1]
  .start_year = record$year[1]
  .end_year = record$year[nrow(record)]
  # Get all variable names
  vars = names(record)
  # Record just the policies who names are p[0-9]+
  policy_vars = vars[ str_detect(vars, "p[0-9]+")]
  # Get integer values
  policies = policy_vars %>% str_remove("p") %>% as.integer()
  
  # Get any variables 
  effect_vars = vars[ str_detect(vars, pattern = "effect_emissions")] 
  ef_vars = vars[str_detect(vars, pattern = "effect_ef")]
  # Get all pollutants
  pollutants = vars[ str_detect(vars, pattern = "effect_emissions") ] %>%
    str_extract("[0-9]+") %>% as.integer() %>% unique()
  
  #pollutant = 98
  # Testing values
  # training = get_training(.geoid = .geoid, .pollutant = pollutant, .by = 16, test = FALSE)
  # frame = get_frame(data = training)
  
  # Get effects data
  effects = get_effects(.geoid = .geoid, .pollutants = pollutants, .start_year = .start_year, .end_year = .end_year, .units = .units,
                        var = "epd", path = path, prob = 0.50, min_cost = 0, range = 5,
                        .policy = policies)
  
  # Tally total cost by policy per geoid
  output = record %>%
    # Get the scales per policy year
    select(geoid, year, units, any_of(policy_vars)) %>%
    pivot_longer(cols = c(any_of(policy_vars)),
                 names_to = "policy", values_to = "scale") %>%
    mutate(policy = str_remove(policy, "p") %>% as.integer())  %>%
    # Join in the policy costs
    left_join(
      by = c("year", "policy"), 
      y = effects %>% 
        select(year, policy,
               effect_cost = cost,
               contains("effect_emissions"),
               effect_vmt,
               contains("effect_ef")
        )) %>%
    # Get cost by policy
    group_by(across(any_of(c("geoid", "policy")))) %>%
    summarize(
      across(
        any_of(c("effect_cost", "effect_emissions", effect_vars, "effect_vmt", "effect_ef", ef_vars)),
        ~sum(.x * scale * units)
      ),
      .groups = "drop") %>%
    # Join in the 
    left_join(
      by = c("policy" = "id"), 
      y = policies_data %>% select(id, policy_name = simple)) 
  
  return(output)
}


#' @name summarize_by_year
#' @title summarize_by_year
#' @description
#' Summarize an optimization scenario `record` data.frame by policy outcomes over time.
#' @param record (data.frame) optimization scenario record. Outputted by `policy_optimizer()`.
#' 
#' @importFrom dplyr `%>%` any_of  mutate select sym
#' @importFrom stringr str_detect str_extract
#' 
#' @export
summarize_by_year = function(record){
  
  # Get names of all variables
  vars = names(record)
  pollutants = vars[ str_detect(vars, pattern = "effect_emissions") ] %>%
    str_extract("[0-9]+") %>% as.integer() %>% unique()
  
  
  # For i in pollutants...
  for(i in pollutants){
    vars_effect_emissions = paste0("effect_emissions", i)
    vars_base_emissions = paste0("base_emissions", i)
    vars_effect_ef = paste0("effect_ef", i)
    vars_base_ef = paste0("base_ef", i)
    vars_emissions = paste0("emissions", i)
    vars_ef = paste0("ef", i)
    
    # Calculate new columns for each pollutant
    record = record %>%
      mutate(!!sym(vars_emissions) := !!sym(vars_base_emissions) + cumsum(!!sym(vars_effect_emissions)),
             !!sym(vars_ef) := !!sym(vars_base_ef) + cumsum(!!sym(vars_effect_ef))) 
  }
  
  pollutant_vars = c(paste0("effect_emissions", pollutants),
                     paste0("base_emissions", pollutants),
                     paste0("emissions", pollutants),
                     paste0("effect_ef", pollutants),
                     paste0("base_ef", pollutants),
                     paste0("ef", pollutants)
  )
  
  output = record %>%
    # Clarfiy the base cost and base effect - we'll be using base_ and effect_ style variables a bunch.
    mutate(base_cost = 0, effect_cost = cost) %>%
    mutate(vmt = base_vmt + cumsum(effect_vmt)) %>%
    select(any_of(c("geoid")), year, base_cost, effect_cost, cost, 
           base_vmt, effect_vmt, vmt,
           any_of(c(pollutant_vars)))
  
  return(output)
}


#' @name visualize_by_policy
#' @title visualize_by_policy
#' @description
#' Visualize optimized scenario by policy, using data generated by `summarize_by_policy()`
#' 
#' @param data data.frame generated by summarize_by_policy
#' @param var (character) a single variable, eg. `"effect_cost"` from your data.frame `data`
#' @param palette (character) viridis-style color palette name, eg. `"mako"`
#' @param adjust (numeric) adjustment for axis. Default is `100`.
#'
#' @importFrom dplyr `%>%` tribble filter mutate summarize select any_of case_when
#' @importFrom stringr str_extract str_detect
#' @importFrom viridis viridis
#' @importFrom ggplot2 aes scale_x_continuous scale_y_continuous theme_bw theme element_blank element_text labs  expansion geom_hline geom_col  coord_flip ggplot `%+%`
#' @importFrom shadowtext geom_shadowtext
#' @importFrom scales label_number cut_si
#' 
#' @export
visualize_by_policy = function(data, var = "effect_cost", palette = "mako", adjust = 100){
  # Testing values
  # var = "effect_ef98"; palette = "mako"
  
  # Create a color scale
  colors = viridis(option = palette, n = 5, begin = 0.2, end = 0.8)
  
  # If it's cost or vmt, ignore.
  if(var %in% c("effect_cost", "effect_vmt") ){ 
    # The title will get a blank addition, changing nothing
    addition = "" 
    
  }else{
    # Then it's either an emissions or emissions factor visual.
    # Extract the pollutant
    pollutant = str_extract(var, "[0-9]+") %>% as.integer() %>% unique()
    # Get the pollutant name
    pollutant_name = get_pollutant(pollutant)
    # Create the addition, which will be appended to the front of the title
    addition = paste0(pollutant_name, " ")
  }
  
  # Get a bank of traits we will sort this variable into
  values = tribble(
    ~id, ~color,    ~title,                   ~prefix, ~unit,
    1,   colors[4], "Cost (USD)",              "$",    "",
    2,   colors[3], paste0("Effect on ", addition, "Emissions (t)"), "",     "",
    3,   colors[2], "Effect on VMT (mi)",      "",     "",
    4,   colors[1], paste0("Effect on ", addition, "EF (t/mi)"),     "",      ""
  )
  
  # Depending on the variable for visualization, do the following...
  id = case_when(
    str_detect(var, "effect_cost") ~ 1,
    str_detect(var, "effect_emissions") ~ 2,
    str_detect(var, "effect_vmt") ~ 3,
    str_detect(var, "effect_ef") ~ 4,
    TRUE ~ NA
  )
  
  # Get the attribute matching that variable
  color = values$color[id]
  title = values$title[id]
  prefix = values$prefix[id]
  unit = values$unit[id]
  
  # Subset
  data = data %>% select(policy, policy_name, any_of(c("stat" = var)))
  
  # Get nudge factor
  nudge = (max(data$stat) - min(data$stat))/adjust
  
  
  # Test if variable is a negative (1), usually, or a positive, usually (0)
  flip = round(mean(data$stat < 0))
  if(flip == 1){ expander = c(0.3,0.02); hjust = 1 }else if(flip == 0){ expander = c(0.02, 0.3); hjust = 0}
  
  # Visualize
  gg = ggplot() +
    geom_hline(yintercept = 0, linetype = "dashed", color = "grey", alpha = 0.5, linewidth = 1.5) +
    geom_col(data = data, mapping = aes(x = reorder(policy_name, -policy), y = stat),
             color = "#373737", fill = color) +
    geom_shadowtext(
      data = data, 
      mapping = aes(x = reorder(policy_name, -policy), y = stat, 
                    label = number(stat, prefix = prefix, scale_cut = cut_si(unit = "") )),
      hjust = hjust, bg.r = 0.2, bg.color = "white", color = "#373737", nudge_y = nudge) +
    coord_flip() +
    labs(x = NULL, y = title) +
    theme_bw(base_size = 14) +
    theme(panel.grid.minor = element_blank(),
          panel.grid.major = element_blank(),
          plot.title =element_text(hjust = 0.5),
          axis.ticks.y = element_blank()) +
    scale_y_continuous(
      labels = label_number(prefix = prefix, scale_cut = cut_si(unit = unit)),
      expand = expansion(expander))
  
  return(gg)
}

#' @name visualize_by_year
#' @title visualize_by_year
#' @description
#' Visualize optimized scenario by year, using data generated by `summarize_by_year()`
#' 
#' @param data data.frame generated by summarize_by_year
#' @param var (character) a single variable, eg. `"cost"` from your data.frame `data`
#' @param palette (character) viridis-style color palette name, eg. `"mako"`
#' @param adjust (numeric) adjustment for axis. Default is `100`.
#'
#' @importFrom dplyr `%>%` tribble filter mutate summarize select any_of case_when
#' @importFrom stringr str_extract str_detect
#' @importFrom viridis viridis
#' @importFrom ggplot2 aes scale_x_continuous scale_y_continuous theme_bw theme element_blank element_text labs  expansion geom_ribbon geom_line geom_point ggplot `%+%`
#' @importFrom shadowtext geom_shadowtext
#' @importFrom scales label_number cut_si
#' 
#' @export
visualize_by_year = function(data, var = "cost", palette = "mako", adjust = 100){
  #data
  # var = "cost"; palette = "mako"; adjust = 100
  
  # Create a color scale
  colors = viridis(option = palette, n = 5, begin = 0.2, end = 0.8)
  
  # If it's cost or vmt, ignore.
  if(var %in% c("effect_cost", "effect_vmt") ){ 
    # The title will get a blank addition, changing nothing
    addition = "" 
    
  }else{
    # Then it's either an emissions or emissions factor visual.
    # Extract the pollutant
    pollutant = str_extract(var, "[0-9]+") %>% as.integer() %>% unique()
    # Get the pollutant name
    pollutant_name = get_pollutant(pollutant)
    # Create the addition, which will be appended to the front of the title
    addition = paste0(pollutant_name, " ")
  }
  
  # Get a bank of traits we will sort this variable into
  values = tribble(
    ~id, ~type, ~color,    ~title,                   ~prefix, ~unit,
    1,  "cost",  colors[4], "Cumulative Cost (USD)",  "$",     "",
    2,  "emissions", colors[3], paste0(addition, "Emissions (t)"),          "",      "",
    3,   "vmt", colors[2], "VMT (mi)",               "",      "",
    4,   "ef", colors[1], paste0(addition, "EF (t/mi)"),              "",      "" 
  )
  
  # Depending on the variable for visualization, do the following...
  id = case_when(
    str_detect(var, "cost") ~ 1,
    str_detect(var, "emissions") ~ 2,
    str_detect(var, "vmt") ~ 3,
    str_detect(var, "ef") ~ 4,
    TRUE ~ NA
  )
  
  # Get the attribute matching that variable
  color = values$color[id]
  title = values$title[id]
  prefix = values$prefix[id]
  unit = values$unit[id]
  #var = values$type[id]
  
  myvars = c(paste0("base_", var), paste0("effect_", var), var)
  newvars = c("base", "effect", "stat")
  # Subset 
  data = data %>% select(geoid, year, any_of(setNames(myvars, newvars)))
  
  text = data %>%
    filter(year == min(year) | year == max(year)) %>%
    mutate(label_stat = number(stat, prefix = prefix, scale_cut = cut_si(unit)),
           label_base = number(base, prefix = prefix, scale_cut = cut_si(unit)))
  
  #nudge = max(data$stat) / 100
  nudge = (max(data$stat) - min(data$base))/adjust
  
  
  gg = ggplot() +
    geom_ribbon(data = data, mapping = aes(x = year, ymin = base, ymax = stat),
                fill = color, alpha = 0.8, color = color, linewidth = 2) +
    geom_line(data = data, mapping = aes(x = year, y = stat),
              linewidth = 1.5, linetype = "dashed", color = "black") +
    geom_point(data = text, mapping = aes(x = year, y = base),
               shape = 21, fill = color, color = color, size = 5) +
    geom_point(data = text, mapping = aes(x = year, y = stat),
               shape = 21, fill = "black", color = "black", size = 5, stroke = 1.5) +
    # Labels
    geom_shadowtext(
      data = text,
      mapping = aes(x = mean(data$year), y = min(data$base), label = "Baseline"),
      bg.r = 0.2, bg.color = "white", color = color, check_overlap = TRUE,
      vjust = 0, hjust = 0.5, size = 5, nudge_y = nudge, fontface = "bold", alpha = 0.5
    ) +
    geom_shadowtext(
      data = text,
      mapping = aes(x = mean(data$year), y = max(data$stat), label = "Scenario"),
      bg.r = 0.2, bg.color = "white", color = "black", check_overlap = TRUE,
      vjust = 0, hjust = 0.5, size = 5, nudge_y = nudge, fontface = "bold", alpha = 0.5
    ) +
    # Numbers
    geom_shadowtext(
      data = text,
      mapping = aes(x = year, y = base, label = label_base),
      bg.r = 0.2, bg.color = "white", color = color, check_overlap = TRUE,
      vjust = 0, hjust = c(0,1), size = 4, nudge_y = nudge
    ) +
    geom_shadowtext(
      data = text,
      mapping = aes(x = year, y = stat, label = label_stat),
      bg.r = 0.2, bg.color = "white", color =  "black", check_overlap = TRUE,
      vjust = 0, hjust = c(0,1), size = 4, nudge_y = nudge*2
    ) +
    scale_y_continuous(labels = label_number(prefix = prefix, scale_cut = cut_si(unit)),
                       expand = expansion(c(0.0,0.05))) +
    scale_x_continuous(expand = expansion(c(0.03,0.03))) +
    theme_bw(base_size = 14) +
    theme(panel.grid = element_blank(),
          plot.title =element_text(hjust = 0.5)) +
    labs(x = "Year", y = title)
  
  return(gg)
}



#' @name report_stats
#' @title report_stats
#' @description
#' Calculates summary statistics for a `record` data.frame compiling the results for n counties.
#' Outputs a table of statistics and standard errors where applicable.
#' @importFrom dplyr `%>%` select filter summarize n mutate reframe across bind_rows
#' @importFrom scales number
#' @export
report_stats = function(record, pollutant = 98){
  
  result1 = record %>%
    select(geoid, year, base_emissions = paste0("base_emissions", pollutant)) %>%
    filter(year == min(year)) %>%
    summarize(
      year = min(year),
      base_emissions = sum(base_emissions),
      n_counties = n()
    ) %>%
    mutate(across(base_emissions, ~scales::number(.x, accuracy = 0.1, scale_cut = scales::cut_si("")))) %>%
    # Format
    summarize(
      name = paste0("Base Emissions in ", year),
      statistic = base_emissions,
      se = NA,
      description = paste0(
        "For n = ", n_counties, " counties. ", 
        "From CATSERVER. Emissions fluctuate over time due to atmospheric factors."
      )
    )
  
  
  result2 = record %>%
    select(geoid, year, cost, reduction = paste0("reduction", pollutant), emissions = paste0("emissions", pollutant)) %>%
    filter(year == max(year)) %>%
    summarize(
      year = max(year),
      cost = sum(cost),
      reduction = sum(reduction),
      emissions = sum(emissions),
      n_counties = n()
    ) %>%
    mutate(across(c(cost, reduction, emissions), ~scales::number(.x, accuracy = 0.1, scale_cut = scales::cut_si("")))) %>%
    # Format
    reframe(
      name = c(paste0("Cost by ", year), paste0("Reduction by ", year), paste0("Emissions by ", year) ),
      statistic = c(cost, reduction, emissions),
      se = NA,
      description = paste0("For n = ", n_counties, " counties")
    )
  
  result3 = record %>%
    select(geoid, year, cost) %>%
    filter(year == max(year)) %>%
    summarize(year = max(year),
              mean = mean(cost),
              sd = sd(cost),
              se = sd / sqrt(n()),
              lower = quantile(cost, probs = 0.025),
              upper = quantile(cost, probs = 0.975),
              n_counties = n()
    ) %>%
    mutate(across(c(mean, sd, se, lower, upper), ~scales::number(.x, accuracy = 0.1, scale_cut = scales::cut_si("")))) %>%
    reframe(
      name = c(
        paste0("Average Total County Cost by ", year), 
        paste0("2.5th Percentile of Total Cost by ", year),
        paste0("97.5th Percentile of Total Cost by ", year)             
      ),
      statistic = c(mean, lower, upper),
      se = c(se, NA, NA),
      description = paste0("For n = ", n_counties, " counties")
    )  
  
  
  result4 = record %>%
    select(geoid, year, cost, prior_cost) %>%
    mutate(annual_cost = cost - prior_cost) %>%
    group_by(year) %>%
    summarize(mean = mean(annual_cost),
              sd = sd(annual_cost),
              n_w = n(), .groups = "drop") %>%
    # Get the pooled estimates, accounting for within-group variation
    summarize(estimate = mean(mean), 
              se = sqrt(sum( sd^2 / n_w) ),
              n_counties = max(n_w),
              n_years = n() 
    ) %>%
    mutate(across(c(estimate, se), ~scales::number(.x, accuracy = 0.1, scale_cut = scales::cut_si("")))) %>%
    summarize(
      name = "Average Annual Cost per County",
      statistic = estimate,
      se = se,
      description = paste0("For n = ", n_counties, " county(ies) over n = ", n_years, " year(s).")
    )
  
  
  result5 = record %>%
    select(geoid, year, reduction = paste0("reduction", pollutant), prior_reduction = paste0("prior_reduction", pollutant)) %>%
    mutate(annual_estimate = reduction - prior_reduction) %>%
    group_by(year) %>%
    summarize(mean = mean(annual_estimate),
              sd = sd(annual_estimate),
              n_w = n(), .groups = "drop") %>%
    # Get the pooled estimates, accounting for within-group variation
    summarize(estimate = mean(mean), 
              se = sqrt(sum( sd^2 / n_w) ),
              n_counties = max(n_w),
              n_years = n() ) %>% 
    mutate(across(c(estimate, se), ~scales::number(.x, accuracy = 0.1, scale_cut = scales::cut_si("")))) %>%
    summarize(
      name = "Average Annual Emissions Reduction per County",
      statistic = estimate,
      se = se,
      description = paste0("For n = ", n_counties, " county(ies) over n = ", n_years, " year(s).")
    )
  
  
  bind_rows(
    result1,
    result2,
    result3,
    result4,
    result5
  ) %>%
    return()
}

