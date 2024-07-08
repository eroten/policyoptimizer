#' @name figure_A3.R
#' @title Tradespace analysis
#' @description
#' In order to figure out the ideal composition of our objective function,
#' We need to map out the tradespace of our several metrics of interest.
#' Here we go!

rm(list = ls()); gc()
setwd(paste0(rstudioapi::getActiveProject(), "/v1"))
devtools::load_all(".")
library(dplyr)
library(readr)
library(tidyr)
library(purrr)
library(ompr)
library(GGally)
library(ggplot2)
library(viridis)
 
# # Testing values
# n = 10; n_scales = 1e2; i = 30000; 
# training = get_training(.geoid = "36109", .pollutant = 98, .by = 16, test = TRUE)
# frame = get_frame(data = training)
# record = get_record(frame, start_year = 2025, end_year = 2050, policies = c(1:10), last_cost = 0, last_reduction = 0)
# effects = get_effects(.start_year = 2025, .end_year = 2050, .units = i, var = "epd", path = "data_raw/cmaq.csv", prob = 0.50, min_cost = 0, range = 5, .pollutant = 98, .policy = c(1:10), training, frame)
# constants = get_constants(effects = effects, training = training, n_scales = n_scales, .year = 2005, .percent_change = 0.50)
# 
# 
# # We're going to write an algorithm that uses the convex hull to sift through the most extreme parts of the space.
# # Start with an optimal seed
# bundle = read_csv("paper/scenarios_by_max_abs_diff.csv") %>%
#   filter(max_abs_diff %in% c(.01)) %>%
#   mutate(rep = 1) 
# 
# for(j in 1:5){
# bundle = sample_chull(bundle= bundle, n = 10, fuzz = 1, n_scales = 100, record, effects, constants)
# }
# for(j in 1:5){
#   bundle = sample_chull(bundle= bundle, n = 10, fuzz = 5, n_scales = 100, record, effects, constants)
# }
# 
# ggplot() +
#   geom_point(data = bundle %>% filter(year == max(year)), mapping = aes(x = emissions, y = cost, color = rep))
# 

# Clean up
rm(list = ls()); gc()
pollutant = 98
pollutant_name = get_pollutant(pollutant)

data = read_rds("paper/metrics_units_seeded.rds") %>%
  ungroup() %>%
  filter(year == max(year)) %>%
  mutate(seed = seed %>% dplyr::recode_factor(
    "None" = "None",
    "1" = "[A]",
    "0.1" = "[B]",
    "0.01" = "[C]"
  )) %>%
  select(seed, `Objective` = z,
         !!sym(paste0(pollutant_name, " Emissions Reduction")) := paste0("reduction", pollutant), 
         `Cost` = cost, `Diversification` = diversification, 
         `Effect on VMT` = effect_vmt, 
         !!sym(paste0("Effect on ", pollutant_name, " EF")) := paste0("effect_ef", pollutant))

gg = GGally::ggpairs(
  data = data,
  columns = 2:7,
  ggplot2::aes(color = seed, alpha = 0.5)
) +
  theme_bw(base_size = 14) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_rect(fill = "black", color = "black"),
        strip.text = element_text(color = "white"),
        plot.title=element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)) +
  scale_y_continuous(labels = scales::label_number(scale_cut = scales::cut_si(""))) +
  scale_x_continuous(labels = scales::label_number(scale_cut = scales::cut_si("")))  +
  scale_color_viridis(option = "mako",discrete= TRUE, begin = 0.1, end = 0.7) +
  scale_fill_viridis(option = "mako", discrete= TRUE,begin = 0.1, end = 0.7)

gg = gg +
  ggtitle(label = "Correlation Matrix for Metrics of Interest by Scenario",
  subtitle = "n = 20,000 random scenarios + Optimal Scenarios [A, B, C], max scale = 100, units = 30,000")

ggsave(gg, filename = "paper/figure_A3.png", dpi = 300, width = 14, height = 14)
browseURL("paper/figure_A3.png")

rm(list = ls())
