#' @name figure_3.R
#' @description
#' Scenario Metrics Visualization


library(dplyr)
library(readr)
library(tidyr)
library(RMySQL)
library(DBI)
library(ggplot2)
library(ggpubr)
library(viridis)
library(stringr)

# Cleanup
rm(list = ls()); gc()
# Set directory
setwd(paste0(rstudioapi::getActiveProject(), "/v1"))
# Load enviromental variables
readRenviron(".Renviron")
# Load package
devtools::load_all(".")

# Load record
record = read_csv("paper/scenarios_by_diversification.csv") %>%
  filter(max_abs_diff %in% c(.01))

# Get data from record
data = record %>%
  summarize_by_policy()

# Make visualizations, with extra ggplot edits
g1 = data %>%
  visualize_by_policy(var = "effect_cost")

g2 = data %>%
  visualize_by_policy(var = "effect_emissions98")  +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank())

g3 = data %>%
  visualize_by_policy(var = "effect_vmt") +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank())


g4 = data %>%
  visualize_by_policy(var = "effect_ef98") +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank())

# ggsave(gg_lower, filename = "paper/figure_3_lower.png", dpi = 300, width = 12, height = 6)
# browseURL("paper/figure_3_lower.png")


# Get data from record
data = record %>%
  summarize_by_year()

g5 = data %>%  visualize_by_year(var = "cost") + 
  labs(y = NULL, title = "Cumulative Cost (USD)") +
  scale_x_continuous(breaks = seq(from = 2025, to = 2050, by = 10), expand = expansion(c(0.03,0.03)))

g6 = data %>%  visualize_by_year(var = "emissions98") +
  labs(y = NULL, title = "CO2e Emissions (t)") +
  scale_x_continuous(breaks = seq(from = 2025, to = 2050, by = 10), expand = expansion(c(0.03,0.03)))

g7 = data %>%  visualize_by_year(var = "vmt") +
  labs(y = NULL, title = "VMT (mi)" ) +
  scale_x_continuous(breaks = seq(from = 2025, to = 2050, by = 10), expand = expansion(c(0.03,0.03)))

g8 = data %>%  visualize_by_year(var = "ef98") +
  labs(y = NULL, title = "CO2e Emissions Factor (t/mi)") +
  scale_x_continuous(breaks = seq(from = 2025, to = 2050, by = 10), expand = expansion(c(0.03,0.03)))


# gg_upper = ggarrange(plotlist = list(g5, g6, g7, g8), nrow = 1)
# ggsave(gg_upper, filename = "paper/figure_3_upper.png", dpi = 300, width = 12, height = 6)
# browseURL("paper/figure_3_upper.png")



gg_lower = ggarrange(plotlist = list(g1,g2,g3,g4), 
                     widths = c(1.5, 1, 1, 1), 
                     labels = c("E", "F", "G", "H"), 
                     label.x = c(0.20, 0.05, 0.05, 0.05),
                     label.y = c(0.99, 0.99, 0.99, 0.99),
                     nrow = 1)

space = ggplot() + theme_void(base_size = 14) + theme(panel.background = element_rect(fill = "white", color = NA))
gg_upper = ggarrange(
  plotlist = list(space, g5, g6, g7, g8),
  widths = c(0.25, 1, 1, 1, 1),
  labels = c(NA, "A", "B", "C", "D"),
  label.x = c(NA, 0.05, 0.05, 0.05, 0.05),
  label.y = c(NA, 0.99, 0.99, 0.99, 0.99),
  nrow = 1
  )

gg = ggarrange(plotlist = list(gg_upper, gg_lower), ncol = 1, nrow = 2 )

ggsave(gg,  filename = "paper/figure_3.png", width = 13, height = 12, dpi = 300)
browseURL("paper/figure_3.png")


rm(list = ls())


