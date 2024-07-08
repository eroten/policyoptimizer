#' @name figure_2.R
#' @title Tradespace Visual
# Reusing data from 01_run_scenarios.R script

library(viridis)
library(ggplot2)
library(dplyr)
library(readr)
library(shadowtext)

rm(list = ls())
setwd(paste0(rstudioapi::getActiveProject(), "/v1"))
devtools::load_all(".")


area = "Tompkins County, NY"
start_year = 2025
end_year = 2050
pollutant = 98
highlight_color = "gold"

# Get metadata
.pollutant = get_pollutant(pollutant)

data = read_rds("paper/metrics_units_seeded.rds") %>%
  ungroup() %>%
  filter(year == max(year)) %>%
  select(seed, rep, year, z, cost, reduction98, icost, ireduction98, diversification, emissions98) %>%
  # Compute pareto rank
  mutate(rank = pareto_rank_me(a = icost, b = ireduction98, maximize = FALSE)) 

# Make labels
text = data %>%
  reframe(x = c(min(emissions98), max(emissions98)),
          y = c(min(cost), max(cost)), 
          label = c("Best", "Worst") )

# Get optimal scenarios
h = read_csv("paper/scenarios_by_diversification.csv") %>%
  filter(max_abs_diff %in% c(1, 0.1, .01)) %>%
  filter(year == max(year)) %>%
  mutate(seed = max_abs_diff %>% recode_factor(
    "0.01" = "C",
    "0.1" = "B",
    "1" = "A"
  ))


lines = h %>%
  # Bind in the 'x' points for the Best point
  bind_cols(text %>% filter(label == "Best")) %>%
  # Calculate distance on x-axis between best and optimal points
  mutate(xmin = emissions98, xmax = x) %>%
  mutate(improvement = xmin - xmax) %>%
  mutate(label = scales::number(improvement, scale_cut = scales::cut_si("t"))) %>%
  mutate(xmid = xmin + (xmax - xmin) / 2) %>%
  # Grab the cost (for y-axis) and the xmin and xmax
  select(id, cost, xmid, xmin, xmax, improvement, label)



g1 = ggplot() +
  geom_vline(data = lines, mapping = aes(xintercept = xmax[1]), color = "lightgrey", linewidth = 1.5, linetype = "dashed") +
  geom_errorbar(data = lines, mapping = aes(xmin = xmin, xmax = xmax, y = cost, group = id), 
                linewidth = 1.5, color = "gold", width = 0.2, alpha = 0.75) +
  shadowtext::geom_shadowtext(
    data = lines, mapping = aes(x = xmax[1], y = cost[1], label = "Min Emissions\nfrom Samples"), 
    nudge_x = 0, nudge_y = 150e6, vjust = 0,
    hjust = 1, bg.r = 0.2, bg.color = "darkgrey", color = "white") +
  shadowtext::geom_shadowtext(
    data = lines, mapping = aes(x = xmax[1], y = cost[1], label =   scales::number(xmax[1], scale_cut = scales::cut_si("t"))),
    nudge_x = -0, nudge_y = 100e6, vjust = 0, 
    hjust = 1, bg.r = 0.2, bg.color = "darkgrey", color = "white") +
  

  shadowtext::geom_shadowtext(
    data = lines, mapping = aes(x = xmid, y = cost, label = label),
    bg.r = 0.1, bg.color = "gold", color = "#373737",
    hjust = 0.5, nudge_y = 10e6, vjust = 0) +
  
  shadowtext::geom_shadowtext(
    data = lines, mapping = aes(x = xmid[1], y = cost[1], label = "Improved Emissions"),
    bg.r = 0.2, bg.color = "gold", color = "white",
    hjust = 0.5, nudge_y = 50e6, vjust = 0) +
  
  geom_point(
    data = data, 
    mapping = aes(x = emissions98, y = cost, fill = z),
    shape = 21, color = "white", size = 5) +
  geom_line(
    data = data %>% filter(rank == 1) ,
    mapping = aes(x = emissions98, y= cost),
    linewidth = 2
  ) +
  geom_point(
    data = data %>% filter(rank == 1),
    mapping = aes(x = emissions98, y= cost, fill =  z),
    size = 5, color = "black", shape = 21, stroke = 1.5
  ) +
  shadowtext::geom_shadowtext(
    data = text,
    mapping = aes(x = x, y = y, label = label),
    color = "white", bg.color = "#373737", bg.r = 0.1, fontface = "bold",
    hjust = c(0, 1)
  ) +
  geom_point(
    data = h, mapping = aes(x = emissions98, y = cost, group = id), 
    shape = 21, fill = "gold", color = "white", size = 8, stroke = 2, alpha = 0.8
  ) +
  shadowtext::geom_shadowtext(
    data = h, mapping = aes(x = emissions98, y = cost, group = id, label = seed),
    color = "black", size = 5, bg.r = 0.2, bg.color = "white"
  ) +
  scale_fill_viridis(option = "mako", direction = -1) +
  theme_bw(base_size = 14) +
  theme(legend.position = "bottom",
        panel.grid.minor = element_blank()) +
  scale_y_continuous(labels = scales::label_number(big.mark = ",", scale = 1e-6, prefix = "$", suffix = "M")) +
  scale_x_continuous(labels = scales::label_number(big.mark = ",", suffix = " t")) +
  guides(fill = guide_colorbar(barwidth = 20, frame.colour = "black")) +
  labs(x = paste0(.pollutant, " Emissions (tons) by Scenario End Year (", end_year, ")"),
       y = paste0("Total Cost (", start_year, "-", end_year,")"),
       fill = "Objective (0-1)",
       title = paste0("Policy Scenarios in ", area),
       subtitle = "N = 20,000 Sampled Scenarios vs. Optimal Scenarios [A, B, C]") +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))
g1
ggsave(g1, filename = "paper/figure_2.png", dpi = 500, width = 8, height = 8)
browseURL("paper/figure_2.png")

