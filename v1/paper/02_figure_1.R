#' @name figure_1.R
#' @description
#' Compilation of figures for 1 optimal scenario for 1 county
#' 
# We developed 3 scenarios.
# First, we show a scenario that enforces no policy portfolio diversification.
# Second, we show a scenario that enforces some policy portfolio diversification.
# Third, we show a scenario that enforces more policy portfolio diversification.
library(dplyr)
library(readr)
library(ggplot2)
library(viridis)
library(ggpubr)
library(ggtext)

library(dplyr)
library(readr)
library(tidyr)
library(purrr)
library(DBI)
library(RMySQL)
library(ROI, warn.conflicts = FALSE, quietly = TRUE)
library(ROI.plugin.glpk, warn.conflicts = FALSE, quietly = TRUE)
library(ompr, warn.conflicts = FALSE, quietly = TRUE)
library(ompr.roi, warn.conflicts = FALSE, quietly = TRUE)


rm(list = ls())
devtools::load_all(".")


data = read_csv("paper/scenarios_by_diversification.csv") %>%
  filter(max_abs_diff %in% c(1, 0.1, 0.01))  %>%
  mutate(diversification = max_abs_diff %>% recode_factor(
    #"0.25" = "≤0.25",
    "1" = "≤1\n[A]",
    "0.10" = "≤0.1\n[B]",
    "0.01" = "≤0.01\n[C]"))


colors = viridis::mako(n = 3, begin = 0.1, end = 0.7)

g1 = ggplot() +
  geom_line(
    data = data, 
    mapping = aes(x = year, y = reduction98, group = id, color = factor(diversification)),
    linewidth = 1.5, alpha = 0.9) +
  # Plot min for very concentrated and very diversified
  shadowtext::geom_shadowtext(
    data = data %>%
      filter(max_abs_diff %in% c(1, 0.01)) %>%
      filter(year == min(year)),
    mapping = aes(
      x = year, y = reduction98, group = id, color = factor(diversification),
      label = paste0( scales::number(reduction98, scale_cut = scales::cut_si("t"))  )),
    bg.r = 0.2, bg.color = "white", hjust = 0, check_overlap = TRUE
  ) +
  # Plot Max for very concentrated and very diversified
  shadowtext::geom_shadowtext(
    data = data %>%
      filter(max_abs_diff %in% c(1, 0.01)) %>%
      filter(year == max(year)),
    mapping = aes(
      x = year, y = reduction98, group = id, color = factor(diversification),
      label = paste0( scales::number(reduction98, scale_cut = scales::cut_si("t"))  )),
    bg.r = 0.2, bg.color = "white", hjust = 1, check_overlap = TRUE
  ) +  
  scale_y_continuous(trans = "log", labels = scales::label_number(scale_cut = scales::cut_si("t"))) +
  scale_color_manual(values = colors) +
  theme_bw(base_size =14) +
  theme(legend.position = "bottom", panel.grid.minor = element_blank(), panel.grid.major = element_blank()) +
  labs(y = "CO2e Emissions Reduction (tons)", x = NULL) +
  guides(color = "none")

g2 = ggplot() +
  # Plot lines
  geom_line(
    data = data, 
    mapping = aes(x = year, y = cost, group = id, color = factor(diversification)),
    linewidth = 1.5, alpha = 0.9) +
  # Plot min for very concentrated and very diversified
  shadowtext::geom_shadowtext(
    data = data %>%
      filter(max_abs_diff %in% c(1, 0.01)) %>%
      filter(year == min(year)),
    mapping = aes(
      x = year, y = cost, group = id, color = factor(diversification),
      label = paste0( scales::number(cost, prefix = "$", scale_cut = scales::cut_si(""))  )),
    bg.r = 0.2, bg.color = "white", hjust = 0, check_overlap = TRUE
  ) +
  # Plot Max for very concentrated and very diversified
  shadowtext::geom_shadowtext(
    data = data %>%
      filter(max_abs_diff %in% c(1, 0.01)) %>%
      filter(year == max(year)),
    mapping = aes(
      x = year, y = cost, group = id, color = factor(diversification),
      label = paste0( scales::number(cost, prefix = "$", scale_cut = scales::cut_si(""))  )),
    bg.r = 0.2, bg.color = "white", hjust = 1, check_overlap = TRUE
  ) +
  scale_y_continuous(trans = "log", labels = scales::label_number(prefix = "$", scale_cut = scales::cut_si(""))) +
  scale_color_manual(values = colors) +
  theme_bw(base_size =14) +
  theme(legend.position = "bottom", 
        panel.grid.minor = element_blank(), 
        panel.grid.major = element_blank(),
        plot.title = element_text(hjust = 0.5)) +
  labs(y = "Cost (USD)", x = "Year") +
  guides(color = "none")

dlong = data %>%
  select(id, diversification, max_abs_diff, year, p1:p10) %>%
  pivot_longer(cols = c(p1:p10), names_to = "policy", values_to = "scale") %>%
  mutate(policy = stringr::str_remove(policy, "p") %>% as.integer()) %>%
  group_by(id, diversification, max_abs_diff, policy) %>%
  summarize(total = sum(scale, na.rm = TRUE), .groups = "drop")


g3 = ggplot() +
  geom_col(data = dlong, mapping = aes(x = reorder(factor(policy), -policy),
                                       y = total, group = id, fill = factor(diversification)),
           alpha = 0.75, position = position_dodge()) +
  scale_y_continuous(expand = expansion(c(0.01,0.05)),
                     breaks = seq(from = 0, to = 2500, by =  500)) +
  scale_fill_manual(values = colors) +
  facet_wrap(~diversification, ncol = 1, labeller = as_labeller(
    c("≤1\n[A]" = "[A] Very Concentrated (≤1)",
      "≤0.1\n[B]" = "[B] Mixed (≤0.1)",
      "≤0.01\n[C]" = "[C] Very Diversified (≤0.01)")
  )) +
  coord_flip() +
  labs(x = "Policy ID", y = "Cumulative Scale of Adoption", 
       fill = "Diversification Threshold<br><sup>(0 = Diversified, 1 = Concentrated)</sup>") +
  theme_bw(base_size =14) +
  theme(strip.background = element_rect(fill = "black", color = "black"),
        strip.text = element_text(color = "white"),
        axis.ticks.y = element_blank(),
        panel.grid.minor = element_blank(), 
        panel.grid.major = element_blank(),
        legend.position = "bottom",
        legend.title = ggtext::element_markdown(),
        plot.title = element_text(hjust = 0.5))


dmax = data %>%
  filter(year == max(year)) %>%
  select(year, max_abs_diff, diversification, icost, ireduction98, diversification, z) %>%
  pivot_longer(cols = c(icost:z), names_to = "var", values_to = "value") %>%
  mutate(label = paste0( scales::number(value, accuracy = 0.01) )) %>%
  mutate(var = var %>% recode_factor(
    "diversification" = "Diversification",
    "icost" = "Cost\nIndex",
    "ireduction98" = "Emissions\nReduction\nIndex",
    "z" = "Objective"
  ))

g4 = ggplot() +
  geom_col(data = dmax, mapping = aes(x = var, y = value, fill = diversification, group = max_abs_diff),
           position = "dodge", alpha = 0.75) +
  shadowtext::geom_shadowtext(
    data = dmax,
    mapping = aes(x = var, y = value + 0.01, color = diversification, group = max_abs_diff, label = label),
    position = position_dodge(width = 1), hjust = 0,
    bg.r = 0.2, bg.color = 'white'
  ) +
  coord_flip() +
  labs(y = "Index Value (0-1)", fill = NULL, color = NULL, x = NULL) +
  scale_y_continuous(expand = expansion(c(0,0.15))) +
  scale_color_manual(values = colors) +
  scale_fill_manual(values = colors) +
  theme_bw(base_size =14) +
  theme(legend.position = "none", 
        panel.grid.minor = element_blank(), 
        panel.grid.major = element_blank(),
        plot.title = element_text(hjust = 0.5))

# # First version of figure
# gx = ggarrange(plotlist = list(g1,g2), ncol = 1, nrow = 2, labels = c("A", "B"), common.legend = TRUE, legend = "none")
# gg = ggarrange(plotlist = list(gx, g3, g4), ncol = 3, nrow = 1, labels = c(NA, "C", "D"), common.legend = TRUE, legend = "bottom") +
#   ggpubr::bgcolor("white") +
#   ggpubr::border(color = "white")
# 
# 
# ggsave(plot = gg, filename = "paper/figure_1b.png", dpi = 400, width = 12, height = 8)
# browseURL("paper/figure_1b.png")


# Redo Figure for better visual clarity
ga = ggarrange(
  plotlist = list(
    g1 + labs(y = NULL, title = "CO2e Reduction (tons)", x = "Year"),
    g2 + labs(y = NULL, title = "Cost (USD)", x = "Year"),
    g4 + labs(x = NULL, title = "Scenario Indices", y = "Index Scores (0-1)")),
  labels = c("B", "C", "D"),
  ncol = 3, nrow = 1, common.legend = TRUE, legend = "none")

gb = g3 +
  scale_x_discrete(labels = setNames(policies_data$simple, policies_data$id)) +
  facet_grid(~diversification,  labeller = as_labeller(
    c("≤1\n[A]" = "[A] Very Concentrated (≤1)",
      "≤0.1\n[B]" = "[B] Mixed (≤0.1)",
      "≤0.01\n[C]" = "[C] Very Diversified (≤0.01)")
  )
  ) +
  labs(x = NULL, title = "Policy Portfolio Mix by Scenario [A, B, C]")

gg2 = ggarrange(plotlist = list(gb,ga), ncol = 1, nrow = 2, labels = c("A", NA), common.legend = TRUE, legend = "bottom") +
  ggpubr::bgcolor("white") +
  ggpubr::border(color = "white")

ggsave(plot = gg2, filename = "paper/figure_1.png", dpi = 400, width = 12, height = 8)
browseURL("paper/figure_1.png")

