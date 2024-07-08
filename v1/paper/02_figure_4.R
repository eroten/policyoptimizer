#' @name figure_4.R
#' @author Tim Fraser
#' @description
#' Script to run policy optimizer for each county 
#' in a state and visualize state level outcomes.


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

# Load policy optimizer package
setwd(paste0(rstudioapi::getActiveProject(), "/v1"))
devtools::load_all(".")




# VISUALIZE ##########################################

library(dplyr)
library(readr)
library(stringr)
library(sf)
library(ggplot2)
library(viridis)
library(ggpubr)

setwd(paste0(rstudioapi::getActiveProject(), "/v1"))
data = read_csv("paper/state_run_1.csv") %>%
  mutate(geoid = stringr::str_pad(geoid, width = 5, pad= "0", side = "left")) %>%
  # For each county, 
  group_by(geoid) %>%
  # get the cumulative records from the last year
  filter(year == max(year)) %>%
  select(geoid, year, cost, reduction98)


state = read_rds("paper/bounds.rds") %>%
  summarize(geom = st_union(geom))

bounds = read_rds("paper/bounds.rds") %>%
  left_join(by = "geoid", y = data)

# For testing only, let's add some randomness
#bounds$cost = rnorm(n = nrow(bounds), mean = 125684, sd = 10000)
#bounds$reduction = rnorm(n = nrow(bounds), mean = 260, sd = 50)


colors = viridis::mako(n = 5, end = 0.8)



g1 = ggplot() +
  # Background lightgrey
  geom_sf(data = state, fill = NA, color = "lightgrey", linewidth = 3) +
  # Make county polygons
  geom_sf(data = bounds, mapping = aes(fill = cost), color = "white", linewidth = 0.1) +
  scale_fill_gradient(
    low = "white", high = colors[2],
    breaks = seq(from = min(bounds$cost), to = max(bounds$cost), length.out = 5),
    labels = scales::label_number(prefix = "$", scale_cut = scales::cut_si(""))
    ) +
  # State black outline
  geom_sf(data = state, fill = NA, color = "black", linewidth = 1) +
  theme_void(base_size = 14) +
  coord_sf() +
  guides(fill = guide_colorsteps(barwidth = 15, show.limits = TRUE, frame.colour = "#373737")) +
  theme(legend.position = "bottom", plot.title = element_text(hjust = 0.5)) +
  ggspatial::annotation_north_arrow(location = "bl", height = unit(0.50, "cm"), width = unit(0.50, "cm"),
                                    style = ggspatial::north_arrow_orienteering(fill = c("white", "black"))) +
  labs(title = "Cumulative Cost", fill = "Cost (USD)    \n2025-2050")

g2 = ggplot() +
  # Background lightgrey
  geom_sf(data = state, fill = NA, color = "lightgrey", linewidth = 3) +
  # Make county polygons
  geom_sf(data = bounds, mapping = aes(fill = reduction98), color = "white", linewidth = 0.1) +
  scale_fill_gradient(
    low = "white", high = colors[4],
    breaks = seq(from = min(bounds$reduction98), to = max(bounds$reduction98), length.out = 5),
    labels = scales::label_number(scale_cut = scales::cut_si(""))                  
  ) +
  # State black outline
  geom_sf(data = state, fill = NA, color = "black", linewidth = 1) +
  theme_void(base_size = 14) +
  guides(fill = guide_colorsteps(barwidth = 15, show.limits = TRUE, frame.colour = "#373737")) +
  theme(legend.position = "bottom", plot.title = element_text(hjust = 0.5)) +
  # Add scale and arrow
  ggspatial::annotation_scale(location = "bl", text_cex = 1.0) +
  labs(title = "Cumulative CO2 Emissions Reduction", fill = "ΔEmissions (tons)\n2025-2050")


gg = ggarrange(g1,g2,labels = c("A", "B"), nrow = 1)

ggsave(gg, filename = "paper/figure_4.png", dpi = 500, width = 10, height = 5)
browseURL("paper/figure_4.png")


# 
# ggplot() +
#   geom_line(data = alldata, mapping = aes(x = year, y = reduction, group = geoid), color = "lightgrey")
# 
# ggplot() +
#   geom_density(data = data, mapping = aes(x = reduction, group = year)) +
#   scale_x_continuous(trans = "log", labels = scales::label_number(scale_cut = scales::cut_si("t")))
# 
# 
# ggplot() +
#   geom_jitter(data = data, mapping = aes(x = reduction, y = cost, group = geoid), width = 0.25, height = 0.25)  
# #  scale_y_continuous(trans = "log", labels = scales::label_number(prefix = "$", scale_cut = scales::cut_si(""))) +
# #  scale_x_continuous(trans = "log", labels = scales::label_number(scale_cut = scales::cut_si("t")))

