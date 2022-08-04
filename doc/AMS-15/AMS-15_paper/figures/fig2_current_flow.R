# Figure showing relationship of current, flow, and evolution over time
# TODO: combine into a single figure and polish

library(tidyverse)
library(here)
library(hgis)
library(patchwork)
library(latex2exp)

theme_set(theme_classic())


# process data from flow test samples

data <- process_hgis_results(here("data/USAMS040121R.txt"),
                             as.Date("2021-04-02")) %>%
  filter(pos == 5) %>% 
  mutate(time = cumsum(corr_lt) / 60,
         co2flow = concCO2(time, r = 244) * 30, #30ul/min
         he12C = he12C * 1E6)

# current vs time subplot
cur_time <- ggplot(data, aes(time, he12C)) +
  geom_smooth(span = .4, se = FALSE, color = "#00b7bd") +
  geom_point(size = 3, color = "#0069b1") +
  xlim(0, 125) +
  labs(title = "A",
       #title = "Ion current",
       x = "Time (min)",
       y = TeX(r"( $^{12}C^-$ current (μA))")) +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank())

# ratio vs time subplot
cur_rat <- ggplot(data, aes(time, he14_12)) +
  geom_smooth(span = .4, se = FALSE, color = "#00b7bd") +
  geom_point(size = 3, color = "#0069b1") +
  xlim(0, 125) +
  labs(title = "B",
       #title = "Ion current",
       x = "Time (min)",
       y = TeX(r"( $^\frac{14}{12}C^-$ ratio )")) +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank())

# flow vs time subplot
flow_time <- data.frame(x = 0:125) %>% 
  ggplot(aes(x)) +
  stat_function(fun = function(x) concCO2(x, r = 244) * 30,
                aes(color = "CO2"), size = 1.5) +
  stat_function(fun = function(x) 30 - (concCO2(x, r = 244) * 30),
                aes(color = "Helium"), size = 1.5) +
  scale_color_manual("Gas", values = c("#00b7bd", "#b7bf10")) +
  labs(title = "C",
       #title = "Gas flows to source",
       #subtitle = "7mL vial, 244μL/min helium, 30μL/min to source",
       x = "Time (min)",
       y = TeX(r"(Gas flow (μl $min^{-1}$))")) +
  theme(legend.position = c(0.87, 0.50),
        legend.background = element_rect(fill = "white", color = "black"))

# Current vs flow subplot
cur_flow <- ggplot(data, aes(co2flow, he12C)) +
  geom_smooth(span = .4, se = FALSE, color = "#00b7bd") +
  geom_point(size = 3, color = "#0069b1") +
  labs(title = "D",
       x = TeX(r"($CO^2$ flow (μl $min^{-1}$))"),
       y = TeX(r"( $^{12}C^-$ current (μA))")) 

# Build figure and save
cur_time / cur_rat / flow_time

ggsave(here("doc/AMS-15/AMS-15_paper/figures/fig2_current_flow.svg"), width = 7, height = 7)
ggsave(here("doc/AMS-15/AMS-15_paper/figures/fig2_current_flow.pdf"), width = 7, height = 7)

cur_flow

ggsave(here("doc/AMS-15/AMS-15_paper/figures/fig3_current_flow.svg"), width = 7, height = 7)
ggsave(here("doc/AMS-15/AMS-15_paper/figures/fig3_current_flow.pdf"), width = 7, height = 7)