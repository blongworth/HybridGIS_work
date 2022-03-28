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
         co2flow = concCO2(time, r = 244) * 30) #30ul/min


# flow vs time subplot
flow_time <- data.frame(x = 0:125) %>% 
  ggplot(aes(x)) +
  stat_function(fun = function(x) concCO2(x, r = 244) * 30,
                aes(color = "CO2"), size = 1.5) +
  stat_function(fun = function(x) 30 - (concCO2(x, r = 244) * 30),
                aes(color = "Helium"), size = 1.5) +
  scale_color_manual("Gas", values = c("#00b7bd", "#b7bf10")) +
  labs(title = "A",
       #title = "Gas flows to source",
       #subtitle = "7mL vial, 244μL/min helium, 30μL/min to source",
       y = "Gas flow (μL/min)") +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        legend.position = c(0.87, 0.65),
        legend.background = element_rect(fill = "white", color = "black"))

# current vs time subplot
cur_time <- ggplot(data, aes(time, he12C)) +
  geom_smooth(span = .4, se = FALSE, color = "#00b7bd") +
  geom_point(size = 3, color = "#0069b1") +
  xlim(0, 125) +
  labs(title = "B",
       #title = "Ion current",
       x = "Time (min)",
       y = "12C current (μA)")


# Current vs flow subplot

cur_flow <- ggplot(data, aes(co2flow, he12C)) +
  geom_smooth(span = .4, se = FALSE, color = "#00b7bd") +
  geom_point(size = 3, color = "#0069b1") +
  labs(title = "C",
       #title = bquote('Ion current is stable for a range of CO'[2]~'flow'),
       #subtitle = bquote('Current vs. CO'[2]~'flow during vial dillution'),
       x = bquote('CO'[2]~'Flow (μl/min)'),
       y = TeX(r'(^{12}C^- current (μA))'))

# Build figure and save
flow_time / cur_time / cur_flow

ggsave(here("doc/AMS-15/AMS-15_paper/figures/fig2_current_flow.svg"))