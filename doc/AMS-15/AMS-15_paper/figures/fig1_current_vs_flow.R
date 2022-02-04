# Figure showing relationship of current, flow, and evolution over time
# TODO: combine into a single figure and polish

library(tidyverse)
library(here)
library(hgis)
library(patchwork)
library(latex2exp)

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
  labs(title = "Gas flows to source",
       subtitle = "7mL vial, 244μL/min helium, 30μL/min to source",
       y = "Gas flow (μL/min)") +
  theme_classic() +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        legend.position = c(0.87, 0.65),
        legend.background = element_rect(fill = "white", color = "black"))

# current vs time subplot
cur_time <- ggplot(data, aes(time, he12C)) +
  geom_smooth(span = .4, se = FALSE, color = "#00b7bd") +
  geom_point(size = 3, color = "#0069b1") +
  xlim(0, 125) +
  labs(title = "Ion current",
       x = "Time (min)",
       y = "12C current (μA)") +
  theme_classic()

flow_time / cur_time

ggsave("fig1a_flow_current_time.svg")

ggplot(data, aes(co2flow, he12C)) +
  geom_smooth(span = .3, se = FALSE, color = "#00b7bd") +
  geom_point(size = 2, color = "#0069b1") +
  labs(title = bquote('Ion current is stable for a range of CO'[2]~'flow'),
       subtitle = bquote('Current vs. CO'[2]~'flow during vial dillution'),
       x = bquote('CO'[2]~'Flow (μl/min)'),
       y = TeX(r'(^{12}C^- current (μA))')) +
  theme_classic()

ggsave("fig1b_current_flow.svg")
