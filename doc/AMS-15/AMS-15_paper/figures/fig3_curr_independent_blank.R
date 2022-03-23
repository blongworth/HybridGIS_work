# Shows uncorrected blank Fm's vs current
# Need to investigate high outliers- bad wheel?

# Libraries
library(tidyverse)
library(here)

# Get data
carb <- readRDS(here("data_analysed/carb_all.rds")) %>% 
  map_dfr(2) 

# Filter blanks used for correction

blanks <- carb %>% 
  filter(rec_num %in% c(2138, 83028),
         he12C > 2E-6,
         sample_type == "B")
# Plot
blanks %>% 
  mutate(Type = ifelse(rec_num == 2138, "TIRI-F", "IAEA C-1")) %>%
  ggplot(aes(he12C, norm_ratio)) +
  geom_errorbar(aes(ymin = norm_ratio - sig_norm_ratio,
                    ymax = norm_ratio + sig_norm_ratio)) +
  geom_errorbarh(aes(xmin = he12C - sig_he12C, xmax = he12C + sig_he12C)) +
  geom_point(aes(color = wheel), size = 3) +
  labs(title = "Blanks",
       x = "12C Current (μA)",
       y = "Fraction modern") +
  theme_classic() +
  theme(legend.position = c(0.82, 0.76),
        legend.background = element_rect(fill = "white", color = "black"))

ggsave("fig3_cur_indep_blank.svg")

ggsave(here("doc/AMS-15/AMS-15_paper/figures/fig3_cur_indep_blank.svg"))