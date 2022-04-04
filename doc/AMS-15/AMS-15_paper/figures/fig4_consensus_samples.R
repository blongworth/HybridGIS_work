# Figure 5 bland altman plot of carbonate consensus standards run as
# hgis compared to accepted value

library(tidyverse)
library(here)
library(hgis)
library(scales)

theme_set(theme_classic())

# Read data. Don't use first carbonate test, smaller samples, or gas standards.
results_df <- read_csv(here("data_analysed/carb_all_results.csv"))

cons_df <- results_df %>% 
  filter(!(wheel %in% c("USAMS020521", "USAMS061121", "USAMS061821"))) %>% 
  mutate(he12C = he12C * 1E6) %>% 
  compare_consensus() %>% 
  filter(!str_detect(Name, "Gas"))

mean_diff <- mean(cons_df$fm_diff)
sd_diff <- sd(cons_df$fm_diff)

# Generate plot
cons_df %>% 
ggplot(aes(fm_consensus, fm_diff, color = Name, shape = Name)) + 
  geom_hline(yintercept = 0) +
  geom_hline(yintercept = mean_diff, color = "blue") +
  geom_hline(yintercept = mean_diff + sd_diff, color = "lightblue") +
  geom_hline(yintercept = mean_diff - sd_diff, color = "lightblue") +
  geom_smooth(method = "lm",  se = FALSE) + 
  geom_pointrange(aes(ymin = fm_diff - sig_fm_corr, 
                      ymax = fm_diff + sig_fm_corr), 
                  position = position_dodge2(width = 0.1), 
                  ) + #size = 0.5) + 
  scale_color_brewer(palette = "Dark2") +
  # scale_color_manual(values = c("#00a9e0", "#0069b1", "#00b7bd", "#b7bf10", "#00b7bd")) +
  scale_y_continuous(breaks = seq(-0.03,0.03,0.01)) +
  labs(#title = "Agreement of HGIS with graphite measurements",
       #subtitle = "Blank corrected HGIS measurements of consensus carbonate standards",
       x = "F14C expected (points jittered for clarity)", 
       y = "F14C difference (HGIS - expected)")

# Save figure
ggsave(here("doc/AMS-15/AMS-15_paper/figures/fig4_consensus_samples.svg"), width = 7, height = 4)
ggsave(here("doc/AMS-15/AMS-15_paper/figures/fig4_consensus_samples.pdf"), width = 7, height = 4)

