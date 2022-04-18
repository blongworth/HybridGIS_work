# Figure 4 bland altman plot of carbonate consensus standards run as
# hgis compared to accepted value

library(tidyverse)
library(here)
library(hgis)
library(scales)

theme_set(theme_classic())

# Read data. Don't use first carbonate test, smaller samples, or gas standards.
results_df <- read_csv(here("data_analysed/carb_all_results.csv"))

cons_df <- results_df %>% 
  compare_consensus() %>% 
  filter(!(wheel %in% c("USAMS020521", "USAMS061121", "USAMS061821")),
         !str_detect(Name, "Gas")) %>% 
  mutate(he12C = he12C * 1E6,
         Name = case_when(Name == "TIRI-F" ~ "TIRI F",
                          Name == "TIRI-I" ~ "TIRI I",
                          Name == "C-2" ~ "IAEA C2",
                          Name == "C-1" ~ "IAEA C1",
                          Name == "NOSAMS2" ~ "NOSAMS 2"),
         Name = factor(Name, levels = c("IAEA C1", "TIRI F", "TIRI I", 
                                        "IAEA C2", "NOSAMS 2")))

mean_diff <- mean(cons_df$fm_diff)
sd_diff <- sd(cons_df$fm_diff)

# Generate plot
cons_df %>% 
ggplot(aes(fm_consensus, fm_diff, color = Name, shape = Name)) + 
  geom_hline(yintercept = 0) +
  geom_hline(yintercept = mean_diff, color = "blue") +
  geom_hline(yintercept = mean_diff + sd_diff, color = "lightblue") +
  geom_hline(yintercept = mean_diff - sd_diff, color = "lightblue") +
  #geom_smooth(method = "lm",  se = FALSE) + 
  #geom_boxplot(aes(fm_consensus, fm_diff, group = fm_consensus), width = .3) +
  geom_pointrange(aes(ymin = fm_diff - sig_fm_corr, 
                      ymax = fm_diff + sig_fm_corr), 
                  position = position_dodge2(width = 0.1), 
                  size = .6,
                  fatten = 5) + 
  scale_color_brewer(palette = "Dark2") +
  scale_shape_manual(values = c(2, 6, 5, 0, 1)) +
  # scale_color_manual(values = c("#00a9e0", "#0069b1", "#00b7bd", "#b7bf10", "#00b7bd")) +
  scale_y_continuous(breaks = seq(-0.03,0.03,0.01)) +
  labs(#title = "Agreement of HGIS with graphite measurements",
       #subtitle = "Blank corrected HGIS measurements of consensus carbonate standards",
       x = TeX(r"($F^{14}C$ expected (points jittered for clarity))"), 
       y = TeX(r"($F^{14}C$ difference (HGIS - expected))")) 

# Save figure
ggsave(here("doc/AMS-15/AMS-15_paper/figures/fig4_consensus_samples.svg"), width = 7, height = 4)
ggsave(here("doc/AMS-15/AMS-15_paper/figures/fig4_consensus_samples.pdf"), width = 7, height = 4)

