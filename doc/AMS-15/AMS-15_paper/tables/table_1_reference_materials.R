# Table 1 showing performance of reference standards and performance summary
# 


library(tidyverse)
library(hgis)
library(here)
library(gt)
library(errors)

options(digits = 3)
options(scipen = 4)
options(decimals = 4)
options(errors.notation = "plus-minus")

# Load data
results_df <- read_csv(here("data_analysed/carb_all_results.csv"))

# Format data
cons_df <- results_df %>% 
  filter(!(wheel %in% c("USAMS020521", "USAMS061121", "USAMS061821"))) %>% 
  mutate(he12C = he12C * 1E6) %>% 
  compare_consensus() %>% 
  filter(!str_detect(Name, "Gas"))

cons_df_sum <- cons_df %>% 
  group_by(Name, fm_consensus) %>% 
  summarize(across(c(fm_corr, sig_fm_corr, fm_diff, sigma),
                   list(mean = mean, sd = sd)),
            N = n()) %>% 
  ungroup()

# Summary table
cons_df_sum %>% 
  arrange(fm_consensus) %>% 
  mutate(across(c(fm_consensus, fm_corr_mean, fm_corr_sd, sig_fm_corr_mean), 
         function(x) x * 100),
         fm_corr_err = set_errors(fm_corr_mean, fm_corr_sd)) %>% 
  select(Name, `Consensus` = fm_consensus,
         `HGIS ± SD` = fm_corr_err,
         #`HGIS` = fm_corr_mean,
         #`Std. dev. of measurements` = fm_corr_sd,
         `Mean per-sample error` = sig_fm_corr_mean,
         N) %>% 
gt() %>% 
  tab_header(title = "Reference material precision and accuracy",
             subtitle = "Values in pMC, Errors are SD of measurements") %>% 
  fmt_number(4, 
             decimals = 1) %>% 
  cols_align("right", 3)

# Consensus summary
cons_sum <- cons_df %>% 
  filter(Name %in% c("TIRI-I", "C-2", "NOSAMS2")) %>% 
  ungroup() %>% 
  summarize(across(c(sig_fm_corr, fm_diff),
                   list(mean = mean, sd = sd)),
            N = n()) %>% 
  select(`Consensus difference` = fm_diff_mean,
         `Std. Dev. of consensus difference` = fm_diff_sd,
         `per-sample error` = sig_fm_corr_mean,
         N
         ) 
