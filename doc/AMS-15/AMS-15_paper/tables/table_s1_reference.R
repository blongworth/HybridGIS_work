# Table S1 Reference standards and blanks

library(tidyverse)
library(here)
library(hgis)
library(errors)

options(errors.notation = "plus-minus")
options(scipen = 999)

theme_set(theme_classic())

# Read data. Don't use first carbonate test, smaller samples, or gas standards.
results_df <- read_csv(here("data_analysed/carb_all_results.csv"))

cons_df <- results_df %>% 
  filter(!(wheel %in% c("USAMS020521", "USAMS061121", "USAMS061821"))) %>% 
  mutate(he12C = he12C * 1E6) %>% 
  compare_consensus() %>% 
  filter(!str_detect(Name, "Gas")) %>% 
  arrange(fm_consensus) %>% 
  mutate (fm_corr_err = set_errors(fm_corr, sig_fm_corr),
          Name = case_when(Name == "TIRI-F" ~ "TIRI F",
                           Name == "TIRI-I" ~ "TIRI I",
                           Name == "C-2" ~ "IAEA C2",
                           Name == "C-2" ~ "IAEA C1",
                           Name == "NOSAMS2" ~ "NOSAMS 2"))

cons_df %>% 
  select(Name, fm_consensus, fm_corr, sig_fm_corr, current = he12C) %>% 
  write_csv(here("data_analysed/reference_standards.csv"))

cons_df %>% 
  select(`Sample Name` = Name, 
         `Reference F^14^C` = fm_consensus,
         `HGIS F^14^C ± 1$\\sigma$` = fm_corr_err,
         `^12^C^-^ current ($\\mu$A)` = he12C) %>% 
  knitr::kable(digits = c(3, 4, 4, 1),
               caption = "\\label{tabS1}Measurements of reference standard materials. Reference 
               values are community consensus values for TIRI I and C2, and the 
               NOSAMS internal graphite value for NOSAMS2.")
