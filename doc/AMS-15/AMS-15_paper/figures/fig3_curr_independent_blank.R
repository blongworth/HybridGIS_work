# Shows uncorrected blank Fm's vs current
# Need to investigate high outliers- bad wheel?

# Get data
carb_data <- read_csv(here("data_analysed/carb_all_results.csv"))

# Plot
carb_data %>% 
    filter(rec_num %in% c(2138, 83028),
                     he12C > 2E-6) %>%
    mutate(Type = ifelse(rec_num == 2138, "TIRI-F", "IAEA C-1")) %>%
  ggplot(aes(he12C, norm_ratio)) +
  geom_errorbar(aes(ymin = norm_ratio - sig_norm_ratio,
                    ymax = norm_ratio + sig_norm_ratio)) +
  geom_errorbarh(aes(xmin = he12C - sig_he12C, xmax = he12C + sig_he12C)) +
  geom_point(aes(color = Type), size = 3) +
  labs(title = "Blanks",
       x = "12C Current (μA)",
       y = "Fraction modern") +
  theme_classic() +
  theme(legend.position = c(0.82, 0.76),
        legend.background = element_rect(fill = "white", color = "black"))
