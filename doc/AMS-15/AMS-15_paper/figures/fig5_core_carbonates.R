# Figure 6 bland altman plot of haiti core carbonates
# hgis compared to graphite value

library(tidyverse)
library(here)
library(hgis)
library(amstools)
library(scales)
library(gt)
library(Bchron)
library(patchwork)

theme_set(theme_classic())

# Read raw and reduced data as produced by analyse_carbonates.R
data <- readRDS(here("data_analysed/carb_all.rds"))

# Read sample data
sample_data <- read_csv(here("data/carb_data.csv"))

# Select relevant reduced data, join with sample data
results <- map_dfr(data, 2) %>% 
  filter(wheel %in% c("USAMS052621", "USAMS062921"),
         pos > 4) %>% 
  left_join(sample_data, by = c("wheel", "pos")) %>% 
  mutate(rec_num = ifelse(is.na(rec_num.y), rec_num.x, rec_num.y)) %>% 
  select(-rec_num.x, -rec_num.y)

names <- results %>% 
  ungroup() %>% 
  filter(wheel == "USAMS062921") %>% 
  select(rec_num, sample_name)

graphite_results <- getRecOS(results$rec_num) %>% 
  filter(!is.na(reported)) %>% 
  select(rec_num, fm_corr = f_modern, sig_fm_corr = f_ext_error) %>% 
  mutate(method = "graphite") %>% 
  left_join(names, by = "rec_num")

combined_results <- results %>% 
  filter(str_starts(sample_name, "HATC") | str_starts(sample_name, "\\d")) %>% 
  mutate(method = "hgis") %>% 
  select(wheel, sample_name, method, rec_num, fm_corr, sig_fm_corr) %>% 
  bind_rows(graphite_results) %>% 
  group_by(rec_num) %>% 
  mutate(fm_mean = mean(fm_corr),
         fm_diff_mean = fm_corr - fm_mean) %>% 
  left_join(rename(names, Name = sample_name), by = "rec_num") %>% 
  left_join(select(graphite_results, rec_num, fm_corr_gr = fm_corr)) %>% 
  mutate(fm_diff = fm_corr - fm_corr_gr) %>% 
  ungroup()

depth <- read_csv(here("data/haiti_depth.csv"))

combined_results <- left_join(combined_results, depth, by = "rec_num") %>% 
  mutate(rc_age = -8033 * log(fm_corr),
         sig_rc_age = rc_age - -8033 * log(fm_corr + sig_fm_corr))
write_csv(combined_results, here("data_analysed/haiti_combined.csv"))

# Remove outlier: HGIS sample leaked
# Remove HATC duplicates
cr_no <- combined_results %>% 
  filter(rec_num != 171996,
         !str_starts(sample_name, "HATC"))

mean_diff <- cr_no %>% 
  filter(method == "hgis") %>% 
  summarise(across(fm_diff, list(mean = mean, sd = sd)))

mean_errs <- cr_no %>% 
  group_by(method) %>% 
  summarise(mean_sig = mean(sig_fm_corr)) %>% 
  pivot_wider(names_from = method,
              values_from = mean_sig)


# Make difference plot of core carbonates

core_compare <- ggplot(cr_no) +
  geom_hline(yintercept = 0) +
  # geom_hline(yintercept = mean_diff$fm_diff_mean, color = "blue") +
  # geom_hline(yintercept = mean_diff$fm_diff_mean + mean_diff$fm_diff_sd, color = "lightblue") +
  # geom_hline(yintercept = mean_diff$fm_diff_mean - mean_diff$fm_diff_sd, color = "lightblue") +
  geom_pointrange(aes(fm_mean, fm_diff, color = method, shape = method,
                      ymin = fm_diff - sig_fm_corr, 
                      ymax = fm_diff + sig_fm_corr),
                  position = "jitter") +
  scale_color_manual(values = c("#00b7bd", "#b7bf10")) +
  labs(title = "A",
       #subtitle = "Sediment core macrofossils measured via HGIS and graphite",
       x = "Mean of measurements (F14C)",
       y = "HGIS - graphite (F14C)") +
  theme(legend.position = "none")
  # theme(legend.position = c(0.25, 0.85),
  #       legend.direction = "horizontal") #,
  #       #legend.background = element_rect(fill = "white", color = "black")) 


# Simple age depth plot
# If this is used, should use calibrated ages
# or at least bomb correction

# age_depth <- cr_no %>% 
#   ggplot(aes(depth, rc_age, color = method, shape = method)) +
#   geom_pointrange(aes(ymin = rc_age - sig_rc_age, 
#                       ymax = rc_age + sig_rc_age),
#                   size = 0.5) +
#   geom_smooth(alpha = .2) +
#   coord_flip() +
#   scale_y_reverse() +
#   scale_x_reverse() +
#   scale_color_manual(values = c("#00b7bd", "#b7bf10")) +
#   labs(title = "B",
#        #subtitle = "Age vs. depth for graphite and HGIS",
#        y = "Radiocarbon years (BP)",
#        x = "Core depth (cm)") +
#   theme(legend.position = c(0.85, 0.30),
#         legend.background = element_rect(fill = "white", color = "black")) 

# Bchron age models

# Read chronologies

chron_hgis <- read_rds(here("data_analysed/hgis_chronology.rds"))
chron_gr <- read_rds(here("data_analysed/graphite_chronology.rds"))

# Individual plots of HGIS and graphite models
hgis_bchron <- plot(chron_hgis, chronCol = "#00b7bd", dateLabels = FALSE) +
  xlim(8000, -500) +
  labs(title = "B",
       x = "Years BP",
       y = "Core depth (cm)")

gr_bchron <- plot(chron_gr, chronCol = "#b7bf10", dateLabels = FALSE) +
  xlim(8000, -500) +
  labs(title = "C",
       x = "Years BP",
       y = "Core depth (cm)") +
  theme(axis.title.y = element_blank(),
        axis.text.y = element_blank())

# Generate hgis dataframe for combined plot
alpha <- 0.95
chronRange <- data.frame(
  chronLow = apply(chron_hgis$thetaPredict, 2, "quantile", probs = (1 - alpha) / 2),
  chronMed = apply(chron_hgis$thetaPredict, 2, "quantile", probs = 0.5),
  chronHigh = apply(chron_hgis$thetaPredict, 2, "quantile", probs = 1 - (1 - alpha) / 2),
  positions = chron_hgis$predictPositions
)
ageGrid <- with(chronRange, seq(min(chronLow), max(chronHigh),
                                length = nrow(chronRange)
))
chronRangeSwap_hgis <- data.frame(
  Age = ageGrid,
  positionLow = with(chronRange, approx(chronLow, positions,
                                        xout = ageGrid,
                                        rule = 2
  )$y),
  Position = with(chronRange, approx(chronMed, positions,
                                     xout = ageGrid,
                                     rule = 2
  )$y),
  positionHigh = with(chronRange, approx(chronHigh, positions,
                                         xout = ageGrid,
                                         rule = 2
  )$y),
  Date = "Bchron",
  densities = NA,
  height = NA
)

# Generate graphite dataframe for combined plot
alpha <- 0.95
chronRange <- data.frame(
  chronLow = apply(chron_gr$thetaPredict, 2, "quantile", probs = (1 - alpha) / 2),
  chronMed = apply(chron_gr$thetaPredict, 2, "quantile", probs = 0.5),
  chronHigh = apply(chron_gr$thetaPredict, 2, "quantile", probs = 1 - (1 - alpha) / 2),
  positions = chron_gr$predictPositions
)
# ageGrid <- with(chronRange, seq(min(chronLow), max(chronHigh),
#                                 length = nrow(chronRange)
# ))
chronRangeSwap <- data.frame(
  Age = ageGrid,
  positionLow = with(chronRange, approx(chronLow, positions,
                                        xout = ageGrid,
                                        rule = 2
  )$y),
  Position = with(chronRange, approx(chronMed, positions,
                                     xout = ageGrid,
                                     rule = 2
  )$y),
  positionHigh = with(chronRange, approx(chronHigh, positions,
                                         xout = ageGrid,
                                         rule = 2
  )$y),
  Date = "Bchron",
  densities = NA,
  height = NA
)

# Combine data, add mean and difference
chronRangeSwap_hg <- chronRangeSwap_hgis %>% 
  mutate(method = "hgis")
chronRangeSwap_gr <- chronRangeSwap %>% 
  mutate(method = "graphite")
chron_comb <- bind_rows(chronRangeSwap_hg, chronRangeSwap_gr)

chron_comb_mean <- chron_comb %>% 
  pivot_wider(id_cols = Age, names_from = method, values_from = c(Position, positionLow, positionHigh)) %>% 
  mutate(pos_mean = (Position_hgis + Position_graphite) / 2) %>% 
  select(Age, pos_mean)

chron_comb <- chron_comb %>% 
  left_join(chron_comb_mean) %>% 
  mutate(pos_diff = Position - pos_mean,
         pos_low_diff = positionLow - pos_mean,
         pos_high_diff = positionHigh - pos_mean)

# Combined plot, no age histograms
chron_combined <- ggplot(chron_comb,
       aes(x = Age, y = Position,
           ymin = positionLow, ymax = positionHigh, 
           color = method, fill = method)) +
  geom_ribbon(alpha = 0.4, color = NA) +
  geom_line(size = 1) +
  xlim(8000, -500) +
  labs(title = "D",
       x = "Years BP",
       y = "Core depth (cm)") +
  scale_color_manual(values = c("#00b7bd", "#b7bf10")) +
  scale_fill_manual(values = c("#00b7bd", "#b7bf10")) +
  scale_y_reverse() +
  # theme(legend.position = "none")
  theme(axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        legend.position = c(0.25, 0.80),
        legend.direction = "horizontal",
        legend.background = element_rect(fill = "white", color = "black")) 

# Difference plot
chron_diff <- ggplot(chron_comb, aes(Age, pos_diff, color = method,
                       ymin = pos_low_diff, ymax = pos_high_diff, 
                       fill = method)) +
  geom_hline(yintercept = 0) +
  geom_ribbon(alpha = 0.4, color = NA) +
  geom_line(size = 1) +
  scale_color_manual(values = c("#00b7bd", "#b7bf10")) +
  scale_fill_manual(values = c("#00b7bd", "#b7bf10")) +
  scale_x_reverse() +
  xlim(8000, -500) +
  labs(title = "E",
       x = "Years BP",
       y = "Offset (cm)") +
  theme(legend.position = "none")

# Combine plots and save

core_compare / (hgis_bchron | gr_bchron) / chron_combined / chron_diff +
  plot_layout(heights = c(2, 3, 3, 1))

# Save figure
ggsave(here("doc/AMS-15/AMS-15_paper/figures/fig5_core_carbonates.svg"), width = 7, height = 10)
ggsave(here("doc/AMS-15/AMS-15_paper/figures/fig5_core_carbonates.pdf"), width = 7, height = 10)
