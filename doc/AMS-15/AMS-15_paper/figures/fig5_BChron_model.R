
# Bchron plots
# separate data and make cal curve for graphite and hgis
# How to handle bomb dates
# use IntCal package and assign bomb curve

library(tidyverse)
library(here)
library(hgis)
library(amstools)
library(scales)
library(gt)
library(Bchron)
library(IntCal)
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

# No outliers
cr_no <- combined_results %>% 
  filter(abs(fm_diff_mean) < 0.02)


# Import Bomb curve

# load curve Hua 2020 NH1 Curve
bomb_NH1 <- IntCal::ccurve(postbomb = TRUE)

# create Bchron cal curve
createCalCurve("bomb_NH1", bomb_NH1[, 1], bomb_NH1[, 2], bomb_NH1[,3])

# Copy to bchron install directory
file.copy(
  from = "bomb_NH1.rda",
  to = system.file("data", package = "Bchron"),
  overwrite = TRUE
)

# create dataframes for BChron
cal_sub <- cr_no %>% 
  mutate(id = paste(sample_name, "-", method)) %>% 
  select(sample_name, id, depth, rc_age, sig_rc_age, method) %>% 
  mutate(curve = if_else(rc_age > 0, "marine20", "bomb_NH1")) %>% 
  add_row(id = "Top", sample_name = "Top", depth = 0, rc_age = 0, sig_rc_age = 1, curve = "normal")

cal_dates <- with(cal_sub,
  BchronCalibrate(rc_age, sig_rc_age, positions = depth, ids = id, calCurves = curve))
plot(cal_dates)

cal_sub_hgis <- cal_sub %>% 
  filter(method == "hgis" | is.na(method))

cal_sub_gr <- cal_sub %>% 
  filter(method == "graphite" | is.na(method)) 

# Calibrate hgis ages
cal_dates_hgis <- with(cal_sub_hgis,
  BchronCalibrate(rc_age, sig_rc_age, 
                  positions = depth, ids = sample_name, calCurves = curve))

# calibrate graphite dates
cal_dates_gr  <- with(cal_sub_gr,
  BchronCalibrate(rc_age, sig_rc_age, 
                  positions = depth, ids = sample_name, 
                  calCurves = curve))

# make dataframe with cal ages
cal_bp_gr <- map(cal_dates_gr, 1)
sig_cal_bp_gr <- map(cal_dates_gr, 2)


plot(cal_dates_hgis) +
  ggtitle("HGIS dates")

# Make hgis chronology
chron_hgis  <- with(cal_sub_hgis,
                    Bchronology(rc_age, sig_rc_age, positions = depth, 
                                ids = sample_name, calCurves = curve,
                                artificialThickness = 1))
plot(chron_hgis) +
  ggtitle("HGIS chronology")
summary(chron_hgis, "convergence")

plot(cal_dates_gr) +
  ggtitle("Graphite dates")

# Make graphite chronolgy
chron_gr  <- with(cal_sub_gr,
                  Bchronology(rc_age, sig_rc_age, positions = depth, 
                              ids = sample_name, calCurves = curve, 
                              artificialThickness = 1.00))
plot(chron_gr) +
  ggtitle("Graphite chronology")

#predict(chron_gr, seq(0, 900, by = 50) )
#class(chron_gr)
