
# Bchron plots
# separate data and make cal curve for graphite and hgis

library(tidyverse)
library(here)
# library(hgis)
library(amstools)
library(Bchron)
library(IntCal)
# library(patchwork)

theme_set(theme_classic())


#####
# Read and combine data
#####

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

# Make cross ref table to allow joining graphite and hgis data
names <- results %>% 
  ungroup() %>% 
  filter(wheel == "USAMS062921") %>% 
  select(rec_num, sample_name)

# Get graphite results
graphite_results <- getRecOS(results$rec_num) %>% 
  filter(!is.na(reported)) %>% 
  select(rec_num, fm_corr = f_modern, sig_fm_corr = f_ext_error) %>% 
  mutate(method = "graphite") %>% 
  left_join(names, by = "rec_num")

# Join graphite and hgis results
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

# read core depth data
depth <- read_csv(here("data/haiti_depth.csv"))

# Join core depth data to combined results
combined_results <- left_join(combined_results, depth, by = "rec_num") %>% 
  mutate(rc_age = -8033 * log(fm_corr),
         sig_rc_age = rc_age - -8033 * log(fm_corr + sig_fm_corr))
write_csv(combined_results, here("data_analysed/haiti_combined.csv"))

# Remove outlier: HGIS sample leaked
cr_no <- combined_results %>% 
  filter(rec_num != 171996)


#####
# Import Bomb curve
#####

# load curve Hua 2020 NH1 Curve
bomb_NH2 <- IntCal::ccurve("nh2")

# create Bchron cal curve
createCalCurve("bomb_NH2", bomb_NH2[, 1], bomb_NH2[, 2], bomb_NH2[,3])

# Copy to bchron install directory
file.copy(
  from = "bomb_NH2.rda",
  to = system.file("data", package = "Bchron"),
  overwrite = TRUE
)

# create dataframes for BChron
# Need to add reservoir age correction
cal_sub <- cr_no %>% 
  mutate(id = paste(sample_name, "-", method)) %>% 
  select(sample_name, id, depth, rc_age, sig_rc_age, method) %>% 
  mutate(curve = if_else(rc_age > 0, "marine20", "bomb_NH2"),
         rc_age_corr = if_else(curve == "marine20", rc_age - 216, rc_age),
         sig_rc_age_corr = if_else(curve == "marine20", sqrt(sig_rc_age^2 + 92^2), sig_rc_age)) %>% 
  add_row(id = "Top", sample_name = "Top", depth = 0, 
          rc_age = -66, sig_rc_age = 1, 
          rc_age_corr = -66, sig_rc_age_corr = 1, 
          curve = "normal")


#####
# Calibrate dates
#####

cal_dates <- with(cal_sub,
  BchronCalibrate(rc_age_corr, sig_rc_age_corr, 
                  positions = depth, ids = id, 
                  calCurves = curve,
                  allowOutside = TRUE))
plot(cal_dates)


#####
# Make chronologies
#####

# S. Moser settings
# Bchronology(ages=ages, 
#             ageSds=ageSds, 
#             calCurves=calCurves, 
#             ids=id, 
#             positions=position, 
#             positionThicknesses=thickness, 
#             predictPositions=seq(0,896,by=1), 
#             allowOutside = TRUE, 
#             iterations=15000, 
#             burn=2000, 
#             thin = 12, 
#             extractDate = -66, 
#             thetaStart = NULL) 

# Filter out post bomb data
cal_sub_nb <- cal_sub %>% 
  filter(curve != "bomb_NH2")

# Separate hgis and graphite
cal_sub_hgis <- cal_sub_nb %>% 
  filter(method == "hgis" | is.na(method),
         !(str_starts(sample_name, "HATC"))) %>% # Remove duplicates
  arrange(depth)

cal_sub_gr <- cal_sub_nb %>% 
  filter(method == "graphite" | is.na(method)) %>% 
  arrange(depth)

# Make hgis chronology
# Some of the MCMC settings are causing a crash
chron_hgis  <- with(cal_sub_hgis,
                    Bchronology(rc_age_corr, sig_rc_age_corr, positions = depth, 
                                ids = sample_name, calCurves = curve,
                                #artificialThickness = 1,
                                allowOutside = TRUE
                                #iterations = 15000,
                                #burn = 2000,
                                #thin = 12,
                                #extractDate = -66
                                ))
plot(chron_hgis) +
  xlim(8000, -500) +
  ggtitle("HGIS chronology")
summary(chron_hgis, "convergence")

# Make graphite chronolgy
chron_gr  <- with(cal_sub_gr,
                  Bchronology(rc_age_corr, sig_rc_age_corr, positions = depth, 
                              ids = sample_name, calCurves = curve, 
                              #artificialThickness = 10.00,
                              allowOutside = TRUE,
                              extractDate = -66))
plot(chron_gr) +
  xlim(8000, -500) +
  ggtitle("Graphite chronology")

#predict(chron_gr, seq(0, 900, by = 50) )
#class(chron_gr)

# Save chronologies
write_rds(chron_hgis, here("data_analysed/hgis_chronology.rds"))
write_rds(chron_gr, here("data_analysed/graphite_chronology.rds"))
