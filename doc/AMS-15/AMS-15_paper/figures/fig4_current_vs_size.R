# Relationship of sample size to ion current


# Read reduced data as produced by analyse_carbonates.R
df <- readRDS(here("data_analysed/carb_all.rds")) %>% 
  map_dfr(2) %>% 
  compare_consensus() %>% 
  mutate(c12ua = he12C * 1E6)
samples <- read_csv(here("data/carb_data.csv")) %>% 
  select(wheel, pos, mass, he_added)
df <- left_join(df, samples, by = c("wheel", "pos")) %>% 
  filter(wheel != "USAMS020521")


# Basic plot of current vs mass
df %>% 
ggplot(aes(mass, c12ua)) +
  geom_point()