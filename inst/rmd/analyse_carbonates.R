  # Reduce HGIS data
# Script to reduce all available wheels of HIGS data using
# most current data reduction method.

library(tidyverse)
library(here)
library(amstools)
library(HybridGIS)


# TODO: Handle case of only one blank

# Analysis metadata: wheels to analyse, outliers, positions of standards and blanks
wheels <- c("020521", "030421", "040121", "041521", "051421", "052621", "061121", "061821")
files <- here("data", paste0("USAMS", wheels, "R.txt"))
dates <- as.Date(c("2021-02-05", "2021-03-05", "2021-04-09", "2021-04-16", "2021-05-14", "2021-05-28", "2021-06-11", "2021-06-18"))
stds <- list(c(5,8), NULL, c(26,37), NULL, c(10,15,65,70), NULL, NULL, NULL)
blanks <- list(7, c(7,8), c(28,29,30), c(27,28,29), c(20,35,50), NULL, NULL, 23)
outliers <- list(NULL, tibble(pos = c(6, 8, 8, 8), meas = c(18, 1, 2, 3)),
                 tibble(pos = c(28,29), meas=c(1, 1)),
                 NULL,
                 tibble(pos=10, meas=1),
                 tibble(pos=c(8,9,13), meas=c(6,1,1)),
                 tibble(pos=c(4,5,5,6,7,8,15), meas=c(1,1,4,10,3,3,2)),
                 NULL)
                 

results <- pmap(list(files, dates, stds, blanks, outliers), reduce_hgis)
raw_df <- map_dfr(results, 1)
results_df <- map_dfr(results, 2)

write_csv(raw_df, here("data_analysed/carb_all_raw.csv"))
write_csv(results_df, here("data_analysed/carb_all_results.csv"))