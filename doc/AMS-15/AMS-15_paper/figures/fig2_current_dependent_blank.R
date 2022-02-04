# Current dependent blank model and data
# Plot of data and model with measured blanks
# This should be remade with more data and functions
# Use sample depletion data?
# Or skip altogether!

data<- get_hgis_data(here("data/USAMS101320R.txt"), as.Date("2020-11-17")) %>% 
  mutate(Num = ifelse(Pos %in% c(2, 4), "S", ifelse(Num == "S", "U", Num)))
stdrat <- mean(data$cor1412he[data$Num == "S" & !data$outlier])
data1013 <- data %>% mutate(normFm = norm_gas(cor1412he, stdrat))

data <- get_hgis_data(here("data/USAMS120320R.txt"), as.Date("2020-12-04")) %>% 
  mutate(Num = ifelse(Pos %in% c(2, 4), "S", ifelse(Num == "S", "U", Num)))
stdrat <- mean(data$cor1412he[data$Num == "S" & !data$outlier])
data1204 <- data %>% mutate(normFm = norm_gas(cor1412he, stdrat))

data1211 <- get_hgis_data(here("data/USAMS120320R.txt")) %>% 
  filter(Pos %in% 1:4 | as.Date(ts) == "2020-12-11")

data <- get_hgis_data(here("data/USAMS120320R.txt"), as.Date("2020-12-18")) %>% 
  filter(Pos != 0) %>% 
  mutate(Num = ifelse(Pos %in% c(2, 4), "S", ifelse(Num == "S", "U", Num)))
stdrat <- mean(data$cor1412he[data$Num == "S" & !data$outlier])
data1218 <- data %>% mutate(normFm = norm_gas(cor1412he, stdrat))

data0108 <- get_hgis_data(here("data/USAMS120320R.txt"), as.Date("2021-01-08"))

data <- get_hgis_data(here("data/USAMS020521R.txt")) %>% 
  mutate(Num = ifelse(Pos %in% c(2, 4), "S", ifelse(Num == "S", "U", Num)))
stdrat <- mean(data$cor1412he[data$Num == "S" & !data$outlier])
data205 <- data %>% mutate(normFm = norm_gas(cor1412he, stdrat))

data <- get_hgis_data(here("data/USAMS030421R.txt"), as.Date("2021-03-05")) %>% 
  mutate(Num = ifelse(Pos %in% c(2, 4), "S", ifelse(Num == "S", "U", Num)))
stdrat <- mean(data$cor1412he[data$Num == "S" & !data$outlier])
data304 <- data %>% mutate(normFm = norm_gas(cor1412he, stdrat))

data <- get_hgis_data(here("data/USAMS040121R.txt"), as.Date("2021-04-09")) %>% 
  mutate(Num = ifelse(Pos %in% c(22, 24), "S", ifelse(Num == "S", "U", Num)))
stdrat <- mean(data$cor1412he[data$Num == "S" & !data$outlier])
data409 <- data %>% mutate(normFm = norm_gas(cor1412he, stdrat))

data <- get_hgis_data(here("data/USAMS041521R.txt"), as.Date("2021-04-16"))  %>% 
  mutate(Num = ifelse(Pos %in% c(22, 24), "S", ifelse(Num == "S", "U", Num)))
stdrat <- mean(data$cor1412he[data$Num == "S" & !data$outlier])
data415 <- data %>% mutate(normFm = norm_gas(cor1412he, stdrat))

std <- getStdTable()

data <- rbind(data1013, data1204, data1211, data1218, data205, data304, data409, data415) %>% 
  mutate(rec_num = case_when(str_detect(Sample.Name, "LiveGas") ~ 101730,
                             str_starts(Sample.Name, "C-1") ~ 83028,
                             str_starts(Sample.Name, "C1") ~ 83028,
                             str_starts(Sample.Name, "TIRI-F") ~ 2138,
                             str_starts(Sample.Name, "TIRI-I") ~ 17185,
                             str_starts(Sample.Name, "C-2") ~ 1082,
                             str_starts(Sample.Name, "C2") ~ 1082,
                             str_starts(Sample.Name, "NOSAMS") ~ 38809,
                             str_detect(Sample.Name, "DeadGas") ~ 72446)) %>% 
 left_join(select(std, rec_num, fm_consensus), by = "rec_num") %>% 
 mutate(fm_consensus = case_when(rec_num == 101730 ~ 1.0398,
                                 rec_num == 72446 ~ 0.0013,
                                 TRUE ~ fm_consensus))

# Summarize by target
ds <- sum_hgis(data) %>% 
  filter((str_detect(Sample.Name, "LiveGas") & Cur > 1) |
         (str_detect(Sample.Name, "DeadGas") & Cur > 1) |
          str_detect(Sample.Name, "Blank")) %>% 
  mutate(Gas = case_when(str_detect(Sample.Name, "LiveGas") ~ "LiveGas",
                         str_detect(Sample.Name, "DeadGas") ~ "DeadGas",
                         TRUE ~ "Blank"),
         Cur_inv = 1/Cur)

# Fit linear model
fits <- ds %>% 
  ungroup() %>% 
  nest(data = -Gas) %>% 
  mutate(fit = map(data, ~lm(mean ~ Cur_inv, data = .x)),
         tidied = map(fit, tidy)) %>% 
  unnest(tidied) 

blankfit <- fits %>% 
  select(Gas, term, estimate) %>% 
  pivot_wider(names_from = c(Gas, term), values_from = estimate) %>% 
  mutate(inv_m_blank = -(`DeadGas_(Intercept)` - `LiveGas_(Intercept)`)/(DeadGas_Cur_inv - LiveGas_Cur_inv),
         Fm_blank = `DeadGas_(Intercept)` + DeadGas_Cur_inv * inv_m_blank,
         m_blank = 1/inv_m_blank) 

# Modeled Fm and mass of blank
blank_val <- blankfit %>% 
  select(Fm_blank, m_blank)

ggplot(ds, aes(Cur_inv, mean, color = Gas)) +
  geom_abline(slope = blankfit$LiveGas_Cur_inv, 
              intercept = blankfit$`LiveGas_(Intercept)`,
              color = "#0069b1") +
  geom_abline(slope = blankfit$DeadGas_Cur_inv, 
              intercept = blankfit$`DeadGas_(Intercept)`,
              color = "#00a9e0") +
  geom_pointrange(aes(ymin = mean - merr, ymax = mean + merr), size = .3) + 
  scale_color_manual("Gas", values = c("#b7bf10", "#00a9e0", "#0069b1")) +
  xlim(0, 10) +
  labs(title = "Modeled blank",
       subtitle = "Fits cross at Fm and current of blank",
       x = "Inverse 12C Current (μA-1)",
       y = "Fraction modern") +
  theme_classic() +
  theme(legend.position = "none",
        legend.background = element_rect(fill = "white", color = "black")) 