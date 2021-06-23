# Combine all carbonate sample data

library(tidyverse)
library(here)

# 2021-02-05
cd1 <- tribble(~pos, ~name, ~rec_num, ~mass,
                      6, "C-1", 83028, 32.05,
                      7, "TIRI-F", 2138, 33.56,
                      9, "TIRI-I", 17185, 32.67,
                     10, "TIRI-I", 17185, 32.35,
                     11, "C-2", 1082, 31.35,
                     12, "C-2", 1082, 34.11,
                     13, "NOSAMS2", 38809, 32.30,
                     14, "NOSAMS2", 38809, 34.00
                     ) %>% 
  mutate(wheel = "USAMS020521")

## 2021-03-05 Carbonate samples
cd2 <- tribble(~pos, ~name, ~rec_num, ~mass,
                      7, "C1-1", 83028, 33.69,
                      8, "C1-2", 83028, 31.62,
                      9, "C2-1", 1082, 32.70,
                     10, "C2-2", 1082, 32.08,
                     11, "NOSAMS2-1", 38809, 33.23,
                     12, "NOSAMS2-2", 38809, 33.95
                     ) %>% 
  mutate(wheel = "USAMS030421")

## 2021-04-09 Carbonate samples
cd3 <- tribble(~pos, ~name, ~rec_num, ~mass,
                     31, "NOSAMS2_1", 38809, 31.8,
                     32, "NOSAMS2_2", 38809, 32.3,
                     33, "NOSAMS2_3", 38809, 33.5,
                     34, "TIRI-I_1", 17185, 33.1,
                     35, "TIRI-I_2", 17185, 31.8,
                     36, "TIRI-I_3", 17185, 31.8,
                     28, "TIRI-F_1", 2138, 31.9,
                     29, "TIRI-F_2", 2138, 33.4,
                     30, "TIRI-F_3", 2138, 32.4
                     ) %>% 
  mutate(wheel = "USAMS040121")

## 2021-04-16 Carbonate samples
cd4 <- tribble(~pos, ~name, ~rec_num, ~mass,
                     27, "C-1_1", 83028, 30.05,
                     28, "C-1_2", 83028, 36.00,
                     29, "C-1_3", 83028, 31.88,
                     30, "C-2_1", 1082, 32.57,
                     31, "C-2_2", 1082, 35.50,
                     32, "C-2_3", 1082, 34.10,
                     33, "NOSAMS-2_1", 38809, 35.20,
                     34, "NOSAMS-2_2", 38809, 32.19,
                     35, "NOSAMS-2_3", 38809, 33.20
                     ) %>% 
  mutate(wheel = "USAMS041521")

## 2021-05-14 Carbonate samples
cd5 <- tribble(~pos, ~name, ~rec_num, ~mass,
                     20, "C1_1", 83028, 31.61,
                     35, "C1_2", 83028, 34.30,
                     50, "C1_3", 83028, 33.17,
                     25, "TIRI-I_1", 17185, 32.74,
                     40, "TIRI-I_2", 17185, 31.58,
                     55, "TIRI-I_3", 17185, 33.25,
                     30, "NOSAMS2_1", 38809, 33.31,
                     45, "NOSAMS2_2", 38809, 31.89,
                     60, "NOSAMS2_3", 38809, 36.47
                     ) %>% 
  mutate(wheel = "USAMS051421")

# 2021-05-28 Pendleton Carbonate
cd6 <- tribble(~pos, ~name, ~rec_num, ~mass,
                    6, "C1_1", 83028, 30.73,
                    9, "C1_2", 83028, 31.59,
                   11, "C1_3", 83028, 30.80,
                    7, "C2_1", 1082, 32.72,
                   10, "C2_2", 1082, 30.52,
                   12, "C2_3", 1082, 25.06,
                   14, "HATC1-D2_2:7", 171989, 31.20,
                   15, "HATC1-D2_5:7", 171995, 35.40,
                   16, "HATC1-D2_6:7", 171999, 30.50
                     ) %>% 
  mutate(wheel = "USAMS052621")


## 2021-06-04 Small(er) sample tests
cd7 <- tribble(~pos, ~name, ~rec_num, ~mass,
                     18, "C-2_1", 1082, 6.03,
                     19, "C-2_2", 1082, 11.10,
                     20, "C-2_3", 1082, 15.52
                     ) %>% 
  mutate(wheel = "USAMS052621")

## 2021-06-11 Small(er) sample tests
cd8 <- tribble(~pos, ~name,         ~rec_num, ~mass, ~he_added,
                      4, "C1_1",       83028,    8.29,            5,
                      8, "C1_2",       83028,    9.01,            5,
                     12, "C1_3",       83028,    9.00,            5,
                     16, "C1_4",       83028,    8.14,            5,
                      5, "C2_1",        1082,    8.23,            5,
                      9, "C2_2",        1082,    8.77,            5,
                     13, "C2_3",        1082,    7.34,            5,
                      6, "NOSAMS-2_1",  38809,    7.72,            5,
                     10, "NOSAMS-2_2",  38809,    7.69,            5,
                     14, "NOSAMS-2_3",  38809,    8.86,            5
                     ) %>% 
  mutate(wheel = "USAMS061121")

# 2021-06-18 Small(er) sample tests
cd9 <- tribble(~pos, ~name, ~rec_num, ~mass, ~he_added,
                     23, "C-1_2", 83028, 9.71, 4.2,
                     27, "C-1_3", 83028, 7.51, 4.6,
                     24, "TIRI-I_1", 17185, 9.61, 5,
                     28, "TIRI-I_2", 17185, 7.89, 5,
                     31, "TIRI-I_3", 17185, 8.33, 5,
                     25, "NOSAMS2_1", 38809, 7.91, 5.5,
                     29, "NOSAMS2_2", 38809, 7.79, 5.2,
                     32, "NOSAMS2_3", 38809, 8.21, 5.2,
                     35, "NOSAMS2_5mg", 38809, 4.76, 6.4,
                     34, "NOSAMS2_2mg", 38809, 2.01, 6.8
                     ) %>% 
  mutate(wheel = "USAMS061821")

carb_data <- bind_rows(cd1, cd2, cd3, cd4, cd5, cd6, cd7, cd8, cd9) %>% 
  select(wheel, pos, name, rec_num, mass, he_added)

write_csv(carb_data, here("data/carb_data.csv"))