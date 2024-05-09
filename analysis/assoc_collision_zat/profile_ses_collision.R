library(tidyverse)
library(sf)
library(broom)

#import data ----------------------
col_ped_zat <- readRDS("../../clean_data/collision/collision_zat_df.rds")
profile <- readRDS("../../clean_data/aggr_hclust_geo/calle2zat_geo.rds") %>% 
  st_drop_geometry()

profile %>% filter(is.na(clus))


# collision ~ profile ----------------------------------
profile_ped_zat <- col_ped_zat %>% 
  left_join(profile, by = "ZAT") %>% 
  mutate(clus = factor(clus)) %>% 
  drop_na(clus)

fit_injury <- glm.nb(injury ~ clus, data = profile_ped_zat)
summary(fit_injury)
injury_df <- tidy(fit_injury) %>% 
  mutate(outcome = 'injury')


fit_death <- glm.nb(death ~ clus, data = profile_ped_zat)
summary(fit_death)
death_df <- tidy(fit_death) %>% 
  mutate(outcome='death')


fit_total <- glm.nb(total ~ clus, data = profile_ped_zat)
summary(fit_total)
total_df <- tidy(fit_total) %>% 
  mutate(outcome='total') %>% 
  bind_rows(injury_df, death_df)

write_csv(total_df, file = "collision-profile_zat.csv")
