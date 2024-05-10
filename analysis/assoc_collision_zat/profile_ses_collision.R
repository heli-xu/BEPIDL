library(tidyverse)
library(sf)
library(broom)

#import data ----------------------
col_ped_zat <- readRDS("../../clean_data/collision/collision_zat_df.rds")
profile <- readRDS("../../clean_data/aggr_hclust_geo/calle2zat_geo.rds") %>% 
  st_drop_geometry()

profile %>% filter(is.na(clus))

# Profile distribution --------------------------------
distr_stat <- function(data, unit, group){
  data %>% 
    dplyr::select({{unit}},{{group}}) %>% 
    count({{group}}) %>% 
    ungroup() %>% 
    mutate(total = nrow(data), #as long as no NA rows
      percent = (n/total)*100)
}

profile_distr <- distr_stat(profile, ZAT, clus) %>% 
  mutate(clus = factor(clus)) #match var type

# collision ~ profile ----------------------------------
profile_ped_zat <- col_ped_zat %>% 
  left_join(profile, by = "ZAT") %>% 
  mutate(clus = factor(clus)) %>% 
  drop_na(clus)

## pedestrian collision with injury --------------

fit_injury <- glm.nb(injury ~ clus, data = profile_ped_zat)
summary(fit_injury)
injury_df <- tidy(fit_injury) %>% 
  mutate(outcome = 'injury')

## pedestrian collision with death --------------
fit_death <- glm.nb(death ~ clus, data = profile_ped_zat)
summary(fit_death)
death_df <- tidy(fit_death) %>% 
  mutate(outcome='death')

## total pedestrian collision --------------
fit_total <- glm.nb(total ~ clus, data = profile_ped_zat)
summary(fit_total)

total_df <- tidy(fit_total) %>% 
  mutate(outcome='total') %>% 
  bind_rows(injury_df, death_df) %>% 
  mutate(clus = case_match(
    term,
    "(Intercept)" ~ "1",
    "clus2" ~ "2",
    "clus3" ~ "3",
    "clus4" ~ "4"
  )) %>% 
  left_join(profile_distr, by = "clus") %>% 
  dplyr::select(clus:percent, estimate:outcome) %>% 
  rename(nb_profile = clus,
    percent_zat = percent)

write_csv(total_df, file = "collision-profile_zat.csv")
