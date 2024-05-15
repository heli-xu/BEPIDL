library(tidyverse)
library(sf)
library(broom)
library(MASS)

# 0. import data ----------------------
col_ped_zat <- readRDS("../../clean_data/collision/collision_zat_df.rds")
profile <- readRDS("../../clean_data/aggr_hclust_geo/calle2zat_geo.rds") %>% 
  st_drop_geometry()

profile %>% filter(is.na(clus))

ses_zat <- readRDS("../../clean_data/ses/ses_zat.rds")

# 1. Profile distribution --------------------------------
distr_stat <- function(data, unit, group){
  data %>% 
    dplyr::select({{unit}},{{group}}) %>% 
    count({{group}}) %>% 
    ungroup() %>% 
    mutate(total = nrow(data), #MUST check no NA rows
      percent = (n/total)*100)
}

profile_distr <- distr_stat(profile, ZAT, clus) %>% 
  mutate(clus = factor(clus)) #match var type

# 2. SES distribution ---------------------
#ses_zat %>% filter(is.na(ses_cat))

ses_zat_distri <- distr_stat(ses_zat, ZAT, ses_cat)

# 3. Collision ~ profile ----------------------------------
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

## save csv ---------------------
write_csv(total_df, file = "collision-profile_zat.csv")

# 4. Collision ~ SES --------------------------
ses_ped_zat <- col_ped_zat %>% 
  left_join(ses_zat %>% 
              dplyr::select(ZAT, ses_cat), by = "ZAT") %>% 
  mutate(ses_cat_r = factor(ses_cat, levels = rev(levels(ses_cat)))) 

levels(ses_ped_zat$ses_cat_r)

## injury --------
fit_injuryS <- glm.nb(injury ~ ses_cat_r, data = ses_ped_zat)
summary(fit_injuryS)
injuryS_df <- tidy(fit_injuryS) %>% 
  mutate(outcome = "injury")

## death ------------
fit_deathS <- glm.nb(death ~ ses_cat_r, data = ses_ped_zat)
summary(fit_deathS)
deathS_df <- tidy(fit_deathS) %>% 
  mutate(outcome = "death")

## total ------------------
fit_totalS <- glm.nb(total ~ ses_cat_r, data = ses_ped_zat)
summary(fit_totalS)

## save csv --------------
total_df <- tidy(fit_totalS) %>% 
  mutate(outcome = "total") %>% 
  bind_rows(injuryS_df, deathS_df) %>% 
  mutate(ses_cat = case_match(
    term,
    "(Intercept)" ~ "6",
    "ses_cat_r5" ~ "5",
    "ses_cat_r4" ~ "4",
    "ses_cat_r3" ~ "3",
    "ses_cat_r2" ~ "2",
    "ses_cat_r1" ~ "1"
  )) %>% 
  left_join(ses_zat_distri, by = "ses_cat") %>% 
  dplyr::select(ses_cat:percent, estimate:outcome) %>% 
  rename(ses_level = ses_cat,
         percent_zat = percent)

write_csv(total_df, file = "collision-ses_zat.csv")
