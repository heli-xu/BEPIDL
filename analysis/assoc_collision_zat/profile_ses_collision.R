library(tidyverse)
library(sf)
library(broom)
library(MASS)
library(readxl)

# 0. import data ----------------------
col_ped_zat <- readRDS("../../clean_data/collision/collision_zat_df.rds")

ses_zat <- readRDS("../../clean_data/ses/ses_zat.rds")
ses_zat %>% filter(is.na(ses_cat))

# covar
pop <- read_xlsx("../../data/pop_zat.xlsx")
zat <- read_xlsx("../../data/ZAT/ZAT_INDICADORES.xlsx")

pop_density <- pop %>% 
  left_join(zat %>% 
              dplyr::select(ZAT, area_m2 = `Area M2`), 
            by = "ZAT") %>% 
  mutate(pop_density = (POBD2021 / area_m2)*100) %>% 
  dplyr::select(ZAT, pop_density)
  
# offset
traffic <- readRDS("../../data/zat_denom.rds")

traffic <- traffic %>% 
  mutate(walk_pubt = total_walk + total_pubt)

# 1. SES distribution ---------------------
#ses_zat %>% filter(is.na(ses_cat))
source("../../functions/distr_stat.R")

ses_zat_distri <- distr_stat(ses_zat, ZAT, ses_cat)


# 2. Collision ~ SES --------------------------
ses_ped_zat <- col_ped_zat %>% 
  left_join(ses_zat %>% 
              dplyr::select(ZAT, ses_cat), by = "ZAT") %>% 
  drop_na(ses_cat) %>%  #after join check NA
  mutate(ses_cat_r = factor(ses_cat, levels = rev(levels(ses_cat)))) 

levels(ses_ped_zat$ses_cat_r)

ses_ped_zat %>% filter(is.na(ses_cat_r))
## injury --------
fit_injuryS <- glm.nb(injury ~ ses_cat_r, data = ses_ped_zat)
summary(fit_injuryS)
injuryS_df <- tidy(fit_injuryS, conf.int = TRUE, exponentiate = TRUE) %>% 
  mutate(outcome = "injury")

## death ------------
fit_deathS <- glm.nb(death ~ ses_cat_r, data = ses_ped_zat)
summary(fit_deathS)

deathS_df <- tidy(fit_deathS, conf.int = TRUE, exponentiate = TRUE) %>% 
  mutate(outcome = "death")

## total ------------------
fit_totalS <- glm.nb(total ~ ses_cat_r, data = ses_ped_zat)
summary(fit_totalS)

## summarise RR --------------
RR_ses <- tidy(fit_totalS, conf.int = TRUE, exponentiate = TRUE) %>% 
  mutate(outcome = "total") %>% 
  bind_rows(injuryS_df, deathS_df) %>%
  mutate(RR_95CI = paste0(round(estimate,2)," (", round(conf.low,2), ",", round(conf.high, 2), ")")) %>% 
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
  mutate(predictor = paste0("ses_", ses_cat)) 

saveRDS(RR_ses, file = "ses_col_RR.rds")

ses_collision_csv <- RR_ses %>% 
  dplyr::select(
    predictor, n, total, 
    percent_zat = percent, 
    RR_95CI, p.value, outcome)


write_csv(ses_collision_csv, file = "collision-ses_zat.csv")

## visualize ---------------
plot_RR(RR_ses, predictor) +
  facet_grid(vars(outcome), switch = "y")+
  labs(
    title = "Pedestrian Collision and Neighborhood SES Level",
    subtitle = "ZAT level, Bogota, Colombia",
    x = "RR (95%CI)",
    y = "SES Level",
    caption = "All comparisons are relative to the SES 6 (highest) level."
  )

# 3. Collision~SES+covar+offset--------
## 3.1 join col, ses, covar, offset-------
ses_ped_covar <- col_ped_zat %>% 
  left_join(ses_zat %>% 
              dplyr::select(ZAT, ses_cat), by = "ZAT") %>% 
  drop_na(ses_cat) %>%  #after join check NA
  mutate(ses_cat_r = factor(ses_cat, levels = rev(levels(ses_cat)))) %>% 
  left_join(pop_density, by = "ZAT") %>% 
  left_join(traffic %>% 
              dplyr::select(ZAT, walk_pubt),
            by = "ZAT") %>% 
  mutate(
    across(pop_density, ~scale(.x)[, 1])
  ) %>% 
  drop_na() %>% 
  filter(walk_pubt > 0) # cause the log()

# ses_ped_covar %>% filter(is.na(walk_pubt))

## 3.2 Model-------
### injury ------
fit_injury_covar <- glm.nb(injury ~ ses_cat_r+ offset(log(walk_pubt)) + pop_density, data = ses_ped_covar)
summary(fit_injury_covar)

injury_co_df <- tidy(fit_injury_covar, conf.int = TRUE, exponentiate = TRUE) %>%
  mutate(outcome = "injury")

### death -------
fit_death_covar <- glm.nb(death ~ ses_cat_r + offset(log(walk_pubt)) + pop_density, data = ses_ped_covar)
summary(fit_death_covar)

death_co_df <- tidy(fit_death_covar, conf.int = TRUE, exponentiate = TRUE) %>%
  mutate(outcome = "death")

### total ---------
fit_total_covar <- glm.nb(total ~ ses_cat_r + offset(log(walk_pubt)) + pop_density, data = ses_ped_covar)
summary(fit_total_covar)

total_co_df <- tidy(fit_total_covar, conf.int = TRUE, exponentiate = TRUE) %>%
  mutate(outcome = "total")

## 3.3 Summarize RR --------
ses_covar_RR <- bind_rows(injury_co_df, death_co_df, total_co_df) %>% 
  mutate(RR_95CI = paste0(round(estimate,2)," (", round(conf.low,2), ",", round(conf.high, 2), ")")) %>% 
  mutate(ses_cat = case_match(
    term,
    "(Intercept)" ~ "6",
    "ses_cat_r5" ~ "5",
    "ses_cat_r4" ~ "4",
    "ses_cat_r3" ~ "3",
    "ses_cat_r2" ~ "2",
    "ses_cat_r1" ~ "1",
    .default = "(Covariates)"
  )) %>% 
  left_join(ses_zat_distri, by = "ses_cat") %>%
  mutate(predictor = paste0("ses_", ses_cat)) 

saveRDS(ses_covar_RR, file = "ses_covar_col_RR.rds")

ses_covar_RR_csv <- ses_covar_RR %>% 
  dplyr::select(
    term, predictor, n, total, 
    percent_zat = percent, 
    RR_95CI, p.value, outcome)

write_csv(ses_covar_RR_csv, file = "ses_covar_col_RR.csv")

## 3.4 Visualize --------------
source("../../functions/plot_RR.R")
ses_covar_RR %>% 
  filter(!ses_cat == "(Covariates)") %>% 
  plot_RR(., predictor) +
  facet_grid(vars(outcome), switch = "y")+
  labs(
    title = "Pedestrian Collision and Neighborhood (ZAT) SES Level in Bogotá",
    subtitle = "Adjusted for population density, age groups, sex composition and types of dwellings.",
    x = "RR (95%CI)",
    y = "SES Level",
    caption = "All comparisons are relative to the SES 6 (highest) level."
  )
