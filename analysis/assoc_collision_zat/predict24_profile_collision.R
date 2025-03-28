library(tidyverse)
library(sf)
library(broom)
library(MASS)

# 0. import data ----------------------
col_ped_zat <- readRDS("../../clean_data/collision/collision_zat_df.rds")
# profile <- readRDS("../../clean_data/aggr_hclust_geo/calle2zat_geo.rds") %>% 
#   st_drop_geometry()

# predict24_profile with road network characteristics - 2015-2019 only
profile <- readRDS("../../clean_data/predict24/w_road_info/2015_19/pr_zat_cluster_rd.rds")

#updated profile using predict312k w road info - 2015-19 only
profile <- readRDS("../../clean_data/predict24/predict_312k/profile_w_road_info/pr312k_zat_cluster_rd.rds")

# if not in profile, road type area as covar
road_type <- readRDS("../../clean_data/road_type/rd_type_area_zat.rds")

#ses
ses_zat <- readRDS("../../clean_data/covar_mzn/ses_zat.rds")

#ipm
ipm <- readRDS("../../clean_data/covar_mzn/ipm_zat.rds")

#covar, offset
traffic <- readRDS("../../clean_data/ZAT/walk_pubt.rds")

pop_density <- readRDS("../../clean_data/ZAT/pop_density2021.rds")


# 1. distribution -------------------
source("../../functions/distr_stat.R")

profile_distr <- distr_stat(profile, ZAT, clus) %>% 
  mutate(clus = factor(clus, levels = c(3, 2, 1, 4)))

write_csv(profile_distr, file = "predict312k_profile/predict312k_1519_descrp.csv")

# primary analysis at 5.

# 2. Collision ~ profile ----------------------------------
## 2.1 join data -------------
profile_ped_zat <- col_ped_zat %>% 
  left_join(profile, by = "ZAT") %>% 
  mutate(clus = factor(clus, levels = c(3, 2, 1, 4))) %>% 
  drop_na(clus)

## 2.2 injury --------------

fit_injuryP <- glm.nb(injury ~ clus, data = profile_ped_zat)
summary(fit_injuryP)
injuryP_df <- tidy(fit_injuryP, conf.int = TRUE, exponentiate = TRUE) %>% 
  mutate(outcome = 'injury')

## 2.3 death --------------
fit_deathP <- glm.nb(death ~ clus, data = profile_ped_zat)
summary(fit_deathP)

deathP_df <- tidy(fit_deathP, conf.int = TRUE, exponentiate = TRUE) %>% 
  mutate(outcome='death')

## 2.4 total pedestrian collision --------------
fit_totalP <- glm.nb(total ~ clus, data = profile_ped_zat)
summary(fit_totalP)

## 2.5 summarise RR -----------------------------
RR_profile <- tidy(fit_totalP, conf.int = TRUE, exponentiate = TRUE) %>% 
  mutate(outcome='total') %>% 
  bind_rows(injuryP_df, deathP_df) %>% 
  mutate(RR_95CI = paste0(round(estimate,2)," (", round(conf.low,2), ",", round(conf.high, 2), ")")) %>% 
  mutate(clus = case_match(
    term,
    "(Intercept)" ~ "5",
    "clus1" ~ "1",
    "clus2" ~ "2",
    "clus3" ~ "3",
    "clus4" ~ "4"
  )) %>% 
  left_join(profile_distr, by = "clus") %>% 
  mutate(predictor = paste0("profile_", clus)) 

saveRDS(RR_profile, file = "predict24_profile/profile_rd_RR_ref5.rds")

## 2.6 visualize-----------
source("../../functions/plot_RR.R")
#RR_profile <- readRDS("predict24_profile/profile_rd_RR.rds")

plot_RR(RR_profile, predictor)+
  facet_grid(vars(outcome), switch = "y")+
  labs(
    title = "Pedestrian Collision and Neighborhood Profiles",
    subtitle = "Unadjusted, ZAT level, Bogotá, Colombia",
    x = "RR (95%CI)",
    y = "ZAT Profile",
    caption = "All comparisons are relative to the profile 1."
  )

# 3. Collision ~ profile + SES -------------------------------
## 3.1 join data-----------
ses_profile_ped <- ses_zat %>% 
  dplyr::select(ZAT, ses_cat) %>% 
  mutate(ses_cat_r = factor(ses_cat, levels = rev(levels(ses_cat)))) %>% 
  left_join(profile, by = "ZAT") %>% 
  mutate(clus = factor(clus, levels = c(5, 1, 2, 3, 4))) %>% 
  #drop_na(clus) %>% 
  left_join(col_ped_zat, by = "ZAT") %>% 
  drop_na()

## 3.2 Model -----------------
### injury ------------------
fit_injury2 <- glm.nb(injury ~ clus + ses_cat_r, data = ses_profile_ped)
summary(fit_injury2)

# ### compare with or w/o profile 
# BIC(fit_injury2, fit_injuryS)
# #about the same

# plot(residuals(fit_injury2))

injury_df <- tidy(fit_injury2, conf.int = TRUE, exponentiate = TRUE) %>% 
  #exponentiate->RR 
  mutate(outcome = "injury")

### death ---------------------------
fit_death2 <- glm.nb(death ~ clus + ses_cat_r, data = ses_profile_ped)
summary(fit_death2)

death_df <- tidy(fit_death2, conf.int = TRUE, exponentiate = TRUE) %>% 
  mutate(outcome = "death")

### total --------------------------------
fit_total2 <- glm.nb(total ~ clus + ses_cat_r, data = ses_profile_ped)
summary(fit_total2)

total_df <- tidy(fit_total2, conf.int = TRUE, exponentiate = TRUE) %>% 
  mutate(outcome = "total")

## 3.3 summarise RR -------------------
prof_ses_RR <- injury_df %>% 
  bind_rows(death_df, total_df) %>% 
  mutate(RR_95CI = paste0(round(estimate,2)," (", round(conf.low,2), ",", round(conf.high, 2), ")")) %>% 
  mutate(predictor = case_match(
    term,
    "(Intercept)" ~ "profile_5",
    "clus1" ~ "profile_1",
    "clus2" ~ "profile_2",
    "clus3" ~ "profile_3",
    "clus4" ~ "profile_4",
    # "ses_cat_r5" ~ "ses_5",
    # "ses_cat_r4" ~ "ses_4",
    # "ses_cat_r3" ~ "ses_3",
    # "ses_cat_r2" ~ "ses_2",
    # "ses_cat_r1" ~ "ses_1"
    .default = "(Covariates)"
  )) 

saveRDS(prof_ses_RR, file = "predict24_profile/profile_rd_ses_RR_ref5.rds")

ses_prof_col_csv <- prof_ses_RR %>% 
  dplyr::select(
    predictor,
    RR_95CI,
    p.value,
    outcome
  )

write_csv(ses_prof_col_csv, file = "profile_w_rd_type/ses_profile_area_zat.csv" )
write_csv(ses_prof_col_csv, file = "profile_w_rd_type/ses_profile_count_zat.csv" )

## 3.4 visualize ----------
##SES as covariates, adjusted for

prof_ses_RR %>% 
  filter(!predictor == "(Covariates)") %>% 
  plot_RR(., predictor)+
  facet_grid(vars(outcome), switch = "y")+
  labs(
    title = "Pedestrian Collision and Neighborhood (ZAT) Profiles in Bogotá",
    subtitle = "Adjusted for ZAT-level SES",
    x = "RR (95%CI)",
    y = "ZAT Profile",
    caption = "All comparisons are relative to the profile 1."
  )+
  theme(
    plot.title = element_text(size = 12),
    plot.title.position = "plot"
  )

# 4. Collision~profile+SES+offset+covar------
## 4.1 join data------------
profile_ses_covar <- ses_zat %>% 
  dplyr::select(ZAT, ses_cat) %>% 
  mutate(ses_cat_r = factor(ses_cat, levels = rev(levels(ses_cat)))) %>% 
  left_join(profile, by = "ZAT") %>% 
  mutate(clus = factor(clus, levels = c(3,1, 2, 4))) %>% 
  #drop_na(clus) %>% 
  left_join(col_ped_zat, by = "ZAT") %>% 
  left_join(traffic %>% 
      dplyr::select(ZAT, walk_pubt), 
    by = "ZAT") %>% 
  left_join(pop_density, by = "ZAT") %>% 
  mutate(
    across(pop_density, ~scale(.x)[, 1])
  ) %>% 
  drop_na() %>% 
  filter(walk_pubt > 0)


## 4.2 Model ------------
### injury ----------
injury_co2 <- glm.nb(injury ~ clus + ses_cat_r + offset(log(walk_pubt)) + pop_density, data = profile_ses_covar)
summary(injury_co2)

injury_co2_df <-  tidy(injury_co2, conf.int = TRUE, exponentiate = TRUE) %>%
  mutate(outcome = "injury")

### death -------
death_co2 <- glm.nb(death ~ clus + ses_cat_r + offset(log(walk_pubt)) + pop_density, data = profile_ses_covar)

death_co2_df <- tidy(death_co2, conf.int = TRUE, exponentiate = TRUE) %>%
  mutate(outcome = "death")

### total -------
total_co2 <- glm.nb(total ~ clus + ses_cat_r + offset(log(walk_pubt)) + pop_density, data = profile_ses_covar)

total_co2_df <- tidy(total_co2, conf.int = TRUE, exponentiate = TRUE) %>% 
  mutate(outcome = "total")

## 4.3 summarise RR------------
prof_ses_covar_RR <- bind_rows(injury_co2_df, death_co2_df, total_co2_df) %>% 
  mutate(RR_95CI = paste0(round(estimate,2)," (", round(conf.low,2), ",", round(conf.high, 2), ")")) %>%
  mutate(predictor = case_match(
    term,
    "(Intercept)" ~ "profile_4",
    "clus1" ~ "profile_1",
    "clus2" ~ "profile_2",
    "clus3" ~ "profile_3",
    "clus5" ~ "profile_5",
    # "ses_cat_r5" ~ "ses_5",
    # "ses_cat_r4" ~ "ses_4",
    # "ses_cat_r3" ~ "ses_3",
    # "ses_cat_r2" ~ "ses_2",
    # "ses_cat_r1" ~ "ses_1",
    .default = "(Covariates)"
  )) 

saveRDS(prof_ses_covar_RR, file = "predict24_profile/profile_rd_covar_RR.rds")

prof_ses_covar_RR_csv <- prof_ses_covar_RR %>% 
  dplyr::select(
    term, predictor,
    RR_95CI,
    p.value,
    outcome
  )

## 4.4 visualize --------
prof_ses_covar_RR <- readRDS("predict24_profile/profile_rd_covar_RR.rds")
source("../../functions/plot_RR.R")

prof_ses_covar_RR %>% 
  filter(!predictor == "(Covariates)") %>% 
  plot_RR(., predictor)+
  facet_grid(vars(outcome), switch = "y")+
  labs(
    title = "Pedestrian Collision and Neighborhood (ZAT) Profiles in Bogotá",
    subtitle = "Adjusted for ZAT-level SES level, walking/public transit trips and population density",
    x = "RR (95%CI)",
    y = "ZAT Profile",
    caption = "All comparisons are relative to the profile 1."
  )+
  theme(
    plot.title.position = "plot"
  )

# *5. Collision~everything+rd_type--------
## 5.1 Join data ---------------
rd_type_zat <- road_type %>% 
  dplyr::select(-c(Collector:total, pcta_Arterial, pcta_Rural, pcta_Pedestrian, pcta_Unknown, pcta_Projected))  
#leave out the *reference group* to avoid colinearity --tried local
#arterial

### using SES-----
profile_covar_rd <- ses_zat %>% 
  dplyr::select(ZAT, ses_cat) %>% 
  mutate(ses_cat_r = factor(ses_cat, levels = rev(levels(ses_cat)))) %>% 
  left_join(profile, by = "ZAT") %>% 
  mutate(clus = factor(clus, levels = c(4,1,2,3))) %>%  #note the ref level
  #drop_na(clus) %>% 
  left_join(col_ped_zat, by = "ZAT") %>% 
  left_join(traffic %>% 
      dplyr::select(ZAT, walk_pubt), 
    by = "ZAT") %>% 
  left_join(pop_density, by = "ZAT") %>% 
  left_join(rd_type_zat, by = "ZAT") %>%  
  drop_na() %>% 
  mutate(
    across(c(pop_density, starts_with("pcta_")), ~scale(.x)[, 1])
  ) %>% 
  filter(walk_pubt > 0)

### using IPM---------
profile_ipm_covar <- profile %>% 
  left_join(ipm, by = "ZAT") %>% 
  mutate(clus = factor(clus, levels = c(4,1,2,3))) %>% #note the ref level
  #drop_na(clus) %>% 
  left_join(col_ped_zat, by = "ZAT") %>% 
  left_join(traffic %>% 
      dplyr::select(ZAT, walk_pubt), 
    by = "ZAT") %>% 
  left_join(pop_density, by = "ZAT") %>% 
  left_join(rd_type_zat, by = "ZAT") %>%  
  drop_na() %>% 
  mutate(
    across(c(pop_density, starts_with("pcta_")), ~scale(.x)[, 1])
  ) %>% 
  filter(walk_pubt > 0)

### full table with both----
profile_all_covar <- ses_zat %>% 
  dplyr::select(ZAT, ses_cat) %>% 
  mutate(ses_cat_r = factor(ses_cat, levels = rev(levels(ses_cat)))) %>% 
  left_join(profile, by = "ZAT") %>% 
  mutate(clus = factor(clus, levels = c(2,1,3,4))) %>% 
  #drop_na(clus) %>% 
  left_join(col_ped_zat, by = "ZAT") %>% 
  left_join(traffic %>% 
      dplyr::select(ZAT, walk_pubt), 
    by = "ZAT") %>% 
  left_join(pop_density, by = "ZAT") %>% 
  left_join(rd_type_zat, by = "ZAT") %>%  
  drop_na() %>% 
  filter(walk_pubt > 0) %>% 
  left_join(ipm, by = "ZAT")

write_csv(profile_all_covar, file = "predict312k_profile/profile312_all_covar_unscaled.csv")

## 5.2 Model ------------
### injury ----------
### SES
injury_co3 <- glm.nb(injury ~ clus + ses_cat_r + offset(log(walk_pubt)) + pop_density + pcta_Collector + pcta_Local + pcta_other, data = profile_covar_rd)
### IPM
injury_co3 <- glm.nb(injury ~ clus + MEANIPM + offset(log(walk_pubt)) + pop_density + pcta_Collector + pcta_Local + pcta_other, data = profile_ipm_covar)

summary(injury_co3)

car::vif(injury_co3)
#pcta_Arterial moderate multicollinearity by adjusted GVIF (1-2.5 acceptable)
#so set arterial as reference instead
# GVIF^(1/(2*Df)) < 2: Indicates low multicollinearity, which is generally acceptable.

injury_co3_df <-  tidy(injury_co3, conf.int = TRUE, exponentiate = TRUE) %>%
  mutate(outcome = "injury")

### death -------
##SES
death_co3 <- glm.nb(death ~ clus + ses_cat_r + offset(log(walk_pubt)) + pop_density + pcta_Collector + pcta_Local + pcta_other, data = profile_covar_rd)
summary(death_co3)

##IPM
death_co3 <- glm.nb(death ~ clus + MEANIPM + offset(log(walk_pubt)) + pop_density + pcta_Collector + pcta_Local + pcta_other, data = profile_ipm_covar)


death_co3_df <- tidy(death_co3, conf.int = TRUE, exponentiate = TRUE) %>%
  mutate(outcome = "death")

### total -------
##SES
total_co3 <- glm.nb(total ~ clus + offset(log(walk_pubt)) + ses_cat_r + pop_density + pcta_Collector + pcta_Local + pcta_other, data = profile_covar_rd)

##IPM
total_co3 <- glm.nb(total ~ clus + MEANIPM + offset(log(walk_pubt)) + pop_density + pcta_Collector + pcta_Local + pcta_other, data = profile_ipm_covar)

total_co3_df <- tidy(total_co3, conf.int = TRUE, exponentiate = TRUE) %>% 
  mutate(outcome = "total")

## 5.3 summarise RR------------
prof_covar_rd_RR <- bind_rows(injury_co3_df, death_co3_df, total_co3_df) %>% 
  mutate(
    across(where(is.numeric), ~round(., 4)),
    RR_95CI = paste0(round(estimate,2)," (", round(conf.low,2), ",", round(conf.high, 2), ")")) %>%
  mutate(predictor = case_match(
    term,
    ## note to change below based on ref!
    "(Intercept)" ~ "profile_4",
    "clus1" ~ "profile_1",
    "clus2" ~ "profile_2",
    "clus3" ~ "profile_3",
    .default = "(Covariates)"
  ),
    .before = "term") 

saveRDS(prof_covar_rd_RR, file = "predict312k_profile/profile_ses_RR_ref3.rds")
saveRDS(prof_covar_rd_RR, file = "predict312k_profile/profile_ses_RR_ref2.rds")
saveRDS(prof_covar_rd_RR, file = "predict312k_profile/profile_ses_RR_ref4.rds")

saveRDS(prof_covar_rd_RR, file = "predict312k_profile/profile_ipm_RR_ref3.rds")
saveRDS(prof_covar_rd_RR, file = "predict312k_profile/profile_ipm_RR_ref2.rds")
saveRDS(prof_covar_rd_RR, file = "predict312k_profile/profile_ipm_RR_ref4.rds")

prof_covar_RR_csv <- prof_covar_rd_RR %>% 
  dplyr::select(
    predictor,
    term,
    outcome,
    RR_95CI,
    everything()
  ) %>% 
  rename(z_value = statistic)

write_csv(prof_covar_RR_csv, file = "predict312k_profile/zat_profile_predict1519_ses_ref3_RR.csv")
write_csv(prof_covar_RR_csv, file = "predict312k_profile/zat_profile_predict1519_ses_ref2_RR.csv")
write_csv(prof_covar_RR_csv, file = "predict312k_profile/zat_profile_predict1519_ses_ref4_RR.csv")

write_csv(prof_covar_RR_csv, file = "predict312k_profile/zat_profile_predict1519_ipm_ref3_RR.csv")
write_csv(prof_covar_RR_csv, file = "predict312k_profile/zat_profile_predict1519_ipm_ref2_RR.csv")
write_csv(prof_covar_RR_csv, file = "predict312k_profile/zat_profile_predict1519_ipm_ref4_RR.csv")


## 5.4 visualize --------

source("../../functions/plot_RR.R")
### ref=3 -------
ses_RR <- readRDS("predict312k_profile/profile_ses_RR_ref3.rds")
ipm_RR <- readRDS("predict312k_profile/profile_ipm_RR_ref3.rds")

ses_RR %>% 
  filter(!predictor == "(Covariates)") %>% 
  plot_RR(., predictor)+
  facet_grid(vars(outcome), switch = "y")+
  labs(
    title = "Pedestrian Collision and Neighborhood (ZAT) Profiles in Bogotá",
    subtitle = "Adjusted for road types, ZAT-level walking/public transit trips, SES, and \npopulation density",
    x = "RR (95%CI)",
    y = "ZAT Profile",
    caption = "All comparisons are relative to the profile 3."
  )+
  theme(
    plot.title = element_text(size = 12),
    plot.title.position = "plot"
  )

ipm_RR %>% 
  filter(!predictor == "(Covariates)") %>% 
  plot_RR(., predictor)+
  facet_grid(vars(outcome), switch = "y")+
  labs(
    title = "Pedestrian Collision and Neighborhood (ZAT) Profiles in Bogotá",
    subtitle = "Adjusted for road types, ZAT-level walking/public transit trips, SES (IPM), \nand population density",
    x = "RR (95%CI)",
    y = "ZAT Profile",
    caption = "All comparisons are relative to the profile 3."
  )+
  theme(
    plot.title = element_text(size = 12),
    plot.title.position = "plot"
  )

### ref =2-------
ses_RR2 <- readRDS("predict312k_profile/profile_ses_RR_ref2.rds")
ipm_RR2 <- readRDS("predict312k_profile/profile_ipm_RR_ref2.rds")

ses_RR2 %>% 
  filter(!predictor == "(Covariates)") %>% 
  plot_RR(., predictor)+
  facet_grid(vars(outcome), switch = "y")+
  labs(
    title = "Pedestrian Collision and Neighborhood (ZAT) Profiles in Bogotá",
    subtitle = "Adjusted for road types, ZAT-level walking/public transit trips, SES, and \npopulation density",
    x = "RR (95%CI)",
    y = "ZAT Profile",
    caption = "All comparisons are relative to the profile 2."
  )+
  theme(
    plot.title = element_text(size = 12),
    plot.title.position = "plot"
  )

ipm_RR2 %>% 
  filter(!predictor == "(Covariates)") %>% 
  plot_RR(., predictor)+
  facet_grid(vars(outcome), switch = "y")+
  labs(
    title = "Pedestrian Collision and Neighborhood (ZAT) Profiles in Bogotá",
    subtitle = "Adjusted for road types, ZAT-level walking/public transit trips, SES (IPM), \nand population density",
    x = "RR (95%CI)",
    y = "ZAT Profile",
    caption = "All comparisons are relative to the profile 2."
  )+
  theme(
    plot.title = element_text(size = 12),
    plot.title.position = "plot"
  )

### ref =4 -----
ses_RR4 <- readRDS("predict312k_profile/profile_ses_RR_ref4.rds")
ipm_RR4 <- readRDS("predict312k_profile/profile_ipm_RR_ref4.rds")

ses_RR4 %>% 
  filter(!predictor == "(Covariates)") %>% 
  mutate(predictor = case_match(
    predictor,
    "profile_1" ~ "Profile 1",
    "profile_2" ~ "Profile 2",
    "profile_3" ~ "Profile 3",
    .default = predictor
  )) %>% 
  plot_RR(., predictor)+
  facet_grid(vars(outcome), switch = "y")+
  labs(
    title = "Pedestrian Collision and Neighborhood (ZAT) Profiles in Bogotá",
    subtitle = "Adjusted for road types, ZAT-level walking/public transit trips, SES, and \npopulation density",
    x = "RR (95%CI)",
    y = "ZAT Profile",
    caption = "All comparisons are relative to the profile 4."
  )+
  theme(
    plot.title = element_text(size = 12),
    plot.title.position = "plot"
  )


ipm_RR4 %>% 
  filter(!predictor == "(Covariates)") %>% 
  mutate(predictor = case_match(
    predictor,
    "profile_1" ~ "Profile 1",
    "profile_2" ~ "Profile 2",
    "profile_3" ~ "Profile 3",
    .default = predictor
  )) %>% 
  plot_RR(., predictor)+
  facet_grid(vars(outcome), switch = "y")+
  labs(
    title = "Pedestrian Collision and Neighborhood (ZAT) Profiles in Bogotá",
    subtitle = "Adjusted for road types, ZAT-level walking/public transit trips, SES (IPM), \nand population density",
    x = "RR (95%CI)",
    y = "ZAT Profile",
    caption = "All comparisons are relative to the profile 4."
  )+
  theme(
    plot.title = element_text(size = 12),
    plot.title.position = "plot"
  )
