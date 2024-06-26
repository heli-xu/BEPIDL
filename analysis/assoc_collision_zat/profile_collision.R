library(tidyverse)
library(sf)
library(broom)
library(MASS)

# 0. import data ----------------------
col_ped_zat <- readRDS("../../clean_data/collision/collision_zat_df.rds")
# profile <- readRDS("../../clean_data/aggr_hclust_geo/calle2zat_geo.rds") %>% 
#   st_drop_geometry()

# profile with road type count
profile <- readRDS("../../clean_data/ZAT/zat_cluster_w_rd_type_count.rds")

profile %>% filter(is.na(clus))

# profile with road type area
profile <- readRDS("../../clean_data/ZAT/zat_cluster_w_rd_type_area.rds")

# profile without road type
## a = 0.25
profile <- readRDS("../../clean_data/ZAT/zat_cluster_wo_rd_type_a25.rds")

## *a =0.35*
profile <- readRDS("../../clean_data/ZAT/zat_cluster_wo_rd_type_a35.rds")

# if not in profile, road type area as covar
road_type <- readRDS("../../clean_data/road_type/rd_type_area_zat.rds")

#ses
ses_zat <- readRDS("../../clean_data/ses/ses_zat.rds")

#covar, offset
traffic <- readRDS("../../clean_data/ZAT/walk_pubt.rds")

pop_density <- readRDS("../../clean_data/ZAT/pop_density2021.rds")

# 1. Profile distribution --------------------------------
source("../../functions/distr_stat.R")

profile_distr <- distr_stat(profile, ZAT, clus) %>% 
  mutate(clus = factor(clus, levels = c(4, 1, 2, 3))) #match var type

# 2. Collision ~ profile ----------------------------------
## join data -------------
profile_ped_zat <- col_ped_zat %>% 
  left_join(profile, by = "ZAT") %>% 
  mutate(clus = factor(clus, levels = c(4, 1, 2, 3))) %>% 
  drop_na(clus)

## injury --------------

fit_injuryP <- glm.nb(injury ~ clus, data = profile_ped_zat)
summary(fit_injuryP)
injuryP_df <- tidy(fit_injuryP, conf.int = TRUE, exponentiate = TRUE) %>% 
  mutate(outcome = 'injury')

## death --------------
fit_deathP <- glm.nb(death ~ clus, data = profile_ped_zat)
summary(fit_deathP)

deathP_df <- tidy(fit_deathP, conf.int = TRUE, exponentiate = TRUE) %>% 
  mutate(outcome='death')

## total pedestrian collision --------------
fit_totalP <- glm.nb(total ~ clus, data = profile_ped_zat)
summary(fit_totalP)

## summarise RR -----------------------------
RR_profile <- tidy(fit_totalP, conf.int = TRUE, exponentiate = TRUE) %>% 
  mutate(outcome='total') %>% 
  bind_rows(injuryP_df, deathP_df) %>% 
  mutate(RR_95CI = paste0(round(estimate,2)," (", round(conf.low,2), ",", round(conf.high, 2), ")")) %>% 
  mutate(clus = case_match(
    term,
    "(Intercept)" ~ "4",
    "clus1" ~ "1",
    "clus2" ~ "2",
    "clus3" ~ "3"
  )) %>% 
  left_join(profile_distr, by = "clus") %>% 
  mutate(predictor = paste0("profile_", clus)) 

#saveRDS(RR_profile, file = "profile_wo_rd_type/profile_a25_RR.rds")
#saveRDS(RR_profile, file = "profile_wo_rd_type/profile_a35_RR.rds")
#saveRDS(RR_profile, file = "profile_w_rd_type/profile_count_RR.rds")
#saveRDS(RR_profile, file = "profile_w_rd_type/profile_area_RR.rds")
saveRDS(RR_profile, file = "profile_wo_rd_type/profile_a35_RR_ref4.rds")

profile_collision_csv <- RR_profile %>% 
  dplyr::select(
    predictor, n, total, 
    percent_zat = percent, 
    RR_95CI, p.value, outcome)


#write_csv(profile_collision_csv, file = "profile_wo_rd_type/collision-profile_zat.csv")
#write_csv(profile_collision_csv, file = "profile_wo_rd_type/collision_profile_a35_zat.csv")
#write_csv(profile_collision_csv, file = "profile_w_rd_type/collision_profile_count_zat.csv")
write_csv(profile_collision_csv, file = "profile_w_rd_type/collision_profile_area_zat.csv")

## visualize-----------
source("../../functions/plot_RR.R")
RR_profile <- readRDS("profile_col_RR.rds")

plot_RR(RR_profile, predictor)+
  facet_grid(vars(outcome), switch = "y")+
  labs(
    title = "Pedestrian Collision and Neighborhood Profiles",
    subtitle = "Unadjusted, ZAT level, Bogotá, Colombia",
    x = "RR (95%CI)",
    y = "ZAT Profile",
    caption = "All comparisons are relative to the profile 4."
  )


#FYI: ses ~ profile -----------------

#not continuous, can't do corr
# ses_profile <- ses_zat %>% 
#   dplyr::select(ZAT, ses_cat) %>% 
#   left_join(profile, by = "ZAT") %>% 
#   mutate(across(-ZAT, ~as.numeric(.), .names = "{.col}")) %>% 
#   drop_na()
# 
# cor(ses_profile %>% dplyr::select(-ZAT))

# library(corrplot) for plot
# corrplot(mx, method = "number") 

ses_profile2 <- ses_zat %>% 
  dplyr::select(ZAT, ses_cat) %>% 
  mutate(ses_cat_r = factor(ses_cat, levels = rev(levels(ses_cat)))) %>% 
  left_join(profile, by = "ZAT") %>% 
  mutate(clus = factor(clus)) %>% 
  drop_na()

fit_ses_p <- nnet::multinom(clus ~ ses_cat_r, data = ses_profile2)
summary(fit_ses_p)

df <- tidy(fit_ses_p)

# 3. Collision ~ profile + SES -------------------------------
## join data-----------
ses_profile_ped <- ses_zat %>% 
  dplyr::select(ZAT, ses_cat) %>% 
  mutate(ses_cat_r = factor(ses_cat, levels = rev(levels(ses_cat)))) %>% 
  left_join(profile, by = "ZAT") %>% 
  mutate(clus = factor(clus, levels = c(4, 1, 2, 3))) %>% 
  #drop_na(clus) %>% 
  left_join(col_ped_zat, by = "ZAT") %>% 
  drop_na()



## injury ------------------
fit_injury2 <- glm.nb(injury ~ clus + ses_cat_r, data = ses_profile_ped)
summary(fit_injury2)

# ### compare with or w/o profile 
# BIC(fit_injury2, fit_injuryS)
# #about the same

# plot(residuals(fit_injury2))

injury_df <- tidy(fit_injury2, conf.int = TRUE, exponentiate = TRUE) %>% 
  #exponentiate->RR 
  mutate(outcome = "injury")

## death ---------------------------
fit_death2 <- glm.nb(death ~ clus + ses_cat_r, data = ses_profile_ped)
summary(fit_death2)

death_df <- tidy(fit_death2, conf.int = TRUE, exponentiate = TRUE) %>% 
  mutate(outcome = "death")

## total --------------------------------
fit_total2 <- glm.nb(total ~ clus + ses_cat_r, data = ses_profile_ped)
summary(fit_total2)

total_df <- tidy(fit_total2, conf.int = TRUE, exponentiate = TRUE) %>% 
  mutate(outcome = "total")

## summarise RR -------------------
ses_prof_RR <- injury_df %>% 
  bind_rows(death_df, total_df) %>% 
  mutate(RR_95CI = paste0(round(estimate,2)," (", round(conf.low,2), ",", round(conf.high, 2), ")")) %>% 
  mutate(predictor = case_match(
    term,
    "(Intercept)" ~ "profile_4",
    "clus1" ~ "profile_1",
    "clus2" ~ "profile_2",
    "clus3" ~ "profile_3",
    # "ses_cat_r5" ~ "ses_5",
    # "ses_cat_r4" ~ "ses_4",
    # "ses_cat_r3" ~ "ses_3",
    # "ses_cat_r2" ~ "ses_2",
    # "ses_cat_r1" ~ "ses_1"
    .default = "(Covariates)"
  )) 

#saveRDS(ses_prof_RR, file = "profile_w_rd_type/profile_area_ses_RR.rds")
#saveRDS(ses_prof_RR, file = "profile_w_rd_type/profile_count_ses_RR.rds")
saveRDS(ses_prof_RR, file = "profile_wo_rd_type/profile_a35_ses_RR_ref4.rds")

ses_prof_col_csv <- ses_prof_RR %>% 
  dplyr::select(
    predictor,
    RR_95CI,
    p.value,
    outcome
  )

write_csv(ses_prof_col_csv, file = "profile_w_rd_type/ses_profile_area_zat.csv" )
write_csv(ses_prof_col_csv, file = "profile_w_rd_type/ses_profile_count_zat.csv" )

## visualize ----------
##SES as covariates, adjusted for
ses_profile_col_RR <- readRDS("ses_profile_col_RR.rds")

ses_prof_RR %>% 
  filter(!predictor == "(Covariates)") %>% 
  plot_RR(., predictor)+
  facet_grid(vars(outcome), switch = "y")+
  labs(
    title = "Pedestrian Collision and Neighborhood (ZAT) Profiles in Bogotá",
    subtitle = "Adjusted for ZAT-level SES level",
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
ses_profile_covar <- ses_zat %>% 
  dplyr::select(ZAT, ses_cat) %>% 
  mutate(ses_cat_r = factor(ses_cat, levels = rev(levels(ses_cat)))) %>% 
  left_join(profile, by = "ZAT") %>% 
  mutate(clus = factor(clus, levels = c(4, 1, 2, 3))) %>% 
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
injury_co2 <- glm.nb(injury ~ clus + ses_cat_r + offset(log(walk_pubt)) + pop_density, data = ses_profile_covar)
summary(injury_co2)

injury_co2_df <-  tidy(injury_co2, conf.int = TRUE, exponentiate = TRUE) %>%
  mutate(outcome = "injury")

### death -------
death_co2 <- glm.nb(death ~ clus + ses_cat_r + offset(log(walk_pubt)) + pop_density, data = ses_profile_covar)

death_co2_df <- tidy(death_co2, conf.int = TRUE, exponentiate = TRUE) %>%
  mutate(outcome = "death")

### total -------
total_co2 <- glm.nb(total ~ clus + ses_cat_r + offset(log(walk_pubt)) + pop_density, data = ses_profile_covar)

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
    # "ses_cat_r5" ~ "ses_5",
    # "ses_cat_r4" ~ "ses_4",
    # "ses_cat_r3" ~ "ses_3",
    # "ses_cat_r2" ~ "ses_2",
    # "ses_cat_r1" ~ "ses_1",
    .default = "(Covariates)"
  )) 

#saveRDS(prof_ses_covar_RR, file = "profile_wo_rd_type/prof_a25_ses_covar_RR.rds")
#saveRDS(prof_ses_covar_RR, file = "profile_wo_rd_type/prof_a35_ses_covar_RR.rds")
#saveRDS(prof_ses_covar_RR, file = "profile_w_rd_type/prof_count_ses_covar_RR.rds")
#saveRDS(prof_ses_covar_RR, file = "profile_w_rd_type/prof_area_ses_covar_RR.rds")
saveRDS(prof_ses_covar_RR, file = "profile_wo_rd_type/profile_a35_covar_RR_ref4.rds")

prof_ses_covar_RR_csv <- prof_ses_covar_RR %>% 
  dplyr::select(
    term, predictor,
    RR_95CI,
    p.value,
    outcome
  )

#write_csv(prof_ses_covar_RR_csv, file = "profile_wo_rd_type/prof_a25_ses_cov_RR.csv")
#write_csv(prof_ses_covar_RR_csv, file = "profile_wo_rd_type/prof_a35_ses_cov_RR.csv")
#write_csv(prof_ses_covar_RR_csv, file = "profile_w_rd_type/prof_count_ses_cov_RR.csv")
write_csv(prof_ses_covar_RR_csv, file = "profile_w_rd_type/prof_area_ses_cov_RR.csv")

## 4.4 visualize --------
prof_ses_covar_RR <- readRDS("prof_ses_covar_col_RR.rds")
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

# 5. Collision~everything+rd_type--------
# for profiles wo road types...
## 5.1 Join data ---------------
rd_type_zat <- road_type %>% 
  dplyr::select(-c(Collector:total, pcta_Arterial, pcta_Rural, pcta_Pedestrian, pcta_Unknown, pcta_Projected))  
#leave out the *reference group* to avoid colinearity --tried local
#arterial

ses_profile_covar_rd <- ses_zat %>% 
  dplyr::select(ZAT, ses_cat) %>% 
  mutate(ses_cat_r = factor(ses_cat, levels = rev(levels(ses_cat)))) %>% 
  left_join(profile, by = "ZAT") %>% 
  mutate(clus = factor(clus, levels = c(4, 1, 2, 3))) %>% 
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

## 5.2 Model ------------
### injury ----------
injury_co3 <- glm.nb(injury ~ clus + ses_cat_r + offset(log(walk_pubt)) + pop_density + pcta_Collector + pcta_Local + pcta_other, data = ses_profile_covar_rd)
summary(injury_co3)

car::vif(injury_co3)
#pcta_Arterial moderate multicollinearity by adjusted GVIF (1-2.5 acceptable)
#so set arterial as reference instead

injury_co3_df <-  tidy(injury_co3, conf.int = TRUE, exponentiate = TRUE) %>%
  mutate(outcome = "injury")

### death -------
death_co3 <- glm.nb(death ~ clus + ses_cat_r + offset(log(walk_pubt)) + pop_density + pcta_Collector + pcta_Local + pcta_other, data = ses_profile_covar_rd)
summary(death_co3)

death_co3_df <- tidy(death_co3, conf.int = TRUE, exponentiate = TRUE) %>%
  mutate(outcome = "death")

### total -------
total_co3 <- glm.nb(total ~ clus + ses_cat_r + offset(log(walk_pubt)) + pop_density + pcta_Collector + pcta_Local + pcta_other, data = ses_profile_covar_rd)

total_co3_df <- tidy(total_co3, conf.int = TRUE, exponentiate = TRUE) %>% 
  mutate(outcome = "total")

## 5.3 summarise RR------------
prof_ses_covar_rd_RR <- bind_rows(injury_co3_df, death_co3_df, total_co3_df) %>% 
  mutate(RR_95CI = paste0(round(estimate,2)," (", round(conf.low,2), ",", round(conf.high, 2), ")")) %>%
  mutate(predictor = case_match(
    term,
    "(Intercept)" ~ "profile_4",
    "clus1" ~ "profile_1",
    "clus2" ~ "profile_2",
    "clus3" ~ "profile_3",
    # "ses_cat_r5" ~ "ses_5",
    # "ses_cat_r4" ~ "ses_4",
    # "ses_cat_r3" ~ "ses_3",
    # "ses_cat_r2" ~ "ses_2",
    # "ses_cat_r1" ~ "ses_1",
    .default = "(Covariates)"
  )) 

#saveRDS(prof_ses_covar_rd_RR, file = "profile_wo_rd_type/prof_a35_ses_cov_rd_RR.rds")
saveRDS(prof_ses_covar_rd_RR, file = "profile_wo_rd_type/profile_a35_cov_rd_RR_ref4.rds")

prof_ses_covar_rd_RR_csv <- prof_ses_covar_rd_RR %>% 
  dplyr::select(
    term, predictor,
    RR_95CI,
    p.value,
    outcome
  )

write_csv(prof_ses_covar_rd_RR_csv, file = "profile_wo_rd_type/prof_a35_ses_cov_rd_RR.csv")

## 5.4 visualize --------
prof_ses_covar_rd_RR <- readRDS("prof_ses_covar_rd_RR.rds")
source("../../functions/plot_RR.R")

prof_ses_covar_rd_RR %>% 
  filter(!predictor == "(Covariates)") %>% 
  plot_RR(., predictor)+
  facet_grid(vars(outcome), switch = "y")+
  labs(
    title = "Pedestrian Collision and Neighborhood (ZAT) Profiles in Bogotá",
    subtitle = "Adjusted for ZAT-level walking/public transit trips, SES level, road types, \nand population density",
    x = "RR (95%CI)",
    y = "ZAT Profile",
    caption = "All comparisons are relative to the profile 1."
  )+
  theme(
    plot.title.position = "plot"
  )

# FYI-Collision~everything (no SES) ------------
profile_covar_rd <- ses_zat %>% 
  #dplyr::select(ZAT, ses_cat) %>% 
  #mutate(ses_cat_r = factor(ses_cat, levels = rev(levels(ses_cat)))) %>% 
  left_join(profile, by = "ZAT") %>% 
  mutate(clus = factor(clus)) %>% 
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

# not that significant either
injury_co4 <- glm.nb(injury ~ clus + offset(log(walk_pubt)) + pop_density + pcta_Collector + pcta_Local + pcta_other, data = profile_covar_rd)
summary(injury_co4)
