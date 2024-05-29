library(tidyverse)
library(sf)
library(broom)
library(MASS)

# 0. import data ----------------------
col_ped_zat <- readRDS("../../clean_data/collision/collision_zat_df.rds")
profile <- readRDS("../../clean_data/aggr_hclust_geo/calle2zat_geo.rds") %>% 
  st_drop_geometry()

profile %>% filter(is.na(clus))

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

# 3. Collision ~ profile ----------------------------------
profile_ped_zat <- col_ped_zat %>% 
  left_join(profile, by = "ZAT") %>% 
  mutate(clus = factor(clus)) %>% 
  drop_na(clus)

## pedestrian collision with injury --------------

fit_injuryP <- glm.nb(injury ~ clus, data = profile_ped_zat)
summary(fit_injuryP)
injuryP_df <- tidy(fit_injuryP, conf.int = TRUE, exponentiate = TRUE) %>% 
  mutate(outcome = 'injury')

## pedestrian collision with death --------------
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
    "(Intercept)" ~ "1",
    "clus2" ~ "2",
    "clus3" ~ "3",
    "clus4" ~ "4"
  )) %>% 
  left_join(profile_distr, by = "clus") %>% 
  mutate(predictor = paste0("profile_", clus)) 

saveRDS(RR_profile, file = "profile_col_RR.rds")

profile_collision_csv <- RR_profile %>% 
  dplyr::select(
    predictor, n, total, 
    percent_zat = percent, 
    RR_95CI, p.value, outcome)


write_csv(profile_collision_csv, file = "collision-profile_zat.csv")

## visualize
## profile-collision ------------
plot_RR(RR_profile, predictor)+
  facet_grid(vars(outcome), switch = "y")+
  labs(
    title = "Pedestrian Collision and Neighborhood Profiles",
    subtitle = "ZAT level, Bogota, Colombia",
    x = "RR (95%CI)",
    y = "ZAT Profile",
    caption = "All comparisons are relative to the profile 1."
  )


# 3. Collision ~ SES + profile -------------------------------
ses_profile_ped <- ses_zat %>% 
  dplyr::select(ZAT, ses_cat) %>% 
  mutate(ses_cat_r = factor(ses_cat, levels = rev(levels(ses_cat)))) %>% 
  left_join(profile, by = "ZAT") %>% 
  mutate(clus = factor(clus)) %>% 
  #drop_na(clus) %>% 
  left_join(col_ped_zat, by = "ZAT") %>% 
  drop_na()

## corr-----------------
ses_profile <- ses_zat %>% 
  dplyr::select(ZAT, ses_cat) %>% 
  left_join(profile, by = "ZAT") %>% 
  mutate(across(-ZAT, ~as.numeric(.), .names = "{.col}")) %>% 
  drop_na()

cor(ses_profile %>% dplyr::select(-ZAT))

# library(corrplot) for plot
# corrplot(mx, method = "number") 

## injury ------------------
fit_injury2 <- glm.nb(injury ~ clus + ses_cat_r, data = ses_profile_ped)
summary(fit_injury2)

## compare with or w/o profile ---------------
BIC(fit_injury2, fit_injuryS)
#about the same

plot(residuals(fit_injury2))

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

## summarise table and RR -------------------
ses_prof_RR <- injury_df %>% 
  bind_rows(death_df, total_df) %>% 
  mutate(RR_95CI = paste0(round(estimate,2)," (", round(conf.low,2), ",", round(conf.high, 2), ")")) %>% 
  mutate(predictor = case_match(
    term,
    "(Intercept)" ~ "profile_1+ses_6",
    "clus2" ~ "profile_2",
    "clus3" ~ "profile_3",
    "clus4" ~ "profile_4",
    "ses_cat_r5" ~ "ses_5",
    "ses_cat_r4" ~ "ses_4",
    "ses_cat_r3" ~ "ses_3",
    "ses_cat_r2" ~ "ses_2",
    "ses_cat_r1" ~ "ses_1"
  )) 

saveRDS(ses_prof_RR, file = "ses_profile_col_RR.rds")

ses_prof_col_csv <- ses_prof_RR %>% 
  dplyr::select(
    predictor,
    RR_95CI,
    p.value,
    outcome
  )

write_csv(ses_prof_col_csv, file = "collision-ses-profile_zat.csv" )


## ses+profile -col ----------
plot_RR(ses_prof_RR, predictor)+
  facet_grid(vars(outcome), switch = "y")+
  labs(
    title = "Pedestrian Collision, Neighborhood Profile and SES Level",
    subtitle = "ZAT level, Bogota, Colombia",
    x = "RR (95%CI)",
    y = "SES Level or Profile",
    caption = "All comparisons are relative to the SES 6 (highest level) and profile 1."
  )+
  theme(
    plot.title.position = "plot"
  )
