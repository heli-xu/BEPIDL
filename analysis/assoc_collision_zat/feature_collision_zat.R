library(tidyverse)
library(sf)
library(broom)
library(MASS)

# 0. import data ----------------------
col_ped_zat <- readRDS("../../clean_data/collision/collision_zat_df.rds")

zat_indicators <- readRDS("../../clean_data/ZAT/zat_std2n.rds")

road_type <- readRDS("../../clean_data/road_type/rd_type_area_zat.rds")

#ses
ses_zat <- readRDS("../../clean_data/ses/ses_zat.rds")

#covar, offset
traffic <- readRDS("../../clean_data/ZAT/walk_pubt.rds")

pop_density <- readRDS("../../clean_data/ZAT/pop_density2021.rds")


# 1. TREE ---------
## 1.1 join data ---------
rd_type_zat <- road_type %>% 
  dplyr::select(-c(Collector:total, pcta_Arterial, pcta_Rural, pcta_Pedestrian, pcta_Unknown, pcta_Projected)) 

tree <- zat_indicators %>% 
  dplyr::select(ZAT, sttree_per_km2) %>% 
  mutate(
    across(-ZAT, ~na_if(., 0)), #turn to NA so that it doesn't get computed
    across(-ZAT, ~ntile(., 3), .names = "{.col}"),
    across(-ZAT, ~replace_na(., 0)), #turn it back 0
    across(-ZAT, ~factor(., levels = c("1", "0", "2", "3")))
  )

zat_ped <- ses_zat %>% 
  dplyr::select(ZAT, ses_cat) %>% 
  mutate(ses_cat_r = factor(ses_cat, levels = rev(levels(ses_cat)))) %>% 
  left_join(tree, by = "ZAT") %>% 
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

fit_injury <- glm.nb(injury ~ sttree_per_km2 +ses_cat_r + offset(log(walk_pubt)) + pop_density + pcta_Collector + pcta_Local + pcta_other, data = zat_ped)
summary(fit_injury)
injury_df <- tidy(fit_injury, conf.int = TRUE, exponentiate = TRUE) %>% 
  mutate(outcome = 'injury')

## death --------------
fit_death <- glm.nb(death ~ sttree_per_km2 +ses_cat_r + offset(log(walk_pubt)) + pop_density + pcta_Collector + pcta_Local + pcta_other, data = zat_ped)
summary(fit_death)

death_df <- tidy(fit_death, conf.int = TRUE, exponentiate = TRUE) %>% 
  mutate(outcome='death')

## total pedestrian collision --------------
fit_total <- glm.nb(total ~ sttree_per_km2 +ses_cat_r + offset(log(walk_pubt)) + pop_density + pcta_Collector + pcta_Local + pcta_other, data = zat_ped)
summary(fit_total)

## summarise RR -----------------------------
RR_tree <- tidy(fit_total, conf.int = TRUE, exponentiate = TRUE) %>% 
  mutate(outcome='total') %>% 
  bind_rows(injury_df, death_df) %>% 
  mutate(RR_95CI = paste0(round(estimate,2)," (", round(conf.low,2), ",", round(conf.high, 2), ")"))
