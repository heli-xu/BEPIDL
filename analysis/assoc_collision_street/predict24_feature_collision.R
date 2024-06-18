library(tidyverse)
library(sf)
library(MASS)
library(broom)

# 0. import data----------------
predict24_calle <- readRDS("../../clean_data/predict24/calle_predict24.rds")
# collision
calle_rename_df <- readRDS("../../clean_data/calles/calle_rename_df.rds")
#covariates
covar_100 <- readRDS("../../clean_data/SES/covar_calle100m.rds")
#road_type
road_type <- readRDS("../../clean_data/road_type/road_type_calle.rds")


# 1. Prep features-------------------
### features to tertile (all counts)
features <-  c(
  "trees",
  #"grade",
  "median", #remove "area_"
  "sidewalk", #no "area_"
  #"road_width",
  "lane_marking", #"road_marks"
  #"warning_signs",
  "sign_traffic",  #"road_signs"
  "traffic_light", #no light"s"
  #"st_dir",  only 1,2 directions
  "lane_bus", #"num_lane_total"
  "sign_crossing",
  "crosswalk",
  #"pedxwalk_signs",
  "sign_school_zone",
  #"stop_signs_v",
  "sign_stop",  #rename
  "sign_yield",  #rename
  #"total_st_signs",
  "bus_stop",  #gis-routes
  "brt_station", #gis-routes
  "lane_bike" #gis-length
 # "traffic_fines_tot"
)


## 1.1 Zeroes, nonzero Tertiles, factored--------------
calle_tertile <- predict24_calle %>% 
  dplyr::select(CodigoCL, all_of(features)) %>% 
  left_join(calle_rename_df %>% 
      dplyr::select(CodigoCL = codigocl, ped_collision), by = "CodigoCL") %>%
  #drop_na()
  mutate(
    across(all_of(features), ~na_if(., 0)), #turn to NA so that it doesn't get computed
    across(all_of(features), ~ntile(., 3), .names = "{.col}"),
    across(all_of(features), ~replace_na(., 0)), #turn it back 0
    across(-c(CodigoCL, ped_collision), ~factor(., levels = c("1", "0", "2", "3")))
  )

### zeros in feature count --------
count_zero <- calle_tertile %>% 
  summarise(across(everything(), ~ sum(. == 0))) %>% 
  pivot_longer(cols = -CodigoCL, names_to = "feature", values_to = "zero_count") %>% 
  mutate(total = nrow(calle_tertile),
    percent = (zero_count/total)*100) %>% 
  dplyr::select(-CodigoCL)

## 1.2 Join cov100m and rd type--------------------
collision_covar_rd_100 <- covar_100 %>%
  mutate(
    across(c(pop_density, starts_with("pct")), ~ scale(.x)[,1])
  ) %>% 
  left_join(road_type, by = "CodigoCL") %>% 
  mutate(road_type2 = factor(road_type2, levels = c("Local", "Arterial", "Collector", "Other"))) %>% 
  #rename(codigocl = CodigoCL) %>% 
  right_join(calle_tertile, by = "CodigoCL") %>% 
  drop_na(road_type2)

# 2. Collision~feature+cov+rd --------------------
## 2.1 Iterate function -------------
fit_feature <- glm.nb(ped_collision ~ median + pct_apt + pct_home + pct_unoccu + pop_density + pct_male + pct_yr_0_9 + pct_yr_10_19 + pct_yr_30_39 + pct_yr_40_49 + pct_yr_50_59 + pct_yr_60_69 + pct_yr_70_79 + pct_yr_80_plus + road_type2, data = collision_covar_rd_100)

summary(fit_feature)

fit_features_x <- function(predictor, data){
  formula <- as.formula(paste("ped_collision ~", predictor, "+ pct_apt + pct_home + pct_unoccu + pop_density + pct_male + pct_yr_0_9 + pct_yr_10_19 + pct_yr_30_39 + pct_yr_40_49 + pct_yr_50_59 + pct_yr_60_69 + pct_yr_70_79 + pct_yr_80_plus + road_type2"))
  
  model <- glm.nb(formula,  data = data)
  return(model)
}

test <- fit_features_x("median", collision_covar_rd_100)
summary(test)

### interate -------------
fit_allfeatures <- map(features, \(x) fit_features_x(x, data = collision_covar_rd_100))

feature_df <- map_df(fit_allfeatures,
  \(x) tidy(x, conf.int = TRUE, exponentiate = TRUE))
#took 20min or something  

## 3.3 summarise RR -----------------------
feature_RR <- feature_df %>%
  mutate(
    RR_95CI = paste0(round(estimate,2)," (", round(conf.low,2), ",", round(conf.high, 2), ")"),
    tertile = str_sub(term, -1),
    predictor = str_sub(term, end = -2),
    category = case_match(
      tertile,
      ")" ~ "Low (ref)",
      "0" ~ "Zero",
      "2" ~ "Medium",
      "3" ~ "High",
      .default = "(Covariates)"
    )
  ) 

saveRDS(feature_RR, file = "predict24/feature_cov100_rdtype_RR.rds")

## 3.4 visualize --------------
source("../../functions/plot_facet_RR.R")
feature_RR<- readRDS("predict24/feature_covar100_rdtype_RR.rds")

feature_RR %>% 
  filter(!term == "(Intercept)",
    !category == "(Covariates)") %>% 
  plot_facet_RR()+
  labs(
    caption = "Data are separated into zeros and nonzero tertiles for analysis. \nComparisons are relative to the 'Low' category.",
    subtitle = "Adjusted for types of dwellings, population density, age groups, sex composition \nand road types (100m)"
  )+
  theme(
    plot.title.position = "plot"
  )
