library(tidyverse)
library(sf)
library(MASS)
library(broom)

# 0. import data----------------
predict312k_calle_adj <- readRDS("../../../../clean_data/predict24/calle_predict24_1519adj.rds")

#additional road characteristics
add_gis <- readRDS("../../../../clean_data/calles/calle_rename_adj_df.rds") %>% 
  dplyr::select(CodigoCL = codigocl,
    road_width, num_lanes_total, st_dir) 

#collision from raw (need to populate 0)
collision_calle <- readRDS("../../../../clean_data/collision/collision_calle_df.rds")
#covariates
covar_100 <- readRDS("../../../../clean_data/covar_mzn/covar_calle100m.rds")
#road_type
road_type <- readRDS("../../../../clean_data/road_type/road_type_calle.rds")

ses_100 <- readRDS("../../../../clean_data/covar_mzn/ses_calle100m.rds")

#population from 800m buffer
pop800 <- readRDS("../../../../clean_data/covar_mzn/pop_calle800m.rds") %>% 
  mutate(pop_yr = pop *5)  #2015-2019


# 1. Prep features-------------------
## tertile/YN (all counts)---------
features <-  c(
  "trees",
  #"grade",
  "median", #remove "area_"
  "median_barrier", #added
  "sidewalk", #no "area_"
  "sidewalk_obstruction",
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
  "speed_bump",
  "lane_bike", #gis-length; here counts
 # "traffic_fines_tot"
  ###GIS below:###
  "road_width"
)

# 2. Join feature - collision --------
## 2.1 Zeroes, nonzero Tertiles, factored--------------
calle_tertile <- predict312k_calle_adj %>% 
  left_join(add_gis, by = "CodigoCL") %>% 
  #rerun because school zone removed from features
  dplyr::select(CodigoCL, st_dir, num_lanes_total, all_of(features)) %>% 
  drop_na() %>% 
  mutate(
    across(all_of(features), ~na_if(., 0)), #turn to NA so that it doesn't get computed
    across(all_of(features), ~ntile(., 3), .names = "{.col}"),
    across(all_of(features), ~replace_na(., 0)), #turn it back 0
    across(all_of(features), ~factor(., levels = c("1", "0", "2", "3"))),
    # lane factors:
    num_lane = case_when(
      num_lanes_total == 1 ~ "1",  #also numeric -> character
      num_lanes_total == 2 ~ "2",
      num_lanes_total %in% c(3, 4) ~ "3-4",
      num_lanes_total >= 5 ~ "5+"
    ),
    num_lane = factor(num_lane, levels = c("2", "1", "3-4", "5+")),
    st_dir = factor(st_dir)
  ) %>% 
  left_join(collision_calle, by = "CodigoCL") %>% 
  mutate(
    across(injury:total, ~replace_na(., 0))
  )


## zeros in feature count --------
count_zero <- calle_tertile %>% 
  summarise(across(everything(), ~ sum(. == 0))) %>% 
  pivot_longer(cols = -CodigoCL, names_to = "feature", values_to = "zero_count") %>% 
  mutate(total = nrow(calle_tertile),
    percent = (zero_count/total)*100) %>% 
  dplyr::select(-CodigoCL)

## 2.2 YN --------------------
calle_yn <- predict312k_calle_adj %>% 
  dplyr::select(CodigoCL, all_of(features)) %>% 
  mutate(
    across(all_of(features), ~if_else(. >0, 1, 0)),
    across(-CodigoCL, ~factor(.))
  ) %>% 
  left_join(collision_calle, by = "CodigoCL") %>% 
  mutate(
    across(injury:total, ~replace_na(., 0))
  )

levels(calle_yn$trees)

# 3. Join cov100m, SES, rd type--------------------
## 3.1 tertiles -----------
collision_covar_rd_ter <- covar_100 %>%
  left_join(pop800, by = "CodigoCL") %>% 
  mutate(
    across(starts_with("pct"), ~ scale(.x)[,1])
  ) %>%  #remove pop_density
  left_join(ses_100, by = "CodigoCL") %>% 
  mutate(ses_cat_r = factor(ses_cat, levels = rev(levels(ses_cat)))) %>% 
  left_join(road_type, by = "CodigoCL") %>% 
  mutate(road_type2 = factor(road_type2, levels = c("Local", "Arterial", "Collector", "Other"))) %>% 
  #rename(codigocl = CodigoCL) %>% 
  right_join(calle_tertile, by = "CodigoCL") %>% 
  drop_na(road_type2)


## 3.2 YN --------------
collision_covar_rd_yn <- covar_100 %>%
  left_join(pop800, by = "CodigoCL") %>% 
  mutate(
    across(starts_with("pct"), ~ scale(.x)[,1])
  ) %>%  #remove pop_density
  left_join(ses_100, by = "CodigoCL") %>% 
  mutate(ses_cat_r = factor(ses_cat, levels = rev(levels(ses_cat)))) %>% 
  left_join(road_type, by = "CodigoCL") %>% 
  mutate(road_type2 = factor(road_type2, levels = c("Local", "Arterial", "Collector", "Other"))) %>% 
  #rename(codigocl = CodigoCL) %>% 
  right_join(calle_yn, by = "CodigoCL") %>% 
  drop_na(road_type2)

# 4. Collision~feature+cov+rd --------------------
## Iterate function -------------
fit_feature <- glm.nb(ped_collision ~ median + pct_apt + pct_home + pct_unoccu + pop_density + pct_male + pct_yr_0_9 + pct_yr_10_19 + pct_yr_30_39 + pct_yr_40_49 + pct_yr_50_59 + pct_yr_60_69 + pct_yr_70_79 + pct_yr_80_plus + road_type2, data = collision_covar_rd_100)

summary(fit_feature)

fit_features_x <- function(predictor, data){
  formula <- as.formula(paste("total ~", predictor, "+ offset(log(pop_yr)) + pct_apt + pct_home + pct_unoccu + pct_male + pct_yr_0_9 + pct_yr_10_19 + pct_yr_30_39 + pct_yr_40_49 + pct_yr_50_59 + pct_yr_60_69 + pct_yr_70_79 + pct_yr_80_plus + road_type2 + ses_cat_r"))
  
  model <- glm.nb(formula,  data = data)
  cli::cli_alert_success("modeling collision ~ {predictor} completed.")
  return(model)
}

test <- fit_features_x("median", collision_covar_rd_ter)
summary(test)

## complete data analysis----
all_features <- c(features, "num_lane", "st_dir")

collision_covar_rd_ter2 <- collision_covar_rd_ter |> 
  select(CodigoCL, pop_yr, pct_apt, pct_home, pct_unoccu, pop_density, pct_male, pct_yr_0_9, pct_yr_10_19, pct_yr_30_39, pct_yr_40_49, pct_yr_50_59, pct_yr_60_69, pct_yr_70_79, pct_yr_80_plus, road_type2, ses_cat_r, all_of(all_features), total, injury, death) |> 
  drop_na()
# 43496->43342

##  4.1 tertiles-------------
fit_allfeatures <- map(all_features, \(x) fit_features_x(x, data = collision_covar_rd_ter2))

feature_df <- map_df(fit_allfeatures,
  \(x) tidy(x, conf.int = TRUE, exponentiate = TRUE))
#took 20min or something  

### summarise RR -----------------------
feature_RR <- feature_df %>%
  mutate(
    across(where(is.numeric), ~round(., 4)),
    RR_95CI = paste0(round(estimate,2)," (", round(conf.low,2), ",", round(conf.high, 2), ")")) %>% 
  dplyr::select(
    term,
    RR_95CI,
    estimate,
    std.error,
    conf.low,
    conf.high,
    p.value,
    statistic
  )

saveRDS(feature_RR, file = "feature312k_ter_RR.rds")

### visualize --------------
source("../../../../functions/plot_facet_RR.R")
feature_RR <- readRDS("feature312k_ter_RR.rds")

fea_plot_RR <- feature_RR %>%
  mutate(tertile = str_sub(term,-1),
    predictor = str_sub(term, end = -2)) %>% 
  filter(predictor %in% features) %>% 
  mutate(
    predictor = case_match(
      predictor,
      "trees" ~ "Trees",
      "median" ~ "Median",
      "median_barrier" ~ "Median Barrier",
      "sidewalk" ~ "Sidewalk",
      "sidewalk_obstruction" ~ "Sidewalk Obstruction",
      "lane_marking" ~ "Lane Marking",
      "sign_traffic" ~ "Traffic Sign",
      "traffic_light" ~ "Traffic Light",
      "lane_bus" ~ "Bus Lane",
      "sign_crossing" ~ "Crossing Sign",
      "crosswalk" ~ "Crosswalk",
      "sign_school_zone" ~ "School Zone Sign", #not in multivariate
      "sign_stop" ~ "Stop Sign",
      "sign_yield" ~ "Yield Sign",
      "bus_stop" ~ "Bus Stop",
      "brt_station" ~ "Brt Station",
      "speed_bump" ~ "Speed Bump",
      "lane_bike" ~ "Bike Lane",
      "road_width" ~ "Road Width",
      "st_dir" ~ "St. Directions",
      "num_lane" ~ "Total Lanes"
    ),
    category = case_match(tertile,
      ")" ~ "Low (ref)",
      "0" ~ "Zero",
      "2" ~ "Medium",
      "3" ~ "High")
  )

fea_plot_RR %>% 
  plot_facet_RR()+
  labs(
    subtitle = "Offset by population within 800m from each street. Adjusted for types of dwellings,\nage groups, sex composition, road types and SES within 100m from streets.",
    caption = "Values of street features are separated into zeros and nonzero tertiles for analysis.\n Comparisons are relative to the 'Low' category."
  )+
  theme(
    plot.title.position = "plot"
  )

## 4.2 YN ---------------
fit_allfeatures <- map(features, \(x) fit_features_x(x, data = collision_covar_rd_yn))

feature_yn_df <- map_df(fit_allfeatures,
  \(x) tidy(x, conf.int = TRUE, exponentiate = TRUE))

saveRDS(feature_yn_RR, file = "feature_yn_RR.rds")

### summarise RR -----------------------
feature_yn_RR <- feature_yn_df %>%
  mutate(
    across(where(is.numeric), ~round(., 4)),
    RR_95CI = paste0(round(estimate,2)," (", round(conf.low,2), ",", round(conf.high, 2), ")")) %>% 
  dplyr::select(
    term,
    RR_95CI,
    estimate,
    std.error,
    conf.low,
    conf.high,
    p.value,
    statistic
  )

### visualize --------------
source("../../functions/plot_facet_RR.R")

fea_plot_RR <- feature_yn_RR %>% 
  mutate(
    tertile = str_sub(term,-1),
    predictor = str_sub(term, end = -2)
  ) %>%
  filter(predictor %in% features) %>%
  mutate(category = "")

fea_plot_RR %>% 
  plot_facet_RR()+
  labs(
    subtitle = "Offset by population within 800m from each street. Adjusted for types of dwellings,\nage groups, sex composition, road types and SES within 100m from streets.",
    caption = "Street features are considered yes/no for analysis.\n Comparisons are relative to the 'no' category."
  )+
  theme(
    plot.title.position = "plot"
  )
