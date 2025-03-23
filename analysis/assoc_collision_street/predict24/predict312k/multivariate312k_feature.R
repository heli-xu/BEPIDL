library(tidyverse)
library(sf)
library(MASS)
library(broom)
library(gtsummary)

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

# 1. prepare feature, cov, collision-----------------
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
  #"sign_school_zone", #removed bc correlated
  #"stop_signs_v",
  "sign_stop",  #rename
  "sign_yield",  #rename
  #"total_st_signs",
  "bus_stop",  #gis-routes
  "brt_station", #gis-routes
  "speed_bump",
  "lane_bike", #gis-length
  # "traffic_fines_tot"
  ##GIS below:
  # "st_dir",
  #"num_lanes_total",
  "road_width"
)

## 1.1 correlation ---------------------
pg_calle <- predict312k_calle_adj %>% 
  left_join(add_gis, by = "CodigoCL") %>% 
  dplyr::select(CodigoCL, st_dir, num_lanes_total, all_of(features)) %>% 
  drop_na() # join gis leads to some NA (though calle_rename itself doesn have NA)

cor_matrix <- cor(pg_calle[,-1]) 

cor_matrix[upper.tri(cor_matrix, diag = FALSE)] <- NA
# 'FALSE' keep the diag 1, to filter it's better to keep TRUE, but for the look of table, I set as FALSE
# >0.6 only those with related variables
##school sign with traffic sign >0.6

###school sign removed####

mx0.6 <- cor_matrix[cor_matrix > 0.6] # a vector of values >0.6

write.csv(cor_matrix, file = "correlation_matrix312k.csv", row.names = TRUE)

## 1.2 Tertiles --------
### 1.2.1 join prediction-GIS, collision ---------
pg_calle_ter <- predict312k_calle_adj %>% 
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

### 1.2.2 Join covar-------------
pg_ter_covar <- covar_100 %>%
  left_join(pop800, by = "CodigoCL") %>% 
  mutate(
    across(starts_with("pct"), ~ scale(.x)[,1])
  ) %>%  #remove pop_density
  left_join(ses_100, by = "CodigoCL") %>% 
  mutate(ses_cat_r = factor(ses_cat, levels = rev(levels(ses_cat)))) %>% 
  left_join(road_type, by = "CodigoCL") %>% 
  mutate(road_type2 = factor(road_type2, levels = c("Local", "Arterial", "Collector", "Other"))) %>% 
  #rename(codigocl = CodigoCL) %>% 
  right_join(pg_calle_ter, by = "CodigoCL") %>% 
  drop_na(road_type2)

### 1.2.3 Complete data analysis----
pg_ter_covar2 <- pg_ter_covar |> 
  select(CodigoCL, pop_yr, pct_apt, pct_home, pct_unoccu, pop_density, pct_male, pct_yr_0_9, pct_yr_10_19, pct_yr_30_39, pct_yr_40_49, pct_yr_50_59, pct_yr_60_69, pct_yr_70_79, pct_yr_80_plus, road_type2, ses_cat_r, all_of(features), st_dir, num_lane, total, injury, death) |> 
  drop_na()
# 43396->43342


# 2. Model-----------

multi_feature_ter <- glm.nb(total ~trees + median + median_barrier + sidewalk + sidewalk_obstruction + lane_marking + sign_traffic + traffic_light + lane_bus + sign_crossing + crosswalk + sign_stop + sign_yield + bus_stop + brt_station + speed_bump + lane_bike + road_width + st_dir + num_lane + offset(log(pop_yr)) + pct_apt + pct_home + pct_unoccu + pct_male + pct_yr_0_9 + pct_yr_10_19 + pct_yr_30_39 + pct_yr_40_49 + pct_yr_50_59 + pct_yr_60_69 + pct_yr_70_79 + pct_yr_80_plus + road_type2 + ses_cat_r, data = pg_ter_covar2)

multi_feature_df <- tidy(multi_feature_ter, conf.int = TRUE, exponentiate = TRUE)

### summarise----------
multi_feature_RR <- multi_feature_df %>%
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

saveRDS(multi_feature_RR, file = "multi_feature312k_ter_RR.rds")

### visualize-----------
source("../../../../functions/plot_facet_RR.R")
pg_features <- c("st_dir", "num_lane", features)
multi_feature_RR <- readRDS("multi_feature312k_ter_RR.rds")


fea_plot_RR <- multi_feature_RR %>% 
  mutate(
    #note the modification for num_lane
    tertile = if_else(term %in% c("num_lane1","num_lane3-4", "num_lane5+"), 
      str_sub(term, 9), str_sub(term,-1)),
    predictor = case_when(
      term %in% c("num_lane1","num_lane3-4", "num_lane5+") ~ "num_lane",
      .default = str_sub(term, end = -2)
    )) %>%
  filter(predictor %in% pg_features) %>%
  mutate(
    predictor = case_match(
      predictor,
      "trees" ~ "Trees",
      "median" ~ "Median",
      "median_barrier" ~ "Median Barrier",
      "sidewalk" ~ "Sidewalk",
      "sidewalk_obstruction" ~ "Sidewalk Obstruction",
      "lane_marking" ~ "Lane Marking",
      "sign_traffic" ~ "Sign Traffic",
      "traffic_light" ~ "Traffic Light",
      "lane_bus" ~ "Bus Lane",
      "sign_crossing" ~ "Crossing Sign",
      "crosswalk" ~ "Crosswalk",
      #"sign_school_zone" ~ "School Zone Sign", #not in multivariate
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
      "3" ~ "High",
      .default = tertile),
    category = if_else(predictor == "st_dir", "double", category)
  )

fea_plot_RR %>% 
  plot_facet_RR()+
  scale_fill_manual(values = rep(c("#ffffff00", "#B6D0E2"), 20), guide = "none")+
  labs(
    subtitle = "Offset by population within 800m from each street. Adjusted for types of dwellings,\nage groups, sex composition, road types and SES within 100m from streets.",
    caption = "'St. Directions' is included as 1 and 2 directions, relative to 1. \n'Total Lanes' is included as categories of total lane counts, relative to 2 lanes. \nOther street features are separated into zeros and nonzero tertiles for analysis.\n Comparisons are relative to the 'Low' category."
  )+
  theme(
    plot.title.position = "plot"
  )
