library(tidyverse)
library(sf)
library(MASS)
library(broom)
library(patchwork)

# 0. import data----------------
predict24_calle_adj <- readRDS("../../../clean_data/predict24/calle_predict24_1519adj.rds")
#additional road characteristics
add_gis <- readRDS("../../../clean_data/calles/calle_rename_adj_df.rds") %>% 
  dplyr::select(CodigoCL = codigocl,
    road_width, num_lanes_total, st_dir) 

#collision from raw (need to populate 0)
collision_calle <- readRDS("../../../clean_data/collision/collision_calle_df.rds")
#covariates
covar_100 <- readRDS("../../../clean_data/covar_mzn/covar_calle100m.rds")
#road_type
road_type <- readRDS("../../../clean_data/road_type/road_type_calle.rds")

ses_100 <- readRDS("../../../clean_data/covar_mzn/ses_calle100m.rds")

#population from 800m buffer
pop800 <- readRDS("../../../clean_data/covar_mzn/pop_calle800m.rds") %>% 
  mutate(pop_yr = pop *5)  #2015-2019

# 1. prediction-GIS, Stratify-----------------
## 1.1 feature-----------
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

## 1.2 prediction-GIS----------
pg_calle <- predict312k_calle_adj %>% 
  left_join(add_gis, by = "CodigoCL") %>% 
  #rerun because school zone removed from features
  dplyr::select(CodigoCL, st_dir, num_lanes_total, all_of(features)) %>% 
  drop_na() 

## 1.3 stratify by rdtype----------
arterial <- road_type %>% 
  filter(road_type2 == "Arterial") %>% 
  #filter(!CodigoCL == "CL100437") %>% #huge st, but after adj area looks ok
  left_join(pg_calle, by = "CodigoCL") %>% 
  drop_na()

collector <- road_type %>% 
  filter(road_type2 == "Collector") %>% 
  left_join(pg_calle, by = "CodigoCL") %>% 
  drop_na()

local <- road_type %>% 
  filter(road_type2 == "Local") %>% 
  left_join(pg_calle, by = "CodigoCL") %>% 
  drop_na()

other <- road_type %>% 
  filter(road_type2 == "Other") %>% 
  left_join(pg_calle, by = "CodigoCL") %>% 
  drop_na()

# 2. Join feature, cov, collision-----------------
## 2.1 Covar-----------
covar <- covar_100 %>% 
  left_join(pop800, by = "CodigoCL") %>% 
  mutate(
    across(starts_with("pct"), ~ scale(.x)[,1])
    #removed pop_density, because offset by total pop
  ) %>% 
  left_join(ses_100, by = "CodigoCL") %>% 
  mutate(ses_cat_r = factor(ses_cat, levels = rev(levels(ses_cat))))

## 2.3 GIS-prediction-collision
merge_calle_tertile <- function(data){
  data2 <-data %>%
    #dplyr::select(CodigoCL, all_of(features)) %>% #already selected
    mutate(
      across(all_of(features), ~ na_if(., 0)),
      #turn to NA so that it doesn't get computed
      across(all_of(features), ~ ntile(., 3), .names = "{.col}"),
      across(all_of(features), ~ replace_na(., 0)),
      #turn it back 0
      across(all_of(features), ~ factor(., levels = c("1", "0", "2", "3"))),
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
    mutate(across(injury:total, ~ replace_na(., 0)))
  
  data3 <- data2 %>% 
    left_join(covar, by = "CodigoCL")
  return(data3)
}

### join by rdtype--------
arterial_collision <- merge_calle_tertile(arterial)
collector_collision <- merge_calle_tertile(collector)
local_collision <- merge_calle_tertile(local)
other_collision <- merge_calle_tertile(other)

# 3. Model-----------

multi_feature_strat <- function(data){
  formula <- as.formula(paste("total ~ trees + median + median_barrier + sidewalk + sidewalk_obstruction + lane_marking + sign_traffic + traffic_light + lane_bus + sign_crossing + crosswalk + sign_stop + sign_yield + bus_stop + brt_station + speed_bump + lane_bike + road_width + st_dir + road_width + num_lane + offset(log(pop_yr)) + pct_apt + pct_home + pct_unoccu + pct_male + pct_yr_0_9 + pct_yr_10_19 + pct_yr_30_39 + pct_yr_40_49 + pct_yr_50_59 + pct_yr_60_69 + pct_yr_70_79 + pct_yr_80_plus + ses_cat_r"))
  
  model <- glm.nb(formula,  data = data)
  return(model)
}

## 3.1 arterial ----------
art_multi <- multi_feature_strat(arterial_collision)
art_df <- tidy(art_multi, conf.int = TRUE, exponentiate = TRUE)

### summarise---------
art_ter_RR <- art_df %>%
  mutate(
    road_type = "arterial",
    across(where(is.numeric), ~round(., 4)),
    RR_95CI = paste0(estimate," (", conf.low, ",", conf.high, ")")
  ) %>% 
  dplyr::select(
    road_type,
    term,
    RR_95CI,
    estimate,
    std.error,
    conf.low,
    conf.high,
    p.value,
    statistic
  )

saveRDS(art_ter_RR, file = "rdtype_strat312k/multi_art_ter_RR.rds")

## 3.2 collector---------
col_multi <- multi_feature_strat(collector_collision)
col_df <- tidy(col_multi, conf.int = TRUE, exponentiate = TRUE)

### summarise---------
col_ter_RR <- col_df %>% 
  mutate(
    road_type = "collector",
    across(where(is.numeric), ~round(., 4)),
    RR_95CI = paste0(estimate," (", conf.low, ",", conf.high, ")")
  ) %>% 
  dplyr::select(
    road_type,
    term,
    RR_95CI,
    estimate,
    std.error,
    conf.low,
    conf.high,
    p.value,
    statistic
  )

saveRDS(col_ter_RR, file = "rdtype_strat312k/multi_col_ter_RR.rds")

## 3.3 local ----------------
multi_feature_strat_sw <- function(data){
  # sidewalk causes tidy() error, high SE
  formula <- as.formula(paste("total ~ trees + median + median_barrier + sidewalk_obstruction + lane_marking + sign_traffic + traffic_light + lane_bus + sign_crossing + crosswalk + sign_stop + sign_yield + bus_stop + brt_station + speed_bump + lane_bike + road_width + st_dir + road_width + num_lane + offset(log(pop_yr)) + pct_apt + pct_home + pct_unoccu + pct_male + pct_yr_0_9 + pct_yr_10_19 + pct_yr_30_39 + pct_yr_40_49 + pct_yr_50_59 + pct_yr_60_69 + pct_yr_70_79 + pct_yr_80_plus + ses_cat_r"))
  
  model <- glm.nb(formula,  data = data)
  return(model)
}
#loc_multi <- multi_feature_strat(local_collision)
loc_multi <- multi_feature_strat_sw(local_collision)
loc_df <- tidy(loc_multi, conf.int = TRUE, exponentiate = TRUE)

### summarise----------
loc_ter_RR <- loc_df %>% 
  mutate(
    road_type = "local",
    across(where(is.numeric), ~round(., 4)),
    RR_95CI = paste0(estimate," (", conf.low, ",", conf.high, ")")
  ) %>% 
  dplyr::select(
    road_type,
    term,
    RR_95CI,
    estimate,
    std.error,
    conf.low,
    conf.high,
    p.value,
    statistic
  )

saveRDS(loc_ter_RR, file = "rdtype_strat312k/multi_loc_ter_RR.rds")

## 3.4 other -------------

multi_feature_strat_sub <- function(data){
  #warning: alternation limit reached
  #remove sidewalk, median_barrier, bus lane, brt_station, lane_marking, sidewalk obstru, speed bump, 
  formula <- as.formula(paste("total ~ trees + median + sign_traffic + traffic_light + sign_crossing + crosswalk + sign_stop + sign_yield + bus_stop + lane_bike + road_width + st_dir + road_width + num_lane + offset(log(pop_yr)) + pct_apt + pct_home + pct_unoccu + ses_cat_r"))
  
  model <- glm.nb(formula,  data = data)
  return(model)
}

oth_multi <- multi_feature_strat_sub(other_collision) 

oth_df <- tidy(oth_multi, conf.int = TRUE, exponentiate = TRUE)

oth_ter_RR <- oth_df %>% 
  mutate(
    road_type = "other",
    across(where(is.numeric), ~round(., 4)),
    RR_95CI = paste0(estimate," (", conf.low, ",", conf.high, ")")
  ) %>% 
  dplyr::select(
    road_type,
    term,
    RR_95CI,
    estimate,
    std.error,
    conf.low,
    conf.high,
    p.value,
    statistic
  )

saveRDS(oth_ter_RR, file = "rdtype_strat312k/multi_oth_ter_RR.rds")

# 4. visualization-------------
pg_features <- c("st_dir", "num_lane", features)

art_ter_RR <- readRDS("rdtype_strat312k/multi_art_ter_RR.rds")
col_ter_RR <- readRDS("rdtype_strat312k/multi_col_ter_RR.rds")
loc_ter_RR <- readRDS("rdtype_strat312k/multi_loc_ter_RR.rds")
oth_ter_RR <- readRDS("rdtype_strat312k/multi_oth_ter_RR.rds")

## 4.1 construct columns --------------
ter_RR <- bind_rows(art_ter_RR, col_ter_RR, loc_ter_RR, oth_ter_RR) %>% 
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
    category = case_match(
      tertile,
      ")" ~ "Low (ref)",
      "0" ~ "Zero",
      "2" ~ "Medium",
      "3" ~ "High",
      .default = tertile
    ),
    category = if_else(predictor == "St. Directions", "double", category)
  )

## 4.2 assemble plots--------
p3_arterial <- ter_RR %>% 
  filter(road_type == "arterial") %>% 
  plot_facet_RR()+
  labs(
    title = "Arterial roads",
    caption = ""
  )+
  theme(plot.title = element_text(size = 11, hjust = 0))

p3_collector <- ter_RR %>% 
  filter(road_type == "collector") %>% 
  plot_facet_RR()+
  labs(
    y = "",
    title = "Collector roads",
    caption = ""
  )+
  theme(plot.title = element_text(size = 11, hjust = 0))

p3_local <- ter_RR %>% 
  filter(road_type=="local") %>% 
  plot_facet_RR()+
  labs(
    y = "",
    title = "Local roads",
    caption = ""
  )+
  theme(plot.title = element_text(size = 11, hjust = 0))

p3_other <- ter_RR %>% 
  filter(road_type =="other") %>% 
  plot_facet_RR()+
  labs(
    y = "",
    title = "Other roads",
    caption = "'St. Directions' is included as 1 and 2 directions, relative to 1. \n'Total Lanes' is included as categories of total lane counts, relative to 2 lanes. \nOther street features are separated into zeros and nonzero tertiles for analysis.\n Comparisons are relative to the 'Low' category."
  )+
  theme(plot.title = element_text(size = 11))

(p3_arterial|p3_collector|p3_local |p3_other) +
  plot_annotation(
    'Pedestrian Collision (total) and Street Features in Bogotá',
    subtitle = "Multivariable analysis with image prediction data and selected GIS road characteristics",
    theme=theme(plot.title=element_text(size=14, face = "bold", hjust=0.5),
      plot.subtitle = element_text(size =12, hjust = 0.5))
  )
