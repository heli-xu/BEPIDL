library(tidyverse)
library(MASS)
library(broom)

options(scipen = 999)

# 0. import data----------------
#features(area adjusted)
predict24_calle_adj <- readRDS("../../../clean_data/predict24/calle_predict24_1519adj.rds")

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

# 1. stratify by road type---------------
arterial <- road_type %>% 
  filter(road_type2 == "Arterial") %>% 
  #filter(!CodigoCL == "CL100437") %>% #huge st, but after adj area looks ok
  left_join(predict24_calle_adj, by = "CodigoCL") %>% 
  drop_na()

collector <- road_type %>% 
  filter(road_type2 == "Collector") %>% 
  left_join(predict24_calle_adj, by = "CodigoCL") %>% 
  drop_na()

local <- road_type %>% 
  filter(road_type2 == "Local") %>% 
  left_join(predict24_calle_adj, by = "CodigoCL") %>% 
  drop_na()

other <- road_type %>% 
  filter(road_type2 == "Other") %>% 
  left_join(predict24_calle_adj, by = "CodigoCL") %>% 
  drop_na()

# 2. Feature, cov -----------------------
## 2.1 features -------------------
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
  "lane_bike" #gis-length
  # "traffic_fines_tot"
)

## 2.2 Covar --------------

covar <- covar_100 %>%
  left_join(pop800, by = "CodigoCL") %>% 
  mutate(
    across(starts_with("pct"), ~ scale(.x)[,1])
    #removed pop_density, because offset by total pop
  ) %>% 
  left_join(ses_100, by = "CodigoCL") %>% 
  mutate(ses_cat_r = factor(ses_cat, levels = rev(levels(ses_cat))))

## 2.3 Join cov-feature--------------
merge_calle_tertile <- function(data){
  data2 <-data %>%
    dplyr::select(CodigoCL, all_of(features)) %>%
    mutate(
      across(all_of(features), ~ na_if(., 0)),
      #turn to NA so that it doesn't get computed
      across(all_of(features), ~ ntile(., 3), .names = "{.col}"),
      across(all_of(features), ~ replace_na(., 0)),
      #turn it back 0
      across(-CodigoCL, ~ factor(., levels = c("1", "0", "2", "3")))
    ) %>% 
    left_join(collision_calle, by = "CodigoCL") %>%
    mutate(across(injury:total, ~ replace_na(., 0)))
  
  data3 <- data2 %>% 
    left_join(covar, by = "CodigoCL")
  return(data3)
}

### 2.3.1 join covar-ter-----------
arterial_collision <- merge_calle_tertile(arterial)
collector_collision <- merge_calle_tertile(collector)
local_collision <- merge_calle_tertile(local)
other_collision <- merge_calle_tertile(other)

# 3. Model-Tertiles ---------------

fit_feature_strat <- function(predictor, data){
  formula <- as.formula(paste("total ~", predictor, "+ offset(log(pop_yr)) + pct_apt + pct_home + pct_unoccu + pct_male + pct_yr_0_9 + pct_yr_10_19 + pct_yr_30_39 + pct_yr_40_49 + pct_yr_50_59 + pct_yr_60_69 + pct_yr_70_79 + pct_yr_80_plus + ses_cat_r"))
  
  model <- glm.nb(formula,  data = data)
  cli::cli_alert_success("modeling collision ~ {predictor} completed.")
  return(model)
}

## 3.1 arterial ----------------
fit_allfea_art <- map(features, \(x) fit_feature_strat(x, data = arterial_collision))

feature_ter_df <- map_df(fit_allfea_art,
  \(x) tidy(x, conf.int = TRUE, exponentiate = TRUE))

art_ter_RR <- feature_ter_df %>%
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

saveRDS(art_ter_RR, file = "rdtype_strat/art_ter_RR.rds")

## 3.2 collector ----------------
fit_allfea_col <- map(features, \(x) fit_feature_strat(x, data = collector_collision))

feature_col_df <- map_df(fit_allfea_col,
  \(x) tidy(x, conf.int = TRUE, exponentiate = TRUE))

col_ter_RR <- feature_col_df %>%
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

saveRDS(col_ter_RR, file = "rdtype_strat/col_ter_RR.rds")

## 3.3 local ----------------------
fit_feature_strat_sub <- function(predictor, data){
  formula <- as.formula(paste("total ~", predictor, "+ offset(log(pop_yr)) + pct_apt + pct_home + pct_unoccu + pct_male + ses_cat_r"))
  
  model <- glm.nb(formula,  data = data)
  cli::cli_alert_success("modeling collision ~ {predictor} completed.")
  return(model)
}

local_collision %>% count(sidewalk)
#sidewalk high SE at 0 level

test <- fit_feature_strat_sub("sidewalk", local_collision)
summary(test)
#removing cov does not improve high SE 

features <- features[!features == "sidewalk"]

fit_allfea_loc <- map(features, \(x) fit_feature_strat(x, data = local_collision))

feature_loc_df <-
  map_df(fit_allfea_loc, \(x) tidy(x, conf.int = TRUE, exponentiate = TRUE))

loc_ter_RR <- feature_loc_df %>% 
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

saveRDS(loc_ter_RR, file = "rdtype_strat/loc_ter_RR.rds")

## 3.4 other --------------
## remove pct_male helps with crosswalk alternation limit
fit_feature_strat_sub2 <- function(predictor, data){
  formula <- as.formula(paste("total ~", predictor, "+ offset(log(pop_yr)) + pct_apt + pct_home + pct_unoccu + ses_cat_r"))
  
  model <- glm.nb(formula,  data = data)
  cli::cli_alert_success("modeling collision ~ {predictor} completed.")
  return(model)
}

## iteration yields warnings. try individual feature. remove pct_male (above)
test <- fit_feature_strat_sub2(features[[11]], other_collision)
summary(test)

##tidy() errors
#median_barrier and sidewalk coefficient error in tidy()
other_collision %>% count(median_barrier)
#too many 0, only 17 in each tertile

other_collision %>% count(sidewalk) # not that many 0
test <- fit_feature_strat_sub2("sidewalk", other_collision)
summary(test) #HUGE SE at sidewalk = 0

##tidy() warnings
#warning msg always comes out at the end, not indicating which one caused it
#had to check individually
tidy(fit_allfea_oth[[9]], conf.int = TRUE, exponentiate = TRUE)
# converging warnings:
# sign_crossing, sign_school_zone

other_collision %>% count(brt_station)
##brt_station only has 14 for each tertile

#rerun features from begining
features2 <- c("crosswalk", "sign_crossing", "sign_school_zone")

features <- features[!features %in% c(features2, "median_barrier", "sidewalk", "brt_station")]

## feature2 use fit_feature_strat_sub2()
fit2 <- map(features2, \(x) fit_feature_strat_sub2(x, data = other_collision))
fit2_df <- map_df(fit2, \(x) tidy(x, conf.int = TRUE, exponentiate = TRUE))

## the rest use fit_feature_strat_sub()
fit_allfea_oth <- map(features, \(x) fit_feature_strat_sub(x, data = other_collision))

feature_oth_df <-
  map_df(fit_allfea_oth, \(x) tidy(x, conf.int = TRUE, exponentiate = TRUE))

oth_ter_RR <- feature_oth_df %>% 
  bind_rows(fit2_df) %>% 
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

saveRDS(oth_ter_RR, file = "rdtype_strat/oth_ter_RR.rds")


