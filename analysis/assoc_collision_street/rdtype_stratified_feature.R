library(tidyverse)
library(MASS)
library(broom)

options(scipen = 999)


# 0. import data----------------
#features(area adjusted)
calle_rename_adj_df <- readRDS("../../clean_data/calles/calle_rename_adj_df.rds")
#collision from raw (need to populate 0)
collision_calle <- readRDS("../../clean_data/collision/collision_calle_df.rds")
#covariates
covar_100 <- readRDS("../../clean_data/covar_mzn/covar_calle100m.rds")
#road_type
road_type <- readRDS("../../clean_data/road_type/road_type_calle.rds")

ses_100 <- readRDS("../../clean_data/covar_mzn/ses_calle100m.rds")

#population from 800m buffer
pop800 <- readRDS("../../clean_data/covar_mzn/pop_calle800m.rds") %>% 
  mutate(pop_yr = pop *5)  #2015-2019

# 1. feature stratify by road type -----------------
arterial <- road_type %>% 
  filter(road_type2 == "Arterial") %>% 
  #filter(!CodigoCL == "CL100437") %>%  #adjusted for calle_area made it less crazy large
  rename(codigocl = CodigoCL) %>%
  left_join(calle_rename_adj_df, by = "codigocl") %>% 
  drop_na()  ##important! calle_rename_adj_df no NA, but left_join() introduce NAs
#remove NA as much as you can helps with modeling, also sometimes 0 were filtered out in calle data but here you reintroduce them later on (with repopulating 0)

#alternative: pull(id) then filter()
# arterial <- calle_rename_adj_df %>% 
#   filter(codigocl %in% arterial_id)

collector <- road_type %>% 
  filter(road_type2 == "Collector") %>% 
  rename(codigocl = CodigoCL) %>% 
  left_join(calle_rename_adj_df, by = "codigocl") %>% 
  drop_na()

local <- road_type %>% 
  filter(road_type2 == "Local") %>% 
  rename(codigocl = CodigoCL) %>% 
  left_join(calle_rename_adj_df, by = "codigocl") %>% 
  drop_na()

other <- road_type %>% 
  filter(road_type2 == "Other") %>% 
  rename(codigocl = CodigoCL) %>% 
  left_join(calle_rename_adj_df, by = "codigocl") %>% 
  drop_na()


# 2. Prepare feature, cov---------------------
## 2.1 features-----------
### tertile-------
features_ter <-  c(
  "trees",
  "grade",
  "area_median",
  "area_sidewalk",  #supposedly Y/N, just to check
  "road_width",
 # "road_marks", remove across board b/c low completion data
  "warning_signs",
  "road_signs",
  "traffic_lights",
  #"st_dir",  only 1,2 directions
  ##"num_lanes_total", # 4 categories, but not tertile, derived 'num_lane' will be added
  "pedxwalk_signs",
  "school_zone_signs",
  "stop_signs_v", # SR 01
  #"stop_signs", #related signs
  "yield_signs",
  "total_st_signs",
  "bus_routes",
  "bus_stops", #added 
  "brt_routes",  #Y/N
  "bike_length",  #Y/N
  "traffic_fines_tot"
)

### yn---------
features_yn <-  c(
  #"trees",
  #"grade", std error huge
  #"area_median",
  "area_sidewalk",
  #"road_width", all >0, exclude
  # "road_marks", remove across board
  # "warning_signs",
  # "road_signs",
  # "traffic_lights",
  ## "st_dir", # not 1 and 0 , but will be added for iterating
  #"num_lanes_total",  all >0, exclude
  # "pedxwalk_signs",
  # "school_zone_signs",
  # "stop_signs_v",
  # "stop_signs",
  # "yield_signs",
  # "total_st_signs",
  # "bus_routes",
  "brt_routes",
  "bike_length"
  # "traffic_fines_tot"
)

### cont-----------
features_cont <-  c(
  "trees",
  "grade",
  "area_median",
  "area_sidewalk",
  "road_width",
  #"road_marks", warning - see below testing function
  #"warning_signs",  warning
  #"road_signs",  warning
  #"traffic_lights",  works, but coeff big (296), with exp huge
  #"st_dir",  only 1,2 directions
  #"num_lanes_total",  error about threshold
  #"pedxwalk_signs",  works, but coeff big (169), with exp huge
  #"school_zone_signs", huge coeff for collector
  #"stop_signs_v",   warning
  #"stop_signs",  warning
  #"yield_signs", works, but upper limit CI huge
  #"total_st_signs",  warning
  "bus_routes",
  #"brt_routes",  huge coeff for collector
  "bike_length"
  #"traffic_fines_tot" does not converge
)

## 2.2 Covar-----------

covar <- covar_100 %>%
  left_join(pop800, by = "CodigoCL") %>% 
  mutate(
    across(starts_with("pct"), ~ scale(.x)[,1])
    #removed pop_density, because offset by total pop
  ) %>% 
  left_join(ses_100, by = "CodigoCL") %>% 
  mutate(ses_cat_r = factor(ses_cat, levels = rev(levels(ses_cat)))) %>% 
  rename(codigocl = CodigoCL)   

## 2.3 functions to join by var type -----------
# calle_cont <- calle_rename_adj_df %>% 
#   dplyr::select(codigocl, all_of(features)) %>% 
#   left_join(collision_calle 
#     %>% rename(codigocl = CodigoCL), 
#     by = "codigocl") %>% 
#   mutate(
#     across(injury:total, ~replace_na(., 0))  #need to populate 0!
#   )

merge_calle_cont <- function(data){
  data2 <- data %>%
    dplyr::select(codigocl, all_of(features_cont)) %>%
    left_join(collision_calle
      %>% rename(codigocl = CodigoCL),
      by = "codigocl") %>%
    mutate(across(injury:total, ~ replace_na(., 0)))  #need to populate 0!
  
  data3 <- data2 %>% 
    left_join(covar, by = "codigocl")
  
  return(data3)
}

merge_calle_tertile <- function(data){
  data2 <-data %>%
    dplyr::select(codigocl, num_lanes_total, all_of(features_ter)) %>%
    mutate(
      across(all_of(features_ter), ~ na_if(., 0)),
      #turn to NA so that it doesn't get computed
      across(all_of(features_ter), ~ ntile(., 3), .names = "{.col}"),
      across(all_of(features_ter), ~ replace_na(., 0)),
      #turn it back 0
      across(-c(codigocl,num_lanes_total), ~ factor(., levels = c("1", "0", "2", "3"))),
      num_lane = case_when(
        num_lanes_total == 1 ~ "1",  #also numeric -> character
        num_lanes_total == 2 ~ "2",
        num_lanes_total %in% c(3, 4) ~ "3-4",
        num_lanes_total >= 5 ~ "5+"
      ),
      num_lane = factor(num_lane, levels = c("2", "1", "3-4", "5+"))
    ) %>% 
    left_join(collision_calle
      %>% rename(codigocl = CodigoCL),
      by = "codigocl") %>%
    mutate(across(injury:total, ~ replace_na(., 0)))
  
  data3 <- data2 %>% 
    left_join(covar, by = "codigocl")
  return(data3)
}

merge_calle_yn <- function(data){
  data2 <- data %>%
    dplyr::select(codigocl, st_dir, all_of(features_yn)) %>%
    mutate(across(all_of(features_yn), ~ if_else(. > 0, 1, 0)),
      across(-codigocl, ~ factor(.))) %>%
    left_join(collision_calle
      %>% rename(codigocl = CodigoCL),
      by = "codigocl") %>%
    mutate(across(injury:total, ~ replace_na(., 0)))
  
  data3 <- data2 %>% 
    left_join(covar, by = "codigocl")
  return(data3)
}

## choose one of below
### 2.3.1 join covar-cont ---------------
#assign features_cont
arterial_collision <- merge_calle_cont(arterial)
collector_collision <- merge_calle_cont(collector)
local_collision <- merge_calle_cont(local)
other_collision <- merge_calle_cont(other)

### 2.3.2 join covar-yn --------
#assign features_yn
arterial_collision <- merge_calle_yn(arterial)
collector_collision <- merge_calle_yn(collector)
local_collision <- merge_calle_yn(local)
other_collision <- merge_calle_yn(other)

### 2.3.3 join covar-ter-----------
arterial_collision <- merge_calle_tertile(arterial) %>% 
  filter(!grade == 0)  #there's only one
collector_collision <- merge_calle_tertile(collector)
local_collision <- merge_calle_tertile(local)
other_collision <- merge_calle_tertile(other)

# 3. Model: collision-feature ------------------
## test(no need to run)
fit_feature <- glm.nb(total ~trees + offset(log(pop_yr)) + pct_apt + pct_home + pct_unoccu + pct_male + pct_yr_0_9 + pct_yr_10_19 + pct_yr_30_39 + pct_yr_40_49 + pct_yr_50_59 + pct_yr_60_69 + pct_yr_70_79 + pct_yr_80_plus +ses_cat_r, data = local_collision)

summary(fit_feature)
##end

fit_feature_strat <- function(predictor, data){
  formula <- as.formula(paste("total ~", predictor, "+ offset(log(pop_yr)) + pct_apt + pct_home + pct_unoccu + pct_male + pct_yr_0_9 + pct_yr_10_19 + pct_yr_30_39 + pct_yr_40_49 + pct_yr_50_59 + pct_yr_60_69 + pct_yr_70_79 + pct_yr_80_plus + ses_cat_r"))
  
  model <- glm.nb(formula,  data = data)
  cli::cli_alert_success("modeling collision ~ {predictor} completed.")
  return(model)
}

test <- fit_feature_strat("grade", arterial_collision)
#features[[6,7, 8,10**, 13,14, 16]] as cont var
#Warning: fitted rates numerically 0 occurred
#**Error: no valid set of coefficients has been found: please supply starting values

summary(test)
df <- tidy(test)
## 3.1 Continous----------
### 3.1.1 arterial-----------
features <- features_cont

fit_allfea_art <- map(features, \(x) fit_feature_strat(x, data = arterial_collision))

feature_df <- map_df(fit_allfea_art,
  \(x) tidy(x, conf.int = TRUE, exponentiate = TRUE))

#### summarise RR -----------------------
art_cont_RR <- feature_df %>%
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

saveRDS(art_cont_RR, file = "gis_rdtype_stratified/art_cont_RR.rds")

### 3.1.2 collector-----------
fit_allfea_col <- map(features, \(x) fit_feature_strat(x, data = collector_collision))

feature_col_df <- map_df(fit_allfea_col,
  \(x) tidy(x, conf.int = TRUE, exponentiate = TRUE))

col_cont_RR <- feature_col_df %>% 
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

saveRDS(col_cont_RR, file = "gis_rdtype_stratified/col_cont_RR.rds")

### 3.1.3 local ---------
fit_allfea_loc <- map(features, \(x) fit_feature_strat(x, data = local_collision))
#didn't converge

### 3.1.4 other---------
fit_allfea_oth <- map(features, \(x) fit_feature_strat(x, data = other_collision))
#didn't converge and many more warnings


## 3.2 Tertile -----------
### 3.2.1 arterial--------
features <- c("num_lane", features_ter)

fit_allfea_art <- map(features, \(x) fit_feature_strat(x, data = arterial_collision))

feature_df <- map_df(fit_allfea_art,
  \(x) tidy(x, conf.int = TRUE, exponentiate = TRUE))

art_ter_RR <- feature_df %>%
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

saveRDS(art_ter_RR, file = "gis_rdtype_stratified/art_ter_RR.rds")

### 3.2.2 collector--------
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

saveRDS(col_ter_RR, file = "gis_rdtype_stratified/col_ter_RR.rds")

### 3.2.3 local-------
features <- features[!features =="yield_signs"]
# error in tidy() for yield sign
# SE huge, this usually can't be helped by removing covar
# yield sign has many zeros, tertile only 16 each

test <- fit_feature_strat("area_median", local_collision)
summary(test)

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

saveRDS(loc_ter_RR, file = "gis_rdtype_stratified/loc_ter_RR.rds")

### 3.2.4 other-----------
# NO NEED to RUN
# fit_allfea_oth <- map(features, \(x) fit_feature_strat(x, data = other_collision))
# 1 mil warnings

## try individual feature
test <- fit_feature_strat(features[[16]], other_collision)
summary(test)
# only total_st_signs works without warnings

## try leaving out covar (keep age compo cause warnings)
fit_feature_strat_sub <- function(predictor, data){
  formula <- as.formula(paste("total ~", predictor, "+ offset(log(pop_yr)) + pct_apt + pct_home + pct_unoccu + pct_male + ses_cat_r"))
  
  model <- glm.nb(formula,  data = data)
  cli::cli_alert_success("modeling collision ~ {predictor} completed.")
  return(model)
}

# test <- fit_feature_strat_sub(features[[1]], other_collision)
# summary(test)
# tidy(test, conf.int = TRUE, exponentiate = TRUE)

#good check--most of time it's just too few
other_collision %>% count(pedxwalk_signs) 

fit_oth <- fit_allfea_oth[-c(9, 10,12)]
#test again individually
tidy(fit_oth[[13]], conf.int = TRUE, exponentiate = TRUE)
#warnings: 6,8 9 (do not converge), not high SE
#stop_signs_v(ter size=27), traffic_lights(11), warning_signs(46)

#remove pct_male in the model helps with warning_signs converging
fit_feature_strat_sub2 <- function(predictor, data){
  formula <- as.formula(paste("total ~", predictor, "+ offset(log(pop_yr)) + pct_apt + pct_home + pct_unoccu + ses_cat_r"))
  
  model <- glm.nb(formula,  data = data)
  cli::cli_alert_success("modeling collision ~ {predictor} completed.")
  return(model)
}

# START RUNNING from here
# separately model warning_signs
warn_sign <- fit_feature_strat_sub2("warning_signs", other_collision)
warn_sign_df <- tidy(warn_sign, conf.int = TRUE, exponentiate = TRUE)

features <- features[!features %in% c(
  # high SE, sometimes running models no problem, get CI problematic
  "pedxwalk_signs", "school_zone_signs", "yield_signs", 
  # tidy()-CI, exponentite not converging, not high SE
  "stop_signs_v", "traffic_lights", "warning_signs")]


fit_allfea_oth <- map(features, \(x) fit_feature_strat_sub(x, data = other_collision))

feature_oth_df <-
  map_df(fit_allfea_oth, \(x) tidy(x, conf.int = TRUE, exponentiate = TRUE))

oth_ter_RR <- feature_oth_df %>% 
  bind_rows(warn_sign_df) %>% 
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

saveRDS(oth_ter_RR, file = "gis_rdtype_stratified/oth_ter_RR.rds")


## 3.3 Binary-----------
### 3.3.1 arterial-------------
features <- c("st_dir", features_yn)

fit_allfea_art <- map(features, \(x) fit_feature_strat(x, data = arterial_collision))

feature_df <- map_df(fit_allfea_art,
  \(x) tidy(x, conf.int = TRUE, exponentiate = TRUE))

art_yn_RR <- feature_df %>%
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

saveRDS(art_yn_RR, file = "gis_rdtype_stratified/art_yn_RR.rds")

### 3.3.2 collector-----------
fit_allfea_col <- map(features, \(x) fit_feature_strat(x, data = collector_collision))

feature_col_df <- map_df(fit_allfea_col,
  \(x) tidy(x, conf.int = TRUE, exponentiate = TRUE))

col_yn_RR <- feature_col_df %>% 
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

saveRDS(col_yn_RR, file = "gis_rdtype_stratified/col_yn_RR.rds")

### 3.3.3 local----------
fit_allfea_loc <- map(features, \(x) fit_feature_strat(x, data = local_collision))

feature_loc_df <- map_df(fit_allfea_loc,
  \(x) tidy(x, conf.int = TRUE, exponentiate = TRUE))

loc_yn_RR <- feature_loc_df %>% 
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

saveRDS(loc_yn_RR, file = "gis_rdtype_stratified/loc_yn_RR.rds")

### 3.3.4 other-------
# test <- fit_feature_strat_sub2(features[[4]], data = other_collision)
# tidy(test, conf.int = TRUE, exponentiate = TRUE)
#"bike_length" problematic with fit_feature_strat_sub(), alternation limit warning

bike <- fit_feature_strat_sub2("bike_length", data = other_collision)
bike_df <- tidy(bike, conf.int = TRUE, exponentiate = TRUE)

features <- features[-4]
fit_allfea_oth <- map(features, \(x) fit_feature_strat_sub(x, data = other_collision))

feature_oth_df <- map_df(fit_allfea_oth,
  \(x) tidy(x, conf.int = TRUE, exponentiate = TRUE))

oth_yn_RR <- feature_oth_df %>% 
  bind_rows(bike_df) %>% 
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

saveRDS(oth_yn_RR, file = "gis_rdtype_stratified/oth_yn_RR.rds")

