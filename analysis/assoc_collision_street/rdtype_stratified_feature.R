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
covar_100 <- readRDS("../../clean_data/SES/covar_calle100m.rds")
#road_type
road_type <- readRDS("../../clean_data/road_type/road_type_calle.rds")

ses_100 <- readRDS("../../clean_data/SES/ses_calle100m.rds")

# 1. feature stratify by road type -----------------
arterial <- road_type %>% 
  filter(road_type2 == "Arterial") %>% 
  filter(!CodigoCL == "CL100437") %>% 
  rename(codigocl = CodigoCL) %>% 
  left_join(calle_rename_adj_df, by = "codigocl")

collector <- road_type %>% 
  filter(road_type2 == "Collector") %>% 
  rename(codigocl = CodigoCL) %>% 
  left_join(calle_rename_adj_df, by = "codigocl")

local <- road_type %>% 
  filter(road_type2 == "Local") %>% 
  rename(codigocl = CodigoCL) %>% 
  left_join(calle_rename_adj_df, by = "codigocl")

other <- road_type %>% 
  filter(road_type2 == "Other") %>% 
  rename(codigocl = CodigoCL) %>% 
  left_join(calle_rename_adj_df, by = "codigocl")

## *descrp stat---------
#use features_ter + st_dir below
fea <- c("st_dir", features_ter)
feature_descrp <- function(data){
  data %>% 
    dplyr::select(all_of(fea)) %>% 
    pivot_longer(cols = everything(), names_to = "feature", values_to = "value") %>% 
    group_by(feature) %>% 
    mutate(
      zero = if_else(value ==0, 1, 0)
    ) %>% 
    drop_na() %>%  ##important, since it's long form can drop easily
    summarise(
      total = nrow(data),
      zero = sum(zero),
      median = median(value),
      mean = mean(value),
      sd = sd(value),
      IQR = IQR(value), 
      min = min(value),
      max = max(value)
    ) 
}
arterial_stat <- feature_descrp(arterial)
collector_stat <- feature_descrp(collector)
local_stat <- feature_descrp(local)
other_stat <- feature_descrp(other)

# 2. Prepare feature, cov---------------------
## 2.1 features-----------
### tertile-------
features_ter <-  c(
  "trees",
  "grade",
  "area_median",
  "area_sidewalk",
  "road_width",
  "road_marks",
  "warning_signs",
  "road_signs",
  "traffic_lights",
  #"st_dir",  only 1,2 directions
  "num_lanes_total",
  "pedxwalk_signs",
  "school_zone_signs",
  "stop_signs_v",
  "stop_signs",
  "yield_signs",
  "total_st_signs",
  "bus_routes",
  "brt_routes",
  "bike_length",
  "traffic_fines_tot"
)

### yn---------
features_yn <-  c(
  "trees",
  #"grade", std error huge
  "area_median",
  "area_sidewalk",
  #"road_width", all >0, exclude
  "road_marks",
  "warning_signs",
  "road_signs",
  "traffic_lights",
  # "st_dir", # not 1 and 0 , will be added for iterating
  #"num_lanes_total",  all >0, exclude
  "pedxwalk_signs",
  "school_zone_signs",
  "stop_signs_v",
  "stop_signs",
  "yield_signs",
  "total_st_signs",
  "bus_routes",
  "brt_routes",
  "bike_length",
  "traffic_fines_tot"
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

## 2.2 functions to join by var type -----------
# calle_cont <- calle_rename_adj_df %>% 
#   dplyr::select(codigocl, all_of(features)) %>% 
#   left_join(collision_calle 
#     %>% rename(codigocl = CodigoCL), 
#     by = "codigocl") %>% 
#   mutate(
#     across(injury:total, ~replace_na(., 0))  #need to populate 0!
#   )
### covar------------
covar <- covar_100 %>%
  mutate(
    across(c(pop_density, starts_with("pct")), ~ scale(.x)[,1])
  ) %>% 
  left_join(ses_100, by = "CodigoCL") %>% 
  mutate(ses_cat_r = factor(ses_cat, levels = rev(levels(ses_cat)))) %>% 
  rename(codigocl = CodigoCL) 

### merging-----------
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
    dplyr::select(codigocl, st_dir, all_of(features_ter)) %>%
    mutate(
      across(all_of(features_ter), ~ na_if(., 0)),
      #turn to NA so that it doesn't get computed
      across(all_of(features_ter), ~ ntile(., 3), .names = "{.col}"),
      across(all_of(features_ter), ~ replace_na(., 0)),
      #turn it back 0
      across(-codigocl, ~ factor(., levels = c("1", "0", "2", "3")))
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
## 2.3 join covar-cont ---------------
#assign features_cont
arterial_collision <- merge_calle_cont(arterial)
collector_collision <- merge_calle_cont(collector)
local_collision <- merge_calle_cont(local)
other_collision <- merge_calle_cont(other)

## 2.4 join covar-yn --------
#assign features_yn
arterial_collision <- merge_calle_yn(arterial)
collector_collision <- merge_calle_yn(collector)
local_collision <- merge_calle_yn(local)
other_collision <- merge_calle_yn(other)

## 2.5 join covar-ter-----------
arterial_collision <- merge_calle_tertile(arterial)
collector_collision <- merge_calle_tertile(collector)
local_collision <- merge_calle_tertile(local)
other_collision <- merge_calle_tertile(other)

# 3. Model: collision-feature ------------------
## test(no need to run)
fit_feature <- glm.nb(total ~trees + pct_apt + pct_home + pct_unoccu + pop_density + pct_male + pct_yr_0_9 + pct_yr_10_19 + pct_yr_30_39 + pct_yr_40_49 + pct_yr_50_59 + pct_yr_60_69 + pct_yr_70_79 + pct_yr_80_plus +ses_cat_r, data = local_collision)

summary(fit_feature)
##end

fit_feature_strat <- function(predictor, data){
  formula <- as.formula(paste("total ~", predictor, "+ pct_apt + pct_home + pct_unoccu + pop_density + pct_male + pct_yr_0_9 + pct_yr_10_19 + pct_yr_30_39 + pct_yr_40_49 + pct_yr_50_59 + pct_yr_60_69 + pct_yr_70_79 + pct_yr_80_plus + ses_cat_r"))
  
  model <- glm.nb(formula,  data = data)
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

## 3.2 Binary-----------
### 3.2.1 arterial-------------
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

### 3.2.2 collector-----------
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

### 3.2.3 local----------
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

### 3.2.4 other-------
fit_allfea_oth <- map(features, \(x) fit_feature_strat(x, data = other_collision))
#didn't converge and other warnings

## 3.3 Tertile -----------
### 3.3.1 arterial--------
features <- c("st_dir", features_ter)

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

### 3.3.2 collector--------
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

### 3.3.3 local-------
features <- features[!features == "yield_signs"]
# error in index 16 in tidy(), feature[[16]] is yield sign

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

### 3.3.4 other-----------
fit_allfea_oth <- map(features, \(x) fit_feature_strat(x, data = other_collision))
#did not converge