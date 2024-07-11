library(tidyverse)
library(sf)
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

# 2. Join collision - feature, cov---------------------
## 2.1 features-----------
### tertile-------
features <-  c(
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
features <-  c(
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

### cont-----------
features <-  c(
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
    dplyr::select(codigocl, all_of(features)) %>%
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
    dplyr::select(codigocl, st_dir, all_of(features)) %>%
    mutate(
      across(all_of(features), ~ na_if(., 0)),
      #turn to NA so that it doesn't get computed
      across(all_of(features), ~ ntile(., 3), .names = "{.col}"),
      across(all_of(features), ~ replace_na(., 0)),
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
    dplyr::select(codigocl, st_dir, all_of(features)) %>%
    mutate(across(all_of(features), ~ if_else(. > 0, 1, 0)),
      across(-codigocl, ~ factor(.))) %>%
    left_join(collision_calle
      %>% rename(codigocl = CodigoCL),
      by = "codigocl") %>%
    mutate(across(injury:total, ~ replace_na(., 0)))
  
  data3 <- data2 %>% 
    left_join(covar, by = "codigocl")
  return(data3)
}

## 2.3 join covar (for cont)---------------
arterial_collision <- merge_calle_cont(arterial)
# arterial_collision <- merge_calle_yn(arterial)
# arterial_collision <- merge_calle_tertile(arterial)

collector_collision <- merge_calle_cont(collector)
# collector_collision <- merge_calle_yn(collector)


local_collision <- merge_calle_cont(local)
# local_collision <- merge_calle_yn(local)

other_collision <- merge_calle_cont(other)

# 3. Model: collision-feature------------------
fit_feature <- glm.nb(death ~trees + pop_density + pct_male + pct_yr_0_9 + pct_yr_10_19 + pct_yr_30_39 + pct_yr_40_49 + pct_yr_50_59 + pct_yr_60_69 + pct_yr_70_79 + pct_yr_80_plus +ses_cat_r, data = arterial_collision)

fit_feature <- glm.nb(total ~trees + pct_apt + pct_home + pct_unoccu + pop_density + pct_male + pct_yr_0_9 + pct_yr_10_19 + pct_yr_30_39 + pct_yr_40_49 + pct_yr_50_59 + pct_yr_60_69 + pct_yr_70_79 + pct_yr_80_plus + +ses_cat_r, data = collector_collision)

fit_feature <- glm.nb(total ~trees + pct_apt + pct_home + pct_unoccu + pop_density + pct_male + pct_yr_0_9 + pct_yr_10_19 + pct_yr_30_39 + pct_yr_40_49 + pct_yr_50_59 + pct_yr_60_69 + pct_yr_70_79 + pct_yr_80_plus + +ses_cat_r, data = local_collision)

summary(fit_feature)

fit_feature_strat <- function(predictor, data){
  formula <- as.formula(paste("total ~", predictor, "+ pct_apt + pct_home + pct_unoccu + pop_density + pct_male + pct_yr_0_9 + pct_yr_10_19 + pct_yr_30_39 + pct_yr_40_49 + pct_yr_50_59 + pct_yr_60_69 + pct_yr_70_79 + pct_yr_80_plus"))
  
  model <- glm.nb(formula,  data = data)
  return(model)
}

test <- fit_feature_strat("pedxwalk_signs", arterial_collision)
#tertile features[[6,7, 8,10**, 13,14, 16]] as cont var
#Warning: fitted rates numerically 0 occurred
#**Error: no valid set of coefficients has been found: please supply starting values

summary(test)
df <- tidy(test)
## 3.1 Continous----------
### 3.1.1 arterial-----------
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