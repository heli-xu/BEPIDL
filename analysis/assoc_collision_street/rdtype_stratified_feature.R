library(tidyverse)
library(sf)
library(MASS)
library(broom)

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
  data %>%
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
}

merge_calle_yn <- function(data){
  data %>%
    dplyr::select(codigocl, st_dir, all_of(features)) %>%
    mutate(across(all_of(features), ~ if_else(. > 0, 1, 0)),
      across(-codigocl, ~ factor(.))) %>%
    left_join(collision_calle
      %>% rename(codigocl = CodigoCL),
      by = "codigocl") %>%
    mutate(across(injury:total, ~ replace_na(., 0)))
}

## 2.3 join covar (for cont)---------------
calle_cont <- merge_calle_cont(arterial)

covar <- covar_100 %>%
  mutate(
    across(c(pop_density, starts_with("pct")), ~ scale(.x)[,1])
  ) %>% 
  left_join(ses_100, by = "CodigoCL") %>% 
  mutate(ses_cat_r = factor(ses_cat, levels = rev(levels(ses_cat)))) %>% 
  rename(codigocl = CodigoCL) 

calle_cont %>% 
  left_join(covar, by = "codigocl")
