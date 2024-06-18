library(tidyverse)
library(sf)
library(MASS)
library(broom)

# 0. import data----------------
calle_rename_df <- readRDS("../../clean_data/calles/calle_rename_df.rds")
#covariates
covar_100 <- readRDS("../../clean_data/SES/covar_calle100m.rds")
#road_type
road_type <- readRDS("../../clean_data/road_type/road_type_calle.rds")

# 1. collision~feature+cov+rd_type------------

## 3.1 Prepare features---------------------
### features to tertile
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

#features to iterate
all_features <-  c(
  "trees",
  "grade",
  "area_median",
  "area_sidewalk",
  "road_width",
  "road_marks",
  "warning_signs",
  "road_signs",
  "traffic_lights",
  "st_dir",
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

### Zeros, nonzero Tertiles, factored------
calle_tertile <- calle_rename_df %>% 
  dplyr::select(codigocl, st_dir, all_of(features), ped_collision) %>% 
  mutate(
    across(all_of(features), ~na_if(., 0)), #turn to NA so that it doesn't get computed
    across(all_of(features), ~ntile(., 3), .names = "{.col}"),
    across(all_of(features), ~replace_na(., 0)), #turn it back 0
    across(-c(codigocl, ped_collision), ~factor(., levels = c("1", "0", "2", "3")))
  )

levels(calle_tertile$trees)

### *rd_type + 100m buffer covar --------------
collision_covar_rd_100 <- covar_100 %>%
  mutate(
    across(c(pop_density, starts_with("pct")), ~ scale(.x)[,1])
  ) %>% 
  left_join(road_type, by = "CodigoCL") %>% 
  mutate(road_type2 = factor(road_type2, levels = c("Local", "Arterial", "Collector", "Other"))) %>% 
  rename(codigocl = CodigoCL) %>% 
  left_join(calle_tertile, by = "codigocl") %>% 
  drop_na(road_type2)


## 3.2 Model ---------------
### function -------------
fit_feature <- glm.nb(ped_collision ~ area_median + pct_apt + pct_home + pct_unoccu + pop_density + pct_male + pct_yr_0_9 + pct_yr_10_19 + pct_yr_30_39 + pct_yr_40_49 + pct_yr_50_59 + pct_yr_60_69 + pct_yr_70_79 + pct_yr_80_plus + road_type2, data = collision_covar_rd_100)

summary(fit_feature)

fit_features_x <- function(predictor, data){
  formula <- as.formula(paste("ped_collision ~", predictor, "+ pct_apt + pct_home + pct_unoccu + pop_density + pct_male + pct_yr_0_9 + pct_yr_10_19 + pct_yr_30_39 + pct_yr_40_49 + pct_yr_50_59 + pct_yr_60_69 + pct_yr_70_79 + pct_yr_80_plus + road_type2"))
  
  model <- glm.nb(formula,  data = data)
  return(model)
}
  
test <- fit_features_x("area_median", collision_covar_rd_100)
summary(test)

### interate -------------
fit_allfeatures <- map(all_features, \(x) fit_features_x(x, data = collision_covar_rd_100))

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
    ),
    category = case_match(
      term,
      "st_dir2" ~ "Double",
      .default = category
    )
  ) 

saveRDS(feature_RR, file = "st_feature_cov100_rdtype_RR.rds")
 
## 3.4 visualize --------------
source("../../functions/plot_facet_RR.R")
feature_covar100 <- readRDS("st_feature_covar100_RR.rds")

feature_RR %>% 
  filter(!term == "(Intercept)",
    !category == "(Covariates)") %>% 
  plot_facet_RR()+
  labs(
    subtitle = "Adjusted for types of dwellings, population density, age groups, sex composition \nand road types (100m)"
  )+
  theme(
    plot.title.position = "plot"
  )

# subset by road_type----------------
  arterial <- collision_covar_rd_100 %>% 
    filter(road_type == "Arterial")
  
  collector <- collision_covar_rd_100 %>% 
    filter(road_type == "Collector") 
  
  local <- collision_covar_rd_100 %>% 
    filter(road_type == "Local")
  
  other <- collision_covar_rd_100 %>% 
    filter(road_type == "Other")
  
  
  
  