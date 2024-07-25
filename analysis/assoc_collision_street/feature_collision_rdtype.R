library(tidyverse)
library(sf)
library(MASS)
library(broom)

# collision~feature+cov+rd_type------------
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

#population from 800m buffer
pop800 <- readRDS("../../clean_data/covar_mzn/pop_calle800m.rds") %>% 
  mutate(pop_yr = pop *5)  #2015-2019



#1. Set features ---------------------
## features to tertile---------------
features_ter <-  c(
  "trees",
  "grade",
  "area_median",
  #"area_sidewalk",  #Y/N
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
  #"brt_routes",  Y/N
  #"bike_length",  Y/N
  "traffic_fines_tot"
)

## YN---------
features_yn <-  c(
  "trees", # just to see
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
  "total_st_signs", # just to see
  # "bus_routes",
  "bus_stops", #added 
  "brt_routes",
  "bike_length"
  # "traffic_fines_tot"
)

#  2. Join feature-collision----------
## 2.1 Continuous var (adjusted counts)-----------
calle_cont <- calle_rename_adj_df %>% 
  dplyr::select(codigocl, all_of(features)) %>% 
  left_join(collision_calle 
    %>% rename(codigocl = CodigoCL), 
    by = "codigocl") %>% 
  mutate(
    across(injury:total, ~replace_na(., 0))  #need to populate 0!
  )

## 2.2 Zeros, nonzero Tertiles, factored------
calle_tertile <- calle_rename_adj_df %>% 
  dplyr::select(codigocl, num_lanes_total, all_of(features_ter)) %>% #add num_lanes_total
  mutate(
    across(all_of(features_ter), ~na_if(., 0)), #turn to NA so that it doesn't get computed
    across(all_of(features_ter), ~ntile(., 3), .names = "{.col}"),
    across(all_of(features_ter), ~replace_na(., 0)), #turn it back 0
    across(all_of(features_ter), ~factor(., levels = c("1", "0", "2", "3"))),
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
  mutate(
    across(injury:total, ~replace_na(., 0))
  )

## 2.3 YN-Binary ------------------
calle_yn <- calle_rename_adj_df %>% 
  dplyr::select(codigocl, st_dir, all_of(features_yn)) %>%
  mutate(
    across(all_of(features_yn), ~if_else(. >0, 1, 0)),
    across(-codigocl, ~factor(.))
  ) %>% 
  left_join(collision_calle 
    %>% rename(codigocl = CodigoCL), 
    by = "codigocl") %>% 
  mutate(
    across(injury:total, ~replace_na(., 0))
  )

levels(calle_yn$trees)

# 3. Join Covar w rd_type + SES--------------
## 3.1 tertile-----------
collision_covar_rd_100 <- covar_100 %>%
  left_join(pop800, by = "CodigoCL") %>% 
  mutate(
    across(starts_with("pct"), ~ scale(.x)[,1])
  ) %>% 
  left_join(ses_100, by = "CodigoCL") %>% 
  mutate(ses_cat_r = factor(ses_cat, levels = rev(levels(ses_cat)))) %>% 
  left_join(road_type, by = "CodigoCL") %>% 
  mutate(road_type2 = factor(road_type2, levels = c("Local", "Arterial", "Collector", "Other"))) %>% 
  rename(codigocl = CodigoCL) %>% 
  left_join(calle_tertile, by = "codigocl") %>% 
  drop_na(road_type2)

## 3.2 YN-------------
collision_covar_rd_yn <- covar_100 %>%
  left_join(pop800, by = "CodigoCL") %>% 
  mutate(
    across(starts_with("pct"), ~ scale(.x)[,1])
  ) %>% 
  left_join(ses_100, by = "CodigoCL") %>% 
  mutate(ses_cat_r = factor(ses_cat, levels = rev(levels(ses_cat)))) %>% 
  left_join(road_type, by = "CodigoCL") %>% 
  mutate(road_type2 = factor(road_type2, levels = c("Local", "Arterial", "Collector", "Other"))) %>% 
  rename(codigocl = CodigoCL) %>% 
  left_join(calle_yn, by = "codigocl") %>% 
  drop_na(road_type2)


# 4. Model ---------------
## function -------------
fit_feature <- glm.nb(total ~trees + offset(log(pop_yr)) + pct_apt + pct_home + pct_unoccu + pct_male + pct_yr_0_9 + pct_yr_10_19 + pct_yr_30_39 + pct_yr_40_49 + pct_yr_50_59 + pct_yr_60_69 + pct_yr_70_79 + pct_yr_80_plus + road_type2 + ses_cat_r, data = collision_covar_rd_100) #remove pop_density!

summary(fit_feature)

df <- tidy(fit_feature, conf.int = TRUE, exponentiate = TRUE)
df_RR <- df %>% 
  mutate(
    RR_95CI = paste0(round(estimate,2)," (", round(conf.low,2), ",", round(conf.high, 2), ")")
  )

fit_features_x <- function(predictor, data){
  formula <- as.formula(paste("total ~", predictor, "+ offset(log(pop_yr)) + pct_apt + pct_home + pct_unoccu + pct_male + pct_yr_0_9 + pct_yr_10_19 + pct_yr_30_39 + pct_yr_40_49 + pct_yr_50_59 + pct_yr_60_69 + pct_yr_70_79 + pct_yr_80_plus + road_type2 + ses_cat_r"))
  
  model <- glm.nb(formula,  data = data)
  cli::cli_alert_success("modeling collision ~ {predictor} completed.")
  return(model)
}
  
test <- fit_features_x("area_median", collision_covar_rd_100)

summary(test)

## 4.1 Tertiles -------------
features <- c("num_lane", features_ter)

fit_allfeatures <- map(features, \(x) fit_features_x(x, data = collision_covar_rd_100))

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

saveRDS(feature_RR, file = "st_feature_cov100_rdtype_RR.rds")
 
### visualize --------------
source("../../functions/plot_facet_RR.R")

fea_plot_RR <- feature_RR %>% 
  mutate(
    #note the modification for num_lane
    tertile = if_else(term %in% c("num_lane1","num_lane3-4", "num_lane5+"), 
      str_sub(term, 9), str_sub(term,-1)),
    predictor = case_when(
      term %in% c("num_lane1","num_lane3-4", "num_lane5+") ~ "num_lane",
      .default = str_sub(term, end = -2)
    )) %>%
  filter(predictor %in% features) %>%
  mutate(
    category = case_match(tertile,
    ")" ~ "Low (ref)",
    "0" ~ "Zero",
    "2" ~ "Medium",
    "3" ~ "High",
    .default = tertile)
    )

fea_plot_RR %>% 
  plot_facet_RR()+
  labs(
    subtitle = "Offset by population within 800m from each street. Adjusted for types of dwellings,\nage groups, sex composition, road types and SES within 100m from streets.",
    caption = "For 'num_lane', the comparison is relative to 2 lanes. \nFor other street features, data are separated into zeros and nonzero tertiles for analysis.\n Comparisons are relative to the 'Low' category."
  )+
  theme(
    plot.title.position = "plot"
  )


## 4.2 YN ---------------
features <- c("st_dir", features_yn)

fit_allfeatures <- map(features, \(x) fit_features_x(x, data = collision_covar_rd_yn))

feature_yn_df <- map_df(fit_allfeatures,
  \(x) tidy(x, conf.int = TRUE, exponentiate = TRUE))
#took 20min or something  

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

saveRDS(feature_RR, file = "st_feature_cov100_rdtype_RR.rds")

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
    caption ="'st_dir' is included as 1 and 2 directions, relative to 1. \nOther street features are considered yes/no for analysis. Comparisons are relative to the 'no' category."
  )+
  theme(
    plot.title.position = "plot"
  )

  
  
  