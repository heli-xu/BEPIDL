library(tidyverse)
library(sf)
library(ggplot2)
library(caret)
library(pROC)
library(epiR)

sf_use_s2(FALSE)

# 0. import data -----------------------
gis_clean <- readRDS("../../clean_data/MLdata_GIS_CANVAS/gis_clean.rds")

#updated data Nov2024#
#prediction by street using different threshold
calle_predict24mean <- readRDS("../../clean_data/predict24/threshold_testing/calle_predict24_1519mean.rds")
calle_predict24median <- readRDS("../../clean_data/predict24/threshold_testing/calle_predict24_1519median.rds")
calle_predict24mode <- readRDS("../../clean_data/predict24/threshold_testing/calle_predict24_1519mode.rds")
calle_predict24q25 <- readRDS("../../clean_data/predict24/threshold_testing/calle_predict24_1519q25.rds")
calle_predict24q75 <- readRDS("../../clean_data/predict24/threshold_testing/calle_predict24_1519q75.rds")
#no threshold
calle_predict312k <- readRDS("../../clean_data/predict24/predict_312k/calle_predict312k_1519.rds")


calle_geo <- readRDS("../../clean_data/calles/calle_shapefile.rds")

# 1. Make function to link GIS -----------------
#(similar to checking reliability by year group in 'prediction_gis.R')

thr_link_gis <- function(data){
  calle_predict <- data %>% 
    rename_all(~paste0("pr_", .)) %>% 
    rename(codigocl = pr_CodigoCL) 
  cli::cli_alert_success("Added prefix to predictions.")
  
  pr_gis_calle <- gis_clean %>%
    right_join(calle_predict, by = "codigocl") %>% #check reference gis are not NA
    mutate(
      gis_median = if_else(area_median > 0, 1, 0),
      gis_sw = if_else(area_sidewalk > 0, 1, 0),
      gis_bike_lane = if_else(bike_length > 0, 1, 0),
      gis_any_bus = if_else(gis_bus_lanes > 0, 1, 0),
      gis_brt_yn = if_else(brt_routes > 0, 1, 0),
      st_dir = case_when(sent_vial == "doble" ~ 0,
        sent_vial == "uno" ~ 1),
      gis_veh_br = if_else(gis_vehicle_bridge == "vehicular", 1, 0, missing = 0),
      gis_ped_br = if_else(gis_ped_bridge == "Peatonal", 1, 0, missing = 0)
    ) %>%
    mutate(across(
      c(
        pr_sign_traffic:pr_potholes,
        gis_trees,
        gis_bus_stops,
        gis_road_signs,
        gis_traffic_lights,
        gis_road_signs_inv,
        gis_stop_signs,
        gis_traffic_fines_tot,
        gis_lturn_sign,
        gis_bike_signs,
        gis_bus_signs,
        gis_pedxwalk_signs,
        gis_speed_bump_signs,
        gis_stop_signs2,
        gis_parking_signs,
        gis_school_zone_signs,
        gis_yield_signs,
        gis_total_st_signs
      ),
      ~ if_else(. > 0, 1, 0, missing = 0),
      .names = "{.col}_yn"
    )) %>%
    mutate(
      any_ped = if_else(si_act_pea > 0, 1, 0),
      pr_pedxwalk_yn = if_else(
        pr_sign_crossing_yn == 1 | pr_crosswalk_yn == 1,
        #pr_pedestrian_light not included (not sure if an_pedestrian is relevant)
        true = 1,
        false = 0
      )
    ) %>%
    mutate(across(
      c(
        gis_sw,
        gis_bike_lane,
        gis_any_bus,
        gis_median,
        ends_with("_yn")
      ),
      ~ factor(.)
    ))
  
  cli::cli_alert_success("Joined GIS to predictions.")
  return(pr_gis_calle)
  
} 


# 2. Reliability by different threshold -----------------
## 2.1 iterate ---------
pg_mean <- thr_link_gis(calle_predict24mean)

pg_median <- thr_link_gis(calle_predict24median)

pg_mode <- thr_link_gis(calle_predict24mode)

pg_q25 <- thr_link_gis(calle_predict24q25)

pg_q75 <- thr_link_gis(calle_predict24q75)

pg_all <- thr_link_gis(calle_predict312k)

## 2.2 set variables ---------
pr_variables <- c(
  "pr_sign_traffic_yn",
  "pr_sign_traffic_yn",
  "pr_traffic_light_yn",
  "pr_pedxwalk_yn",
  "pr_sign_stop_yn",
  "pr_sign_stop_yn",
  "pr_sign_yield_yn",
  "pr_sign_school_zone_yn",
  "pr_sidewalk_yn",
  "pr_lane_bike_yn",
  "pr_lane_bus_yn",
  "pr_median_yn",
  "pr_speed_bump_yn",
  "pr_trees_yn",
  "pr_bus_stop_yn",
  "pr_parked_vehicles_yn",
  "pr_lane_parking_yn",
  "pr_brt_station_yn"
)

# Variables starting with "gis_"
gis_variables <- c(
  "gis_road_signs_inv_yn",
  "gis_total_st_signs_yn",
  "gis_traffic_lights_yn",
  "gis_pedxwalk_signs_yn",
  "gis_stop_signs_yn",
  "gis_stop_signs2_yn",
  "gis_yield_signs_yn",
  "gis_school_zone_signs_yn",
  "gis_sw",
  "gis_bike_lane",
  "gis_any_bus",
  "gis_median",
  "gis_speed_bump_signs_yn",#noted change
  "gis_trees_yn",
  "gis_bus_stops_yn",
  "gis_parking_signs_yn",
  "gis_parking_signs_yn",
  "gis_brt_yn"
)


## 2.3 Summary table -------------------------------------
source("../../functions/reliability_table.R")

df_mean <- reliability_table(pr_variables, gis_variables, pg_mean) |> 
  mutate(threshold = "mean")
df_median <- reliability_table(pr_variables, gis_variables, pg_median) |> 
  mutate(threshold = "median")
df_mode <- reliability_table(pr_variables, gis_variables, pg_mode) |> 
  mutate(threshold = "mode")
df_q25 <- reliability_table(pr_variables, gis_variables, pg_q25) |> 
  mutate(threshold = "q25")
df_q75 <- reliability_table(pr_variables, gis_variables, pg_q75) |> 
  mutate(threshold = "q75")
df_keep <- reliability_table(pr_variables, gis_variables, pg_all) |> 
  mutate(threshold = "keep all")

df_all <- bind_rows(df_mean, df_median, df_mode, df_q25, df_q75, df_keep)

write_csv(df_all, file = "reliability_by_filtering_threshold.csv")

## 2.4 Visualization---------------
source("../../functions/plot_kappa.R")

colors <- c("#800000", "#d62728","#FF69B4", "#f7b6d2", "#9c27b0", "#673ab7", "#6c579d","#3f51b5", "#0000FF", "#2196f3", "#03a9f4", "#00bcd4", "#009688", "#008000", "#8bc34a", "#ffc107", "#ff9800", "#ff5722")

plot_kappa(df_all)+
  geom_point(aes(x = kappa_est, color = threshold), size = 2)+
  scale_color_manual(values = colors)+
  facet_wrap(~threshold)+
  labs(
    title = "Prediction2024 vs GIS by Different Filtering Thresholds",
    subtitle = "Agreement between AI predictions and GIS data at the street level",
    caption = "Interpretations for the kappa statistic: < 0.2 slight agreement, \n0.2 - 0.4 fair agreement, 0.4 - 0.6 moderate agreement.",
    x = "Cohen's kappa (95%CI)",
    y = "Variables"
  )+
  theme(
    legend.position = "none"
  )

