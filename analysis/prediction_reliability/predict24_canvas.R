library(tidyverse)
library(readr)
library(sf)
library(leaflet)
library(ggplot2)
library(caret)
library(pROC)
library(epiR)

# 0. import data -----------------------
canvas_calle <- readRDS("../../clean_data/MLdata_GIS_CANVAS/calle_canvas_sf.rds")
predict_200k <- read_csv("../../data/AI_prediction2024/predictions_200k.csv")

calle_geo <- readRDS("../../clean_data/calles/calle_shapefile.rds")

# 1. Prediction: img to street ------------------

calle_predict <- img_to_st(predict_200k)

calle_predict_yn <- calle_predict %>% 
  mutate(
    across(c(pr_sign_traffic:pr_potholes),
      ~if_else(.>0, 1, 0, missing = 0), .names = "{.col}_yn"),
    pr_pedxwalk_yn = if_else(pr_sign_crossing_yn == 1 | pr_crosswalk_yn == 1, 
      #pr_pedestrian_light not included 
      true = 1, 
      false = 0)
  ) 

# 2. Join prediction with canvas---------------

predict_canvas <- canvas_calle %>% 
  st_drop_geometry() %>% 
  rename(codigocl = CodigoCL) %>% 
  rename_with(~paste0("can_", .), tree_yn:transit_lane_yn) %>% 
  left_join(calle_predict_yn, by = "codigocl") %>% 
  mutate(
    across(ends_with("_yn"), ~factor(.))
  )

saveRDS(predict_canvas, file = "predict24_canvas.rds")

# 3. Reliability metrics-------------------
can_variables <- c(
  "can_traffic_light_yn",
  "can_crosswalk_yn",
  "can_stop_sign_yn", 
  # "can_stop_sign_yn", 
  "can_school_zone_yn",
  "can_sidewalk_yn", 
  "can_bike_lane_yn", 
  "can_transit_lane_yn",
  "can_median_yn", 
  "can_speed_bump_yn", 
  "can_tree_yn"
  # "can_roundabout_yn"
)

pr_variables <- c(
  # "pr_sign_traffic_yn",
  # "pr_sign_traffic_yn",
  "pr_traffic_light_yn",
  "pr_pedxwalk_yn",
  "pr_sign_stop_yn",
  # "pr_sign_stop_yn",
  # "pr_sign_yield_yn",
  "pr_sign_school_zone_yn",
  "pr_sidewalk_yn",
  "pr_lane_bike_yn",
  "pr_lane_bus_yn",
  "pr_median_yn",
  "pr_speed_bump_yn",
  "pr_trees_yn"
  # "pr_bus_stop_yn",
  # "pr_parked_vehicles_yn",
  # "pr_lane_parking_yn",
  # "pr_brt_station_yn"
)

source("../../functions/reliability_table.R")

df <- reliability_table(pr_variables, can_variables, predict_canvas)

df %>%
  mutate(var_plot = str_sub(var, 4, -4),
    var_plot = fct_reorder(var_plot, kappa_est)) %>%
  ggplot(aes(x = kappa_est, y = var_plot))+
  geom_errorbar(aes(xmin = kappa_lower, xmax = kappa_upper), linewidth = 0.5)+
  geom_point(aes(x = kappa_est), size = 2)+
  geom_vline(aes(xintercept = 0.2), linetype = 2)+
  scale_x_continuous(breaks = sort(round(c(seq(min(df$kappa_lower), max(df$kappa_upper), length.out = 4), 0.2), 1)))+
  theme_bw()+
  # facet_grid(vars(year), switch = "y")+
  theme(
    plot.title = element_text(size = 13, face = "bold", hjust = 0),
    plot.title.position = "plot",
    text = element_text(size = 11),
    axis.title = element_text(size = 12),
    # plot.title.position = "plot",
    panel.spacing.y = unit(0, "points"),
    panel.border = element_blank(),
    #axis.text.y = element_blank(),
    axis.ticks.length.y = unit(0, "points"),
    strip.text.y.left = element_text(face = "bold", angle = 0),
    #strip.background.y = element_blank(),
    strip.placement = "outside",
    axis.line = element_line()
  )+  
  #below are outside of function
  labs(
    title = "Built Environment Features: Prediction2024 vs CANVAS",
    subtitle = "Agreement between AI predictions and CANVAS data (n =350) at the street level",
    caption = "Interpretations for the kappa statistic: < 0.2 slight agreement, \n0.2 - 0.4 fair agreement, 0.4 - 0.6 moderate agreement.",
    x = "Cohen's kappa (95%CI)",
    y = "Street Features"
  )

plot_kappa(df) +
  labs(
    title = "Built Environment Features: Prediction2024 vs CANVAS",
    subtitle = "Agreement between AI predictions and CANVAS data (n =350) at the street level",
    caption = "Interpretations for the kappa statistic: < 0.2 slight agreement, \n0.2 - 0.4 fair agreement, 0.4 - 0.6 moderate agreement.",
    x = "Cohen's kappa (95%CI)",
    y = "Street Features"
  )
  