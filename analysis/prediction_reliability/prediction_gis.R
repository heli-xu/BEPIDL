library(tidyverse)
library(readr)
library(sf)
library(leaflet)
library(ggplot2)
library(caret)
library(pROC)
library(epiR)

sf_use_s2(FALSE)

# 0. import data -----------------------
gis_clean <- readRDS("../../clean_data/MLdata_GIS_CANVAS/gis_clean.rds")

#prediction by street
calle_predict24 <- readRDS("../../clean_data/predict24/calle_predict24.rds")

calle_geo <- readRDS("../../clean_data/calles/calle_shapefile.rds")

# 1. Year of image (optional) --------------
predict_200k <- read_csv("../../data/AI_prediction2024/predictions_200k.csv")

image_year <- predict_200k %>% 
  count(Date)


predict_sf <- st_as_sf(predict_200k, coords = c("Latitude", "Longitude"),  
  #googlemap api use lat first, but longitude is x, lat is y
  #note confusing column names
  crs = st_crs(4326)) 

ggplot()+
  geom_sf(data = predict_sf, color = "navy")+
  facet_wrap(~Date)+
  theme(axis.text = element_blank(),
        axis.ticks = element_blank())

# 2. Add prefix to prediction -------------------
calle_predict <- calle_predict24 %>% 
  rename_all(~paste0("pr_", .)) %>% 
  rename(codigocl = pr_CodigoCL) 

# 3. Join GIS to prediction ----------
pr_gis_calle <- gis_clean %>% 
  right_join(calle_predict, by = "codigocl") %>% 
  #check no NAs
  # select(-gis_vehicle_bridge, -gis_ped_bridge) %>% 
  # drop_na() %>% 
  mutate(
    gis_median = if_else(area_median > 0, 1, 0),
    gis_sw = if_else(area_sidewalk > 0, 1, 0),
    gis_bike_lane = if_else(bike_length > 0, 1, 0),
    gis_any_bus = if_else(gis_bus_lanes > 0, 1, 0),
    gis_brt_yn = if_else(brt_routes > 0, 1, 0),
    st_dir = case_when(
      sent_vial == "doble" ~ 0,
      sent_vial == "uno" ~ 1
    ),
    gis_veh_br = if_else(gis_vehicle_bridge == "vehicular", 1, 0, missing = 0),
    gis_ped_br = if_else(gis_ped_bridge == "Peatonal", 1, 0, missing = 0)
  ) %>% 
  mutate(
    across(c(pr_sign_traffic:pr_potholes, gis_trees, gis_bus_stops, 
             gis_road_signs, gis_traffic_lights, gis_road_signs_inv, 
             gis_stop_signs, gis_traffic_fines_tot, gis_lturn_sign, 
             gis_bike_signs, gis_bus_signs, gis_pedxwalk_signs, 
             gis_speed_bump_signs, gis_stop_signs2, gis_parking_signs, 
             gis_school_zone_signs, gis_yield_signs, gis_total_st_signs),
           ~if_else(.>0, 1, 0, missing = 0), .names = "{.col}_yn")
  ) %>% 
  mutate(
    any_ped = if_else(si_act_pea >0, 1, 0),
    pr_pedxwalk_yn = if_else(pr_sign_crossing_yn == 1 | pr_crosswalk_yn == 1, 
                             #pr_pedestrian_light not included (not sure if an_pedestrian is relevant)
                             true = 1, 
                             false = 0)
  ) %>% 
  mutate(
    across(c(gis_sw,
             gis_bike_lane,
             gis_any_bus,
             gis_median,
             ends_with("_yn")), ~factor(.))
  )

saveRDS(pr_gis_calle, file = "predict_gis_calle.rds")

# 4. Reliability metrics ------------------------------
pr_gis_calle <- readRDS("predict_gis_calle.rds")

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

## ROC -----------------
roc(pr_gis_calle$gis_brt_yn, as.numeric(pr_gis_calle$pr_brt_station_yn))

res <- map2(pr_variables, gis_variables, 
           \(x, y) roc(pr_gis_calle[[y]], as.numeric(pr_gis_calle[[x]])))

## ROC curve 
label <- str_sub(gis_variables, 5)
n <- c(1:length(gis_variables))
# colors <- c("#FF0000", "#FFA500", "#FF6347", "#008000", "#00FF00", "#00FFFF", "#0000FF", "#800080", "#FFC0CB", "#FF69B4", "#8B4513", "#FFD700", "#00CED1", "#483D8B", "#32CD32", "#800000", "#800080", "#2E8B57")
#https://colorswall.com/palette/

colors <- c("#800000", "#d62728","#FF69B4", "#f7b6d2", "#9c27b0", "#673ab7", "#6c579d","#3f51b5", "#0000FF", "#2196f3", "#03a9f4", "#00bcd4", "#009688", "#008000", "#8bc34a", "#ffc107", "#ff9800", "#ff5722")

plot(res[[1]])
map2(n, colors, \(x, y) plot(res[[x]], col = y, add = T))
legend("bottomright", legend = label, col = colors, lty = 1, lwd = 3, cex =0.8, text.font = 2, bty = "n")
title(main = "Prediction2024-GIS comparison", line = 3)

## Summary table -------------------------------------
source("../../functions/reliability_table.R")

df <- reliability_table(pr_variables, gis_variables, pr_gis_calle)
# 
# epi.kappa(m2,
#           method = "cohen")
# 
# m <- table(pr_gis_calle[["pr_traffic_light_yn"]], pr_gis_calle$gis_traffic_lights_yn)
# 
# #class(m) <- "numeric"
# m2 <- apply(m, c(1, 2), as.numeric)
# 
# epi.kappa(table(pr_gis_calle[["pr_sign_traffic_yn"]], pr_gis_calle$gis_road_signs_inv_yn),
#           method = "cohen")
# 
# confusionMatrix(pr_gis_calle[["pr_traffic_light_yn"]], pr_gis_calle$gis_traffic_lights_yn)

saveRDS(df, file = "predict24_gis_reliability.rds")
write_csv(df, file = "predict24_gis_reliability_allyears.csv")

## *Visualization ------------

# colors <- c("#800000", "#d62728","#FF69B4", "#f7b6d2", "#9c27b0", "#673ab7", "#6c579d","#3f51b5", "#0000FF", "#2196f3", "#03a9f4", "#00bcd4", "#009688", "#008000", "#8bc34a", "#ffc107", "#ff9800", "#ff5722")

plot_kappa(df) +
  labs(
    title = "Built Environment Features: Prediction2024 vs GIS",
    subtitle = "Agreement between AI predictions and GIS data (n = ~63k) at the street level",
    caption = "Interpretations for the kappa statistic: < 0.2 slight agreement, \n0.2 - 0.4 fair agreement, 0.4 - 0.6 moderate agreement.",
    x = "Cohen's kappa (95%CI)",
    y = "Street Features"
  )

# 5. Reliability by year group ---------------------
## 5.1 Split data by year group---------
g1 <- c(2012:2015)
g2 <- c(2016:2019)
g3 <- c(2020:2023)

predict_g1 <- predict_200k %>% 
  filter(Date %in% g1)

predict_g2 <- predict_200k %>% 
  filter(Date %in% g2)

predict_g3 <- predict_200k %>% 
  filter(Date %in% g3)

## 5.2 Aggr image to street-----------------

img_to_st <- function(data){
  st_data <- st_as_sf(data,
    coords = c("Latitude", "Longitude"),
    #googlemap api use lat first, but longitude is x, lat is y
    #note confusing column names
    crs = st_crs(4326)) %>%
    st_transform(crs = st_crs(calle_geo)) %>%
    st_join(calle_geo, join = st_within) %>% 
    drop_na(CodigoCL) %>% 
    st_drop_geometry() %>% 
    rename_all(~paste0("pr_", .)) %>% 
    rename(CodigoCL = pr_CodigoCL) %>% 
    group_by(CodigoCL) %>% 
    summarise(
      across(pr_Sign_traffic:pr_Potholes, ~sum(.x), .names = "{.col}")
  ) %>% 
    rename_with(~ tolower(.x))
  return(st_data)
}

g1_st <- img_to_st(predict_g1)

g2_st <- img_to_st(predict_g2)

g3_st <- img_to_st(predict_g3)

## 5.3 link GIS ----------
#input data from last step
link_gis <- function(data){
  pr_gis_calle <- gis_clean %>%
    right_join(data, by = "codigocl") %>% #check reference gis are not NA
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
  return(pr_gis_calle)
}

pr_gis_g1 <- link_gis(g1_st)

pr_gis_g2 <- link_gis(g2_st)

pr_gis_g3 <- link_gis(g3_st)

## 5.4 Reliability Metrics------
### ROC (not necessary)
res_g1 <- map2(pr_variables, gis_variables, 
            \(x, y) roc(pr_gis_g1[[y]], as.numeric(pr_gis_g1[[x]])))

### summary table
source("../../functions/reliability_table.R")

df_g1 <- reliability_table(pr_variables, gis_variables, pr_gis_g1) %>% 
  mutate(year = "2012-2015")

df_g2 <- reliability_table(pr_variables, gis_variables, pr_gis_g2) %>% 
  mutate(year = "2016-2019")

df_g3 <- reliability_table(pr_variables, gis_variables, pr_gis_g3) %>% 
  mutate(year = "2020-2023")

df_all <- bind_rows(df_g1, df_g2, df_g3)


## 5.5 Visualization---------------
source("../../functions/plot_kappa.R")

plot_kappa(df_all)+
  geom_point(aes(x = kappa_est, color = var), size = 2)+
  scale_color_manual(values = colors)+
  facet_grid(vars(year), switch = "y")+
  labs(
    title = "Prediction2024 vs GIS by Year Groups",
    subtitle = "Agreement between AI predictions and GIS data at the street level",
    caption = "Interpretations for the kappa statistic: < 0.2 slight agreement, \n0.2 - 0.4 fair agreement, 0.4 - 0.6 moderate agreement.",
    x = "Cohen's kappa (95%CI)",
    y = "Variables"
  )+
  theme(
    legend.position = "none"
  )

