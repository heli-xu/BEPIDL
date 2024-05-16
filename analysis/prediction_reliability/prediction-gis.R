library(tidyverse)
library(readr)
library(sf)
library(leaflet)
library(ggplot2)
library(caret)
library(pROC)
library(epiR)

# 0. import data -----------------------
gis_clean <- readRDS("../../clean_data/MLdata_GIS_CANVAS/gis_clean.rds")
predict_200k <- read_csv("../../data/AI_prediction2024/predictions_200k.csv")

calle_geo <- readRDS("../../clean_data/calles/calle_shapefile.rds")

# 1. Link image to street ------------------
predict_sf <- st_as_sf(predict_200k, coords = c("Latitude", "Longitude"),  
                        #googlemap api use lat first, but longitude is x, lat is y
                        #note confusing column names
                        crs = st_crs(4326)) 

predict_sf %>% 
  st_transform(crs = st_crs("+proj=longlat +datum=WGS84")) %>% 
  leaflet() %>% 
    addTiles() %>% 
    addCircles(
      radius =0.5,
      color = "#D37506",
      opacity = 0.6)


## Year of image --------------
image_year <- predict_200k %>% 
  count(Date)

ggplot()+
  geom_sf(data = predict_sf, color = "navy")+
  facet_wrap(~Date)+
  theme(axis.text = element_blank(),
        axis.ticks = element_blank())

# 2. Aggregate to street ---------
calle_predict_sf <- predict_sf %>% 
  st_transform(crs = st_crs(calle_geo)) %>% 
  st_join(calle_geo,join= st_within) 

## unmatched points ------------------
#before drop_na()
leaflet() %>% 
  addTiles() %>% 
  addCircles(
    data = calle_predict_sf %>% 
      filter(is.na(CodigoCL)) %>% 
      st_transform(crs = st_crs("+proj=longlat +datum=WGS84")),
    radius =0.5,
    color = "purple",
    opacity = 0.6) %>% 
  addPolygons(
    data = calle_geo %>% 
      st_transform(crs = st_crs("+proj=longlat +datum=WGS84")),
    fillColor = "blue"
  )

count <- calle_predict_sf %>%
  st_drop_geometry() %>%
  count(CodigoCL)
#9900 NA

## summarise------------------
calle_predict_sf2 <- calle_predict_sf %>% 
  drop_na(CodigoCL)

calle_predict <- calle_predict_sf2 %>% 
  st_drop_geometry() %>% 
  rename_all(~paste0("pr_", .)) %>% 
  rename(CodigoCL = pr_CodigoCL) %>% 
  group_by(CodigoCL) %>% 
  summarise(
    across(pr_Sign_traffic:pr_Potholes, ~sum(.x), .names = "{.col}")
  ) %>% 
  rename_with(~ tolower(.x))

# 3. Join GIS to prediction ----------
pr_gis_calle <- gis_clean %>% 
  right_join(calle_predict, by = "codigocl") %>% #check reference gis are not NA
  drop_na()
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
  ) 
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

# 4. Reliability metrics
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

## ROC 
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

## Summary table 
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

write_csv(df, file = "MLdata_GIS/train_gis_reliability.csv")
