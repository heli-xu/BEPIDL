library(tidyverse)
library(readr)
library(sf)
library(leaflet)
library(sfheaders)
library(pROC)
library(caret)
library(epiR)

# import data -------------------------------

gis_clean <- readRDS("MLdata_GIS_CANVAS/gis_clean.rds")
canvas <- read_csv("../data/CANVAS/Bogota_AllRaters_20230519.csv")

colnames(canvas)

# clean/filter col in canvas -----------------------------------

canvas_clean <- canvas %>% 
  filter(!is.na(bldg_pres)) %>% #this entry all var are NA
  mutate(
    tree_yn = if_else(veg_tree == 0, 0, 1, missing = 0),
    median_yn = if_else(str_med == 0, 0, 1, missing = 0),
    stop_sign_yn = if_else(str_tcont == 1, 1, 0, missing = 0),
    traffic_light_yn = if_else(str_tcont == 3, 1, 0, missing = 0),
    roundabout_yn = if_else(str_tcont ==2, 1, 0, missing = 0),
    speed_bump_yn = if_else(str_scont == 1, 1, 0, missing = 0),
    crosswalk_yn = if_else(str_cwalk == 1, 1, 0, missing = 0),
    bike_lane_yn = if_else(str_blane == 0, 0, 1, missing = 0), #temp bike lane?
    school_zone_yn = if_else(str_mod == 1, 1, 0, missing = 0),
    sidewalk_yn = if_else(swalk_pres == 0, 0, 1, missing = 0),
    transit_lane_yn = if_else(trans_blane == 0, 0, 1, missing = 0)
  ) %>% 
  group_by(segment_id) %>%  # 5 people rated one segment
  summarise(
    across(ends_with("_yn"), ~ sum(.x), .names = "{.col}")
  ) %>% 
  mutate(
    across(-segment_id, ~if_else(. == 0, 0, 1))  #it takes one rater to make it yes
  ) 

# make CANVAS linestring from coordinate pairs------------------------------
canvas_coord  <- canvas %>% 
  distinct(segment_id, .keep_all = TRUE) %>% 
  select(segment_id,start_lat, start_lng, end_lat, end_lng) 
  
canvas_line <- canvas_coord %>% 
  select(segment_id, lat = start_lat, lng = start_lng) %>% 
  mutate(order = 1) %>% 
  bind_rows(canvas_coord %>% 
      select(segment_id, lat = end_lat, lng=end_lng) %>% 
      mutate(order =2)) %>% 
  group_by(segment_id) %>% 
  arrange(order, .by_group = TRUE) %>% 
  sf_linestring(x = "lng", y = "lat", linestring_id = "segment_id")

canvas_sf <- canvas_clean %>% 
  left_join(canvas_line, by = "segment_id") %>% 
  st_as_sf(crs = 4326)

leaflet() %>% 
  addTiles() %>% 
  addPolylines(
    data = canvas_sf %>%
      st_transform(crs = st_crs("+proj=longlat +datum=WGS84")),
    weight = 2, fillColor = 'blue', color = 'blue')
  # addPolygons(
  #   data = pr_gis_geo %>% 
  #     st_transform(crs = st_crs("+proj=longlat +datum=WGS84")), 
  #   weight = 2,
  #   fillColor = 'purple', color = 'purple') 

saveRDS(canvas_sf, file = "MLdata_GIS_CANVAS/canvas_sf.rds")

# Join CANVAS to calle----------------------------------------
sf_use_s2(FALSE)
calle_geo <- readRDS(file = "calles/calle_shapefile.rds")

##line to polygon, a bit more tricky (st_within doesn't work)
calle_canvas_sf <- canvas_sf %>% 
  st_transform(crs = st_crs(calle_geo)) %>% 
  st_join(calle_geo,
    join= st_intersects, largest = TRUE) %>% 
  #NOTE the order of sf object in st_join() also matters here with 'largest' arg.
  drop_na(CodigoCL) %>% 
  st_drop_geometry() %>% 
  left_join(calle_geo, by = "CodigoCL") %>% 
  st_as_sf()

## map: canvas lines and matched street polygons -----------------
leaflet() %>% 
  addProviderTiles(provider = "CartoDB.Positron") %>% 
  addPolylines(
    data = canvas_sf %>%
      st_transform(crs = st_crs("+proj=longlat +datum=WGS84")),
    weight = 2, fillColor = 'blue', color = 'blue') %>% 
  addPolygons(
    data = calle_canvas_sf %>%
      st_transform(crs = st_crs("+proj=longlat +datum=WGS84")),
    weight = 2,
    fillColor = 'purple', color = 'purple')

saveRDS(calle_canvas_sf, file = "MLdata_GIS_CANVAS/calle_canvas_sf.rds")


calle_canvas <- calle_canvas_sf %>% 
  st_drop_geometry() %>% 
  rename(codigocl = CodigoCL) %>% 
  rename_with(~paste0("can_", .), tree_yn:transit_lane_yn)

# 1. CANVAS vs GIS------------------------------------------------------
## joining with gis_clean and derive ---------------------------

##mostly on the gis side, except the last factoring '_yn' var
##also acting on canvas side
canvas_gis <- gis_clean %>% 
  right_join(calle_canvas, by = "codigocl") %>% #check reference gis are not NA
  #drop_na(tr_Potholes)
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
    across(c(gis_trees, gis_bus_stops, 
      gis_road_signs, gis_traffic_lights, gis_road_signs_inv, 
      gis_stop_signs, gis_traffic_fines_tot, gis_lturn_sign, 
      gis_bike_signs, gis_bus_signs, gis_pedxwalk_signs, 
      gis_speed_bump_signs, gis_stop_signs2, gis_parking_signs, 
      gis_school_zone_signs, gis_yield_signs, gis_total_st_signs),
      ~if_else(.>0, 1, 0, missing = 0), .names = "{.col}_yn")
  ) %>% 
  mutate(
    any_ped = if_else(si_act_pea >0, 1, 0)
  ) %>% 
  mutate(
    across(c(gis_sw,
      gis_bike_lane,
      gis_any_bus,
      gis_median,
      ends_with("_yn")), ~factor(.))
  )

saveRDS(canvas_gis, file = "MLdata_GIS_CANVAS/canvas_gis.rds")

write_csv(canvas_gis, file = "MLdata_GIS_CANVAS/canvas_gis_comparison.csv")

## Reliability metrics---------------------------------------------

# can_variables <- calle_canvas %>% 
#   select(can_tree_yn:can_transit_lane_yn) %>% 
#   colnames()
# list it out to repeat if needed for comparison pair
# order canvas according to gis for easy comparing with train, predict and annot

can_variables <- c(
  "can_traffic_light_yn",
  "can_crosswalk_yn",
  "can_stop_sign_yn", 
  "can_stop_sign_yn", 
  "can_school_zone_yn",
  "can_sidewalk_yn", 
  "can_bike_lane_yn", 
  "can_transit_lane_yn",
  "can_median_yn", 
  "can_speed_bump_yn", 
  "can_tree_yn"
  # "can_roundabout_yn"
)

gis_variables <- c(
  # "gis_road_signs_inv_yn",
  # "gis_total_st_signs_yn",
  "gis_traffic_lights_yn",
  "gis_pedxwalk_signs_yn",
  "gis_stop_signs_yn",
  "gis_stop_signs2_yn",
  # "gis_yield_signs_yn",
  "gis_school_zone_signs_yn",
  "gis_sw",
  "gis_bike_lane",
  "gis_any_bus",
  "gis_median",
  "gis_speed_bump_signs_yn",#noted change
  "gis_trees_yn"
  # "gis_bus_stops_yn",
  # "gis_parking_signs_yn",
  # "gis_parking_signs_yn",
  # "gis_brt_yn"
)

### ROC ----------------------
#using CANVAS as reference
roc(canvas_gis$can_tree_yn, as.numeric(canvas_gis$gis_trees_yn))

res = map2(can_variables, gis_variables, 
  \(x, y) roc(canvas_gis[[x]], as.numeric(canvas_gis[[y]])))


label <- str_sub(can_variables, 5)
n <- c(1:length(can_variables))
colors <- c("#FF0000", "#FFA500", "#008000",  "#0000FF", "#FF69B4", "#8B4513", "#00CED1", "#483D8B", "#32CD32", "#800000", "#800080")



plot(res[[1]])
map2(n, colors, \(x, y) plot(res[[x]], col = y, add = T))
legend("bottomright", legend = label, col = colors, lty = 1, lwd = 3, cex =0.8, text.font = 2, bty = "n")
title(main = "CANVAS-GIS comparison", line = 3)

### Summary metrics table -------------------------------
source("../functions/reliability_table.R")

df <- reliability_table(var = gis_variables, var_ref = can_variables, canvas_gis)

write_csv(df, file = "MLdata_GIS_CANVAS/canvas_gis_reliability.csv")

# 2. CANVAS vs Training ----------------------------------------------------------------
## joining training-CANVAS--------------------------
tr_gis_calle <- readRDS("MLdata_GIS_CANVAS/tr_gis_calle.rds")

canvas_train <- calle_canvas %>%   
  # factoring is performed at canvas_gis!! not calle_canvas
  left_join(tr_gis_calle %>% select(codigocl, starts_with("tr_")), by = "codigocl") %>% 
  drop_na() %>%  # NA in `tr_xx` columns
# 69 streets matched...
  mutate(
    across(ends_with("_yn"), ~factor(.))
  )
#factor again

saveRDS(canvas_train, file = "MLdata_GIS_CANVAS/canvas_train.rds")

write_csv(canvas_train, file = "MLdata_GIS_CANVAS/canvas_training_comparison.csv")

## map: few street overlap -------------------------------
tr_gis_sf <- tr_gis_calle %>% 
  rename(CodigoCL = codigocl) %>% 
  left_join(calle_geo, by = "CodigoCL") %>% 
  drop_na(CodigoCL) %>% 
  st_as_sf()


leaflet() %>% 
  addProviderTiles(provider = "CartoDB.Positron") %>% 
  addPolygons(
    data = calle_canvas_sf %>%
      st_transform(crs = st_crs("+proj=longlat +datum=WGS84")),
    weight = 2, fillColor = 'blue', color = 'blue') %>% 
  addPolygons(
    data = tr_gis_sf %>%
      st_transform(crs = st_crs("+proj=longlat +datum=WGS84")),
    weight = 2,
    fillColor = 'purple', color = 'purple')

## Reliability metrics ---------------------------
tr_variables <- c(
  # "tr_sign_traffic_yn",
  # "tr_sign_traffic_yn",
  "tr_traffic_light_yn",
  "tr_pedxwalk_yn",
  "tr_sign_stop_yn",
  # "tr_sign_stop_yn",  # no need to repeat pair
  # "tr_sign_yield_yn",
  "tr_sign_school_zone_yn",
  "tr_sidewalk_yn",
  "tr_lane_bike_yn",
  "tr_lane_bus_yn",
  "tr_median_yn",
  "tr_speed_bump_yn",
  "tr_trees_yn"
  # "tr_bus_stop_yn",
  # "tr_parked_vehicles_yn",
  # "tr_lane_parking_yn",
  # "tr_brt_station_yn"
)

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

### ROC ---------------------
res <- map2(tr_variables, can_variables, 
  \(x, y) roc(canvas_train[[y]], as.numeric(canvas_train[[x]])))

label <- str_sub(can_variables, 5)
n <- c(1:length(can_variables))
colors <- c("#FF0000", "#FFA500", "#008000",  "#0000FF", "#FF69B4", "#8B4513", "#00CED1", "#32CD32", "#800000", "#800080")


plot(res[[1]])
map2(n, colors, \(x, y) plot(res[[x]], col = y, add = T))
legend("bottomright", legend = label, col = colors, lty = 1, lwd = 3, cex =0.8, text.font = 2, bty = "n")
title(main = "CANVAS-training comparison", line = 3)

### summary table --------------------------
#source("../functions/reliability_table.R")

df <- reliability_table(var = tr_variables, var_ref = can_variables, canvas_train)

write_csv(df, file = "MLdata_GIS_CANVAS/canvas_train_reliability.csv")

# 3. CANVAS vs Predict --------------------------------------------
## joining predict-CANVAS -----------------------------
predict_gis <- readRDS("MLdata_GIS_CANVAS/predict_gis_clean2.rds")

canvas_predict <- calle_canvas %>%   
  # factoring is performed at canvas_gis!! not calle_canvas
  left_join(predict_gis %>% select(codigocl, starts_with("an_")), by = "codigocl") %>% 
  drop_na() %>%  # NA in `an_xx` columns
  # 85 streets matched...
  mutate(
    across(ends_with("_yn"), ~factor(.))
  )
#factor again

saveRDS(canvas_predict, file = "MLdata_GIS_CANVAS/canvas_predict.rds")

write_csv(canvas_predict, file = "MLdata_GIS_CANVAS/canvas_predict_comparison.csv")

## map ---------------------------------------------------

pr_gis_sf <- predict_gis %>% 
  rename(CodigoCL = codigocl) %>% 
  left_join(calle_geo, by = "CodigoCL") %>% 
  drop_na(CodigoCL) %>% 
  st_as_sf()

leaflet() %>% 
  addProviderTiles(provider = "CartoDB.Positron") %>% 
  addPolygons(
    data = calle_canvas_sf %>%
      st_transform(crs = st_crs("+proj=longlat +datum=WGS84")),
    weight = 2, fillColor = 'blue', color = 'blue') %>% 
  addPolygons(
    data = pr_gis_sf %>%
      st_transform(crs = st_crs("+proj=longlat +datum=WGS84")),
    weight = 2,
    fillColor = 'purple', color = 'purple')

## Reliability metrics ----------------------------------
an_variables <- c(
  # "an_sign_traff_yn",
  # "an_sign_traff_yn",
  "an_traffic_li_yn",
  "an_pedxwalk_yn",
  "an_sign_stop_yn",
  # "an_sign_stop_yn",
  # "an_sign_yield_yn",
  "an_sign_schoo_yn",
  "an_sidewalk_yn",
  "an_lane_bike_yn",
  "an_lane_bus_yn",
  "an_median_yn",
  "an_speed_bump_yn",
  "an_trees_yn"
  # "an_bus_stop_yn",
  # "an_parked_veh_yn",
  # "an_lane_parki_yn",
  # "an_brt_statio_yn"
)

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

### ROC --------------------------------
res <- map2(an_variables, can_variables, 
  \(x, y) roc(canvas_predict[[y]], as.numeric(canvas_predict[[x]])))

label <- str_sub(can_variables, 5)
n <- c(1:length(can_variables))
colors <- c("#FF0000", "#FFA500", "#008000",  "#0000FF", "#FF69B4", "#8B4513", "#00CED1", "#32CD32", "#800000", "#800080")


plot(res[[1]])
map2(n, colors, \(x, y) plot(res[[x]], col = y, add = T))
legend("bottomright", legend = label, col = colors, lty = 1, lwd = 3, cex =0.8, text.font = 2, bty = "n")
title(main = "CANVAS-predict comparison", line = 3)

### summary table------------------------
#source("../functions/reliability_table.R")

df <- reliability_table(an_variables, can_variables, canvas_predict)

write_csv(df, file = "MLdata_GIS_CANVAS/canvas_predict_reliability.csv")
