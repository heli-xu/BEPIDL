library(tidyverse)
library(leaflet)


# 0. import data -------
calle_geo <- readRDS("../../clean_data/calles/calle_shapefile.rds")

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

# 1. Diff (pr vs gis) -------------------
new_col <- paste0("pYgN_", str_sub(pr_variables, 4, -4))
pr_gis_calle <- readRDS("predict_gis_calle.rds")

prYgisN_expr <- paste(pr_variables, "-", gis_variables)

check_yn <- pr_gis_calle %>%
  dplyr::select(codigocl, all_of(pr_variables), all_of(gis_variables)) %>% 
  mutate(across(-codigocl, ~as.numeric(.x)))

check2 <- map2_dfc(new_col, prYgisN_expr, function(new_col, prYgisN_expr){
  check_yn %>% 
    transmute(
      {{new_col}} := eval(str2lang({{prYgisN_expr}}))
    )
}) %>% 
  bind_cols(check_yn) %>% 
  dplyr::select(codigocl, starts_with("pYgN_"))

colnames(check2)

# 2. prYES, gisNO --------------  
prYgisN <- check2 %>% 
  pivot_longer(cols = -codigocl, names_to = "variable", values_to = "diff") %>% 
  filter(diff > 0) %>% 
  group_by(codigocl) %>% 
  summarise(
    diff_all = sum(diff)
  )
#62k rows

saveRDS(prYgisN, file = "predictY_gisN.rds")

prYgisN_top250 <- prYgisN %>%
  arrange(desc(diff_all)) %>% 
  head(500) %>% 
  #slice_max(diff_all, n = 250) %>%  
  #12-15 feature diff
  left_join(calle_geo %>% rename(codigocl = CodigoCL),
    by = "codigocl") %>% 
  st_as_sf()

prYgisN_top250 %>% 
  st_transform(crs = st_crs("+proj=longlat +datum=WGS84")) %>% 
  leaflet() %>% 
  addTiles() %>% 
  addPolygons(
    color = "purple",
    fillColor = "purple"
  )

# 3. gisYes, prNO-------------------
gisYprN <- check2 %>% 
  pivot_longer(cols = -codigocl, names_to = "variable", values_to = "diff") %>% 
  filter(diff < 0) %>% 
  group_by(codigocl) %>% 
  summarise(
    diff_all = sum(diff)
  )
#9k rows

saveRDS(gisYprN, file = "gisY_predictN.rds")

gisYprN_top250 <- gisYprN %>% 
  arrange(diff_all) %>% 
  head(500) %>% 
  #slice_min(diff_all, n = 250) %>% 
  # 3-7 feature diff
  left_join(calle_geo %>% rename(codigocl = CodigoCL),
    by = "codigocl") %>% 
  st_as_sf()

gisYprN_top250 %>% 
  st_transform(crs = st_crs("+proj=longlat +datum=WGS84")) %>% 
  leaflet() %>% 
  addTiles() %>% 
  addPolygons(
    color = "blue",
    fillColor = "blue"
  )



      