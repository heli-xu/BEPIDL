library(tidyverse)
library(readr)
library(sf)
library(leaflet)

# import data -------------------------------

gis_clean <- readRDS("MLdata_GIS/gis_clean.rds")
canvas <- read_csv("../data/CANVAS/Bogota_AllRaters_20230519.csv")

colnames(canvas)

# clean/filter col in canvas

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

# join canvas segments to street------------------------------------------

# derive corresp. columns in gis_clean---------------------
gis_clean2 <- gis_clean %>% 
  right_join(tr_calle, by = "codigocl") %>% #check reference gis are not NA
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
    across(c(tr_sign_traffic:tr_potholes, gis_trees, gis_bus_stops, 
      gis_road_signs, gis_traffic_lights, gis_road_signs_inv, 
      gis_stop_signs, gis_traffic_fines_tot, gis_lturn_sign, 
      gis_bike_signs, gis_bus_signs, gis_pedxwalk_signs, 
      gis_speed_bump_signs, gis_stop_signs2, gis_parking_signs, 
      gis_school_zone_signs, gis_yield_signs, gis_total_st_signs),
      ~if_else(.>0, 1, 0, missing = 0), .names = "{.col}_yn")
  ) %>% 
  mutate(
    any_ped = if_else(si_act_pea >0, 1, 0),
    tr_pedxwalk_yn = if_else(tr_sign_crossing_yn == 1 | tr_crosswalk_yn == 1, 
      #tr_pedestrian_light not included (not sure if an_pedestrian is relavant)
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
