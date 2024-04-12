library(tidyverse)
library(readr)
library(jsonlite)
library(foreign)
library(sf)
library(leaflet)
library(janitor)

# import data -------------------------------------------------------------

annot <- fromJSON("../data/MLdata_GIS/test.json")

z <- annot$images

annot2 <- fromJSON("../data/MLdata_GIS/train.json")
annot3 <- fromJSON("../data/MLdata_GIS/val.json")

calle_geo <- readRDS("calles/calle_shapefile.rds")



# test.json to sf object -----------------------------------------------------

#str_locate("images/_-9NH_oRrVLTZndqJ1W-cQ_4.605982_-74.075430_2019.jpg", "-74.075430")

#options(digits = 8)
#very important to set signifiant digits, otherwise as.numeric() cuts off decimal places
img_coord <- annot %>% 
  pluck("images") %>% 
  mutate(file_name_len = nchar(file_name)) %>%   # found NA if not check if all filenames are same length
 # count(file_name_len) ---2file names are extra long!
  mutate(lat = if_else(file_name_len == 58, str_sub(file_name, 31, 38), str_sub(file_name, 73, 80)), #do not use gpt for index, 
         long = if_else(file_name_len ==58, str_sub(file_name, 40, 49), str_sub(file_name, 82, 91))) %>%
  mutate(lat = as.numeric(lat),
         long = as.numeric(long)) %>% 
  #remember to use numeric for coords!
  rename(image_id = id)
 # filter(is.na(lat))

object <- annot %>% 
  pluck("annotations") %>% 
  select(id, image_id, category_id)

category <- annot %>% 
  pluck("categories") %>% 
  select(-supercategory) %>% 
  rename(category_id = id)

annot1 <- object %>% 
  left_join(category, by = "category_id") %>% 
  select(-category_id) %>% 
  mutate(count = 1) %>% 
 # cannot just pivot, need to group by image(point) 
  #pivot_wider(id_cols = image_id, names_from = "name", values_from = "count")
  group_by(image_id, name) %>% 
  summarise(count = sum(count), .groups = "drop") %>% 
  pivot_wider(names_from = "name", values_from = "count") %>% 
 # filter(is.na(image_id))
  mutate(
    across(everything(), ~ replace_na(., 0))
  )

test_sf <- annot1 %>% 
  left_join(img_coord %>% select(image_id, lat, long), by = "image_id") %>% 
  #filter(is.na(long))
  st_as_sf(coords = c("long","lat"), crs = 4326)
#omg...everytime struggle with this, long is x, if label is right, --check map
# function to process------------------------------------------------------------

json_sf_annot <- function(annot_json){
  options(digits = 8)
  
  img_coord <- annot_json %>% 
    pluck("images") %>% 
    mutate(file_name_len = nchar(file_name)) %>%   # found NA if not check if all filenames are same length
    # count(file_name_len) ---2file names are extra long!
    mutate(lat = if_else(file_name_len == 58, str_sub(file_name, 31, 38), str_sub(file_name, 73, 80)), #do not use gpt for index, 
           long = if_else(file_name_len ==58, str_sub(file_name, 40, 49), str_sub(file_name, 82, 91))) %>%
    mutate(lat = as.numeric(lat),
           long = as.numeric(long)) %>% 
    #remember to use numeric for coords!
    rename(image_id = id)
  # filter(is.na(lat))
  
  object <- annot_json %>% 
    pluck("annotations") %>% 
    select(id, image_id, category_id)
  
  category <- annot_json %>% 
    pluck("categories") %>% 
    select(-supercategory) %>% 
    rename(category_id = id)
  
  annot_df <- object %>% 
    left_join(category, by = "category_id") %>% 
    select(-category_id) %>% 
    mutate(count = 1) %>% 
    # cannot just pivot, need to group by image(point) 
    #pivot_wider(id_cols = image_id, names_from = "name", values_from = "count")
    group_by(image_id, name) %>% 
    summarise(count = sum(count), .groups = "drop") %>% 
    pivot_wider(names_from = "name", values_from = "count") %>% 
    # filter(is.na(image_id))
    mutate(
      across(everything(), ~ replace_na(., 0))
    )
  
  annot_sf <- annot_df %>% 
    left_join(img_coord %>% select(image_id, lat, long), by = "image_id") %>% 
    #filter(is.na(long))
    st_as_sf(coords = c("long","lat"), crs = st_crs(4326))
  
  return(annot_sf)
}

# train.json to sf object ---------------------------------------------------

train_sf <- json_sf_annot(annot2)

# val.json to sf------------------------------------------------------------

val_sf <- json_sf_annot(annot3)

#val_sf %>% filter(st_is_empty(geometry))

# Join json-calle  -----------------------------------

#always a mess when joining point to polygon from different source
#sometime no joining because the point projected to africa
leaflet() %>% 
  addTiles() %>% 
  addPolygons(
    data=calle_geo[1:100,]%>%
      st_transform(crs = st_crs("+proj=longlat +datum=WGS84")),
    weight = 1, fillColor = 'blue', color = 'blue') %>% 
  addCircleMarkers(
    data=test_sf %>% 
      st_transform(crs = st_crs("+proj=longlat +datum=WGS84")), 
    radius =3,
    fillColor = 'purple', color = 'purple') 


annot_all <- bind_rows(test_sf, train_sf, val_sf)
 # distinct(image_id)

sf_use_s2(FALSE)

annot_calle_sf <- annot_all %>% 
  st_transform(crs = st_crs(calle_geo)) %>% 
  st_join(calle_geo,join= st_within) %>% 
  filter(is.na(CodigoCl))
  drop_na(CodigoCL)

na <- annot_all %>% 
    st_transform(crs = st_crs(calle_geo)) %>% 
    st_join(calle_geo,join= st_within) %>% 
    filter(is.na(CodigoCL))
#849 unmatch   

# Clean annot_calle ------------------------------------------------

annot_calle <- annot_calle_sf %>% 
  st_drop_geometry() %>% 
  clean_names() %>% 
  rename(codigocl = codigo_cl) %>% 
  rename_with(~ paste0("js_",.), crosswalk:yield_sign)

# Join with gis_clean, YN columns-------------------------------------------------

json_annot_gis <- annot_calle %>% 
  left_join(gis_clean, by = "codigocl") %>% 
 # drop_na(gis_bike_signs) %>%  no NA in GIS columns
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
    across(c(js_crosswalk:js_yield_sign, gis_trees, gis_bus_stops, 
             gis_road_signs, gis_traffic_lights, gis_road_signs_inv, 
             gis_stop_signs, gis_traffic_fines_tot, gis_lturn_sign, 
             gis_bike_signs, gis_bus_signs, gis_pedxwalk_signs, 
             gis_speed_bump_signs, gis_stop_signs2, gis_parking_signs, 
             gis_school_zone_signs, gis_yield_signs, gis_total_st_signs),
           ~if_else(.>0, 1, 0, missing = 0), .names = "{.col}_yn")
  ) %>% 
  mutate(
    any_ped = if_else(si_act_pea >0, 1, 0),
    js_pedxwalk_yn = if_else(js_crossing_sign_yn == 1 | js_crosswalk_yn == 1 | js_raised_crosswalk_yn == 1, 
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

saveRDS(json_annot_gis, file = "MLdata_GIS/json_annot_gis.rds")

# Reliability Metrics ------------------------------------------

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


js_variables <- c(
  "js_traffic_sign_yn",
  "js_traffic_sign_yn",
  "js_traffic_light_yn",
  "js_pedxwalk_yn",
  "js_stop_sign_yn",
  "js_stop_sign_yn",
  "js_yield_sign_yn",
  "js_school_zone_yn",
  "js_sidewalk_yn",
  "js_bike_lanes_yn",
  "js_bus_lane_yn",
  "js_median_yn",
  "js_speed_bump_yn",
  "js_trees_yn",
  "js_bus_stop_yn",
  "js_parked_vehicles_yn",
  "js_parking_lane_yn",
  "js_brt_station_yn"
)

## ROC ------------------------------------
#(again, note that the var name will overwrite previous comparisons')
res <- map2(js_variables, gis_variables, 
           \(x, y) roc(json_annot_gis[[y]], as.numeric(json_annot_gis[[x]])))

### Curve -----------
label <- str_sub(gis_variables, 5)

n <- c(1:length(gis_variables))
colors <- c("#FF0000", "#FFA500", "#FFFF00", "#008000", "#00FF00", "#00FFFF", "#0000FF", "#800080", "#FFC0CB", "#FF69B4", "#8B4513", "#FFD700", "#00CED1", "#483D8B", "#32CD32", "#800000", "#800080", "#2E8B57")

plot(res[[1]])
map2(n, colors, \(x, y) plot(res[[x]], col = y, add = T))
legend("bottomright", legend = label, col = colors, lty = 1, cex =0.8, text.font = 2, bty = "n")
title(main = "Annotator-GIS comparison", line = 3)



## summary metrics table
source("../functions/reliability_table.R")

df <- reliability_table(js_variables, gis_variables, json_annot_gis)

write_csv(df, file = "MLdata_GIS/annotator_gis_reliability.csv")

