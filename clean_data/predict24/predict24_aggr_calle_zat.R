library(sf)
library(tidyverse)
library(leaflet)

sf_use_s2(FALSE)

# 0. import data--------
predict_200k <- read_csv("../../data/AI_prediction2024/predictions_200k.csv")
calle_geo <- readRDS("../../clean_data/calles/calle_shapefile.rds")
calle_zat_xwalk <- readRDS("../calle_zat_xwalk.rds")

# 1. Link image coordinates ------------------
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

## summarise ---------
calle_predict24 <- calle_predict_sf %>% 
  drop_na(CodigoCL) %>% 
  st_drop_geometry() %>% 
  group_by(CodigoCL) %>% 
  summarise(
    across(Sign_traffic:Potholes, ~sum(.x), .names = "{.col}")
  ) %>% 
  rename_with(~ tolower(.x), .cols = -CodigoCL)

saveRDS(calle_predict24, file = "calle_predict24.rds")
#gitignored, need rerunning for new device

# 3. Aggregate to ZAT w replicates -----------------------
predict24_zat_rep <- calle_predict24 %>% 
  left_join(calle_zat_xwalk, by = "CodigoCL") %>% 
  drop_na(ZAT) %>% #still need to remove NA here
  uncount(match_n) %>% ## make replicates for multimatching zat
  group_by(ZAT) %>% 
  summarise(
    across(sign_traffic:potholes, ~sum(.x), .names = "{.col}"),
    .groups = "drop"
  )

# 4. Standardize -------------------
zat_area <- readRDS("../ZAT/zat_std2n.rds") %>% 
  select(ZAT, areakm2)

predict24_zat_rep2 <- predict24_zat_rep %>% 
  left_join(zat_area, by = "ZAT") %>% 
  mutate(
    across(sign_traffic:potholes, ~.x/areakm2)
  ) %>% 
  select(-areakm2)

saveRDS(predict24_zat_rep2, file = "predict24_zat_rep2.rds")  
