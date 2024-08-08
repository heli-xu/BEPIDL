library(sf)
library(tidyverse)
library(leaflet)

sf_use_s2(FALSE)

# 0. import data--------
predict_200k <- read_csv("../../data/AI_prediction2024/predictions_200k.csv")
##only keep 2015-2019
predict_yr <- predict_200k %>% 
  filter(Date %in% 2015:2019)

calle_geo <- readRDS("../../clean_data/calles/calle_shapefile.rds")
calle_zat_xwalk <- readRDS("../calle_zat_xwalk.rds")
zat_shapefile <- readRDS("../../clean_data/ZAT/zat_shapefile.rds") 

# 1. Link image coordinates ------------------
predict_sf <- st_as_sf(predict_yr, coords = c("Latitude", "Longitude"),  
                       #googlemap api use lat first, but longitude is x, lat is y
                       #note confusing column names
                       crs = st_crs(4326)) 

#make sure they're at right places
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

count %>% filter(is.na(CodigoCL))
#9900 NA all years
#4502 NA 2015-2019

## summarise calle_predict24 ---------
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

saveRDS(calle_predict24, file = "calle_predict24_2015_19.rds")

## adjust by calle area-------------------
calle_area <- readRDS("../calles/calle_rename_df.rds") %>%
  dplyr::select(codigocl, area_calle) %>% 
  rename(CodigoCL = codigocl)

predict24_calle_adj <- predict24_calle %>% 
  left_join(calle_area, by = "CodigoCL") %>% 
  mutate(across(-CodigoCL, ~.x/area_calle))

saveRDS(predict24_calle_adj, file = "calle_predict24_1519adj.rds")
#gitignored

# 3. Aggregate to ZAT  -----------------------
zat_geo <- zat_shapefile %>% st_zm()

predict24_zat <- predict_sf %>% 
  st_transform(crs = st_crs(zat_geo)) %>% 
  st_join(zat_geo,join= st_within) 
## NO NEED to create replicates! use point to zat

predict24_zat2 <- predict24_zat %>% 
  st_drop_geometry() %>% 
  drop_na(ZAT) %>% 
  rename_with(~ tolower(.x), .cols = -ZAT) %>% 
  group_by(ZAT) %>% 
  summarise(
    across(sign_traffic:potholes, ~sum(.x), .names = "{.col}"),
    .groups = "drop"
  )

# 4. Standardize -------------------
zat_area <- readRDS("../ZAT/zat_std2n.rds") %>% 
  select(ZAT, areakm2)

predict24_zat3 <- predict24_zat2 %>% 
  left_join(zat_area, by = "ZAT") %>% 
  mutate(
    across(sign_traffic:potholes, ~.x/areakm2)
  ) %>% 
  select(-areakm2)

saveRDS(predict24_zat3, file = "zat_predict24.rds")  
