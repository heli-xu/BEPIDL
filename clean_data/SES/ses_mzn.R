library(sf)
library(tidyverse)
library(leaflet)

ses <- st_read("../../data/SES/SHP_MGN2018_INTGRD_MANZ/MGN_ANM_MANZANA.shp")

ses_level <- ses %>% 
  select(COD_DANE_A, starts_with("TP19_EE_E"))

calle_geo <- readRDS("../../clean_data/calles/calle_shapefile.rds")

st_crs(calle_geo)$units #m
st_crs(ses_level)$units #null

buffer500m <- st_buffer(calle_geo, dist = 500)

#MGN not only for bogota
bb <- st_bbox(calle_geo)

bogota_ses <- ses_level %>% 
  st_transform(crs = st_crs(calle_geo)) %>% 
  st_filter(st_as_sfc(bb), .predicate = st_within)

#just to see how buffer looks like
leaflet() %>% 
  addTiles() %>% 
  addPolygons(
    data = buffer500m[6000:9000,] %>% 
      st_transform(crs = st_crs("+proj=longlat +datum=WGS84")),
    fillColor = "orange", fillOpacity = 0.5, color = "orange"
  ) %>% 
  addPolygons(
    data = bogota_ses[1:2000,] %>% 
      st_transform(crs = st_crs("+proj=longlat +datum=WGS84")),
    fillColor = "blue"
  )


#which one comes first doesn't matter, below is a little bit faster
#will turn out to be 16mil rows....
#in case some edges blocks get cut off, we'll use orginial to join, 
#'bout the same time
ses_buffer <- ses_level %>%
  st_transform(crs = st_crs(buffer500m)) %>% 
  st_join(buffer500m, .predicate = st_intersects)

ses_calle <- ses_buffer %>% 
  st_drop_geometry() %>% 
  group_by(CodigoCL) 
  
  
ses_calle2 <-  ses_calle %>% 
  summarise(across(TP19_EE_E1:TP19_EE_E9, ~sum(.x), .names = "{.col}"), .groups = "drop") %>% 
#  rowwise(CodigoCL) %>% 
  mutate(
    total_household = rowSums(pick(TP19_EE_E1:TP19_EE_E9)),  #faster than rowwise
  #ungroup() %>% 
    across(TP19_EE_E1:TP19_EE_E6, ~ (.x/total_household)*100, .names = "percent_{.col}")
  )

saveRDS(ses_calle2, file = "ses_calle.rds")

check <- ses_calle2 %>% 
  mutate(total = rowSums(pick(percent_TP19_EE_E1:percent_TP19_EE_E6))) %>% 
  select(CodigoCL, TP19_EE_E9, total_household, total)
