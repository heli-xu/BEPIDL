library(sf)
library(tidyverse)
library(leaflet)
library(glue)

sf_use_s2(FALSE)

# 0. Import data ------------------------------------
ses <- st_read("../../data/SES/SHP_MGN2018_INTGRD_MANZ/MGN_ANM_MANZANA.shp")

ses2 <- ses %>% 
  select(
    COD_DANE_A, TP27_PERSO)

calle_geo <- readRDS("../../clean_data/calles/calle_shapefile.rds")

# 1. Create street-800m buffer --------------------

st_crs(calle_geo)$units #m
st_crs(ses2)$units #null

buffer800m <- st_buffer(calle_geo, dist = 800, endCapStyle = "SQUARE", joinStyle = "BEVEL")

# 2. Join buffer-block-------------
mzn_buffer <- ses2 %>%
  st_transform(crs = st_crs(buffer800m)) %>% 
  st_join(buffer800m, .predicate = st_intersects)

# 3. Sum pop by street --------------- 
pop800m <- mzn_buffer %>% 
  st_drop_geometry() %>% 
  drop_na(CodigoCL) %>% #remember!
  #count(CodigoCL) 
  group_by(CodigoCL) %>% 
  summarise(pop = sum(TP27_PERSO), .groups = "drop")

saveRDS(pop800m, file = "pop_calle800m.rds")
