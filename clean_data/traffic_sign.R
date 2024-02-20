library(sf)
library(tidyverse)

st_layers("../data/traffic_sign/Inventario.gdb/")
raw <- st_read("../data/traffic_sign/Inventario.gdb/", layer = "SEN_VERTICAL")

calle <- st_read("../data/Calles/Calles_datos/Calles_datos.shp")

st_crs(raw)
st_crs(calle)

sign_st <- calle %>% 
  select(CodigoCL, geometry) %>% 
  st_transform(crs = st_crs(raw)) %>% 
  st_join(raw, left = TRUE, join = st_contains) %>% 
  #filter(is.na(CodigoCL)) %>% 

#takes forever 
# calle_count <- sign_st %>% 
#   group_by(CodigoCL) %>% 
#   summarise(n = n())

calle_count <- sign_st %>% 
  as.data.frame() %>% 
  select(CodigoCL) %>% 
  count(CodigoCL)

clean_sign <- raw %>% 
  group_by(TIPO_SENAL, FASE, ACCION) %>% 
  summarize(n = n())
