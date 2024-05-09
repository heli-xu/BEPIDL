library(tidyverse)
library(sf)
library(leaflet)

# get ped collision data --------------------
a <- st_read("../../data/collision/siniestros2015-2021.gdb/")

a2 <- a %>% 
  filter(CLASE_ACC == "ATROPELLO",
         ANO_OCURRENCIA_ACC %in% c(2015:2019))

saveRDS(a2, "../../data/collision/atropello_point_sf.rds")

# Join to ZAT level -----------------------
ped <- readRDS("../../data/collision/atropello_point_sf.rds")
zat_xy <- readRDS("../../clean_data/zat/zat_shapefile.rds") %>% 
  st_zm()

sf_use_s2(FALSE)

st_crs(ped)
st_crs(zat_xy)

ped_zat <- ped %>% 
  st_transform(crs = st_crs(zat_xy)) %>% 
  st_join(zat_xy, .predicate = st_within) %>% 
  drop_na(ZAT) #30ish NA

## Map of point in ZAT ----------------------------
label <- glue::glue("Collision point matched to ZAT{ped_zat$ZAT}")
leaflet() %>% 
  addTiles() %>% 
  addPolygons(
    data = zat_xy %>% 
      st_transform(crs = st_crs("+proj=longlat +datum=WGS84")),
    fillColor = "blue", color = "blue"
  ) %>% 
  addCircleMarkers(
    data = ped_zat %>% 
      st_transform(crs = st_crs("+proj=longlat +datum=WGS84")),
    radius = 1, fillColor = "purple", color = "purple",
    label = label,
    labelOptions = labelOptions(
      style = list(
        "font-family" = "Fira Sans, sans-serif",
        "font-size" = "1.2em"
      ))
  )

saveRDS(ped_zat, file = "atropello_zat_sf.rds")

ped_zat_df <- ped_zat %>% 
  st_drop_geometry() %>% 
  dplyr::select(ZAT, CODIGO_ACCIDENTE, GRAVEDAD) %>% 
  mutate(count = 1)

ped_zat_df2 <- ped_zat_df %>% 
  group_by(ZAT, GRAVEDAD) %>% 
  summarise(count = sum(count), .groups = "drop") %>%
  pivot_wider(id_cols = ZAT, names_from = GRAVEDAD, values_from = count) %>% 
  rename(injury = "CON HERIDOS",
    death = "CON MUERTOS",
    damage = "SOLO DANOS") %>% 
  mutate(across(injury:damage, ~replace_na(., 0))) %>% 
  mutate(total = injury + death + damage)

saveRDS(ped_zat_df2, file = "collision_zat_df.rds")
