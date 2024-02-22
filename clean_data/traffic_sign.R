library(sf)
library(tidyverse)
library(readr)

# import ------------------------------------------------------------------------
st_layers("../data/traffic_sign/Inventario.gdb/")
raw <- st_read("../data/traffic_sign/Inventario.gdb/", layer = "SEN_VERTICAL")

example_raw <- raw %>% 
  as.data.frame() %>% 
  head(10)
saveRDS(example_raw, file = "../clean_data/example_raw.rds")

calle <- st_read("../data/Calles/Calles_datos/Calles_datos.shp")

# spatial join ------------------------------------------------------------------
st_crs(raw)
st_crs(calle)

sign_st <- calle %>% 
  select(CodigoCL, geometry) %>% 
  st_transform(crs = st_crs(raw)) %>% 
  st_join(raw, left = TRUE, join = st_contains) %>% 
  as.data.frame()
# to speed things up during manipulation, join later again

sign_st %>% 
  #filter(is.na(CodigoCL)) %>% 
# not much NA in here 
# but actually quite some NA in tipo_senal--53780
  filter(is.na(TIPO_SENAL))

# if keeping sign_st as sf obj, takes forever -- again, no need for geometry during manipulation
# calle_count <- sign_st %>% 
#   group_by(CodigoCL) %>% 
#   summarise(n = n())

calle_count <- sign_st %>%
  select(CodigoCL) %>% 
  count(CodigoCL) # match calle_datos data 

# clean NA data ------------------------------------------------------------
clean_sign <- sign_st %>% 
  select(-geometry) %>% 
  drop_na(TIPO_SENAL) %>% 
  group_by(CodigoCL, TIPO_SENAL, FASE, ACCION) %>% 
  summarise(n = n(), .groups = "drop") # same as count() %>% ungroup()
# if still grouped data, the grouping variable will be sticky



# ped crossing: SP-46, SI-24, SR-19 separately-------------------------------
pedx_sp46 <- clean_sign %>% 
  filter(str_detect(TIPO_SENAL, pattern = "SP-46")) 
  #ungroup()
write_csv(pedx_sp46, file = "../clean_data/pedx_sp46.csv")

pedxwalk_si24 <- clean_sign %>% 
  filter(str_detect(TIPO_SENAL, pattern = "SI-24"))
write_csv(pedxwalk_si24, file = "../clean_data/pedxwalk_si24.csv")

ped_sr19 <- clean_sign %>% 
  filter(str_detect(TIPO_SENAL, pattern = "SR-19"))
#has 3 counts in 2 street 

# pedx_wide <- pedx %>% 
#   mutate(sign_status = paste(TIPO_SENAL, FASE, ACCION, sep = "_")) %>% 
#   select(CodigoCL, sign_status, n) %>% 
#   pivot_wider(names_from = sign_status, values_from = n)
#very sparse, not making much sense to do

# parking: SI-07, SI-07A -----------------------------------------------
parking <- clean_sign %>% 
  filter(str_detect(TIPO_SENAL, pattern = "SI-07"))

write_csv(parking, file = "../clean_data/parking_calle.csv")

# descriptive statistics-----------------------------------------------------
parking <- read_csv("clean_data/parking_calle.csv")
pedx_sp46 <- read_csv("clean_data/pedx_sp46.csv")
pedxwalk_si24 <- read_csv("clean_data/pedxwalk_si24.csv")

descrp_stat <- function(data){
  data %>% 
    group_by(TIPO_SENAL, FASE, ACCION) %>% 
    count() %>% 
    ungroup() %>% 
    group_by(TIPO_SENAL) %>% 
    mutate(percent_in_sign_type = n/sum(n)*100)
}

pedx_sp46_stat <- descrp_stat(pedx_sp46)

pedxwalk_si24_stat <- descrp_stat(pedxwalk_si24)

parking_stat <- descrp_stat(parking)

write_csv(pedxwalk_si24_stat, file = "../clean_data/traffic_sign/pedxwalk_si24_stat.csv")
write_csv(pedx_sp46_stat, file = "../clean_data/traffic_sign/pedx_sp46_stat.csv")
write_csv(parking_stat, file = "../clean_data/traffic_sign/parking_stat.csv")  
#summary excel sheet organized 02/22/24


# below doesn't seem to help much, removing
# # type-specific signs by street ALL STATUS (for visualization)
# pedx_parking_total <- sign_st %>% 
#   select(-geometry) %>% 
#   drop_na(TIPO_SENAL) %>% 
#   group_by(CodigoCL, TIPO_SENAL) %>% 
#   summarise(n = n(), .groups = "drop") %>% 
#   filter(str_detect(TIPO_SENAL, pattern = "SP-46|SI-24|SR-19|SI-07")) %>% 
#   mutate(sign = case_when(
#     str_detect(TIPO_SENAL, "SP-46") ~ "pedx-SP46",
#     str_detect(TIPO_SENAL, "SI-24") ~ "pedx-SI24",
#     str_detect(TIPO_SENAL, "SR-19") ~ "pedx-SR19",
#     str_detect(TIPO_SENAL, "SI-07") ~ "parking-SI07"
#    )) %>% 
#   select(-TIPO_SENAL)
# 
# # pedx_p_geo.rds ---------------------------------------------------------  
# pedx_p_geo <- calle %>% 
#   select(CodigoCL, geometry) %>% 
#   left_join(pedx_parking_total, by = "CodigoCL") %>% 
#   mutate(n = replace_na(n, 0))
# #this is pretty big 50+MB
# 
# 
# pedx_p_geo %>% 
#   as.data.frame() %>% 
#   distinct(CodigoCL)
# #checking it's same calle count
# 
# saveRDS(pedx_p_geo, file = "../clean_data/pedx_park_geo.rds")
# 
# 
# pedx_pa_geo_sm <- pedx_p_geo %>% 
#   drop_na(sign)
# 
# saveRDS(pedx_pa_geo_sm, file = "../clean_data/pedx_pa_geo_sm.rds")
