library(sf)
library(tidyverse)
library(leaflet)
library(glue)
library(ggplot2)

# 0. Import data ------------------------------------
ses <- st_read("../../data/SES/SHP_MGN2018_INTGRD_MANZ/MGN_ANM_MANZANA.shp")

ses2 <- ses %>% 
  select(
    COD_DANE_A, AREA, TVIVIENDA, TP14_1_TIP, TP14_2_TIP,
    TP15_4_OCU, TP27_PERSO, TP32_1_SEX, TP32_2_SEX,
    starts_with("TP34_"))

calle_geo <- readRDS("../../clean_data/calles/calle_shapefile.rds")

# 1. Link to street-500m buffer --------------------
## Create buffer -------------------
st_crs(calle_geo)$units #m
st_crs(ses2)$units #null

buffer500m <- st_buffer(calle_geo, dist = 500, endCapStyle = "SQUARE", joinStyle = "BEVEL")

## Join buffer-block-------------
mzn_buffer <- ses2 %>%
  st_transform(crs = st_crs(buffer500m)) %>% 
  st_join(buffer500m, .predicate = st_intersects)

## Sum by street ------------
age_group <- c(c("0_9", "10_19", "20_29", "30_39", 
  "40_49", "50_59", "60_69", "70_79", "80_plus"))

age_group_col <- paste0("pct_yr_", age_group)

calle500m <- mzn_buffer %>% 
  st_drop_geometry() %>% 
  drop_na(CodigoCL) %>% #remember!
  #count(CodigoCL) 
  group_by(CodigoCL) %>% 
  summarise(across(-COD_DANE_A, ~sum(.x), .names = "{.col}"), .groups = "drop") %>%    
  #  rowwise(CodigoCL) %>% 
  mutate(
    pct_home = (TP14_1_TIP / TVIVIENDA) * 100, 
    pct_apt = (TP14_2_TIP / TVIVIENDA) * 100,
    pct_unoccu = (TP15_4_OCU / TVIVIENDA) * 100,
    pop_density = (TP27_PERSO / AREA) * 100,
    pct_male = (TP32_1_SEX / TP27_PERSO) * 100,
    pct_female = (TP32_2_SEX / TP27_PERSO) * 100, 
    across(TP34_1_EDA:TP34_9_EDA, ~ (.x/TP27_PERSO)*100, .names = "{age_group_col}")
  ) 

calle500m %>% filter(is.na(CodigoCL))

saveRDS(calle500m, file = "covar_calle500m.rds")

# 2. Link to street-100m buffer-------------------
## create buffer -------------
buffer100m <- st_buffer(calle_geo, dist = 100, endCapStyle = "SQUARE", joinStyle = "BEVEL")

## Join buffer-block ---------------
mzn_buffer100 <- ses2 %>%
  st_transform(crs = st_crs(buffer100m)) %>% 
  st_join(buffer100m, .predicate = st_intersects)

## Sum by st -----------
## Sum by street ------------

covar_calle100m <- mzn_buffer100 %>% 
  st_drop_geometry() %>% 
  drop_na(CodigoCL) %>% #remember!
  #count(CodigoCL) 
  group_by(CodigoCL) %>% 
  summarise(across(-COD_DANE_A, ~sum(.x), .names = "{.col}"), .groups = "drop") %>%    
  #  rowwise(CodigoCL) %>% 
  mutate(
    pct_home = (TP14_1_TIP / TVIVIENDA) * 100, 
    pct_apt = (TP14_2_TIP / TVIVIENDA) * 100,
    pct_unoccu = (TP15_4_OCU / TVIVIENDA) * 100,
    pop_density = (TP27_PERSO / AREA) * 100,
    pct_male = (TP32_1_SEX / TP27_PERSO) * 100,
    pct_female = (TP32_2_SEX / TP27_PERSO) * 100, 
    across(TP34_1_EDA:TP34_9_EDA, ~ (.x/TP27_PERSO)*100, .names = "{age_group_col}")
  ) 

covar_calle100m %>% filter(is.na(CodigoCL))

saveRDS(covar_calle100m, file = "covar_calle100m.rds")
