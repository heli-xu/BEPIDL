library(sf)
library(skimr)
library(readxl)
library(tidyverse)

# ZAT raw data ------------------------------------------------------------

zat <- st_read("data/ZAT/ZAT_geo/ZAT.shp")

zat_data <- read_xlsx("data/ZAT/ZAT_INDICADORES.xlsx")

# zat_data_xtr.rds --------------------------------------------------------

zat_data_xtr <- zat_data %>% 
  mutate(STTREESPROAD = NUMSTTREES/LONGMV,
         NUMBRIDGESAREA = NUMBRIDGES/areakm2,
         PEDLIGHTTRAFLIGHT = NUMPTFLIGH/NUMTTFLIGH,
         TRAFLIGHTINTS = NUMPTFLIGH/NUMINT) 

saveRDS(zat_data_xtr, file = "clean_data/ZAT/zat_data_xtr.rds")

# georef_zat.rds ----------------------------------------------------------
## geo-referenced ZAT indicators 

georef_zat <- zat_data %>% 
  left_join(zat, by = "ZAT")
#note it'll keep the format of the first argument (zat_data), which is not sf obejct
#but can set if needed

saveRDS(georef_zat, file = "clean_data/ZAT/georef_zat.rds")

georef_zat <- readRDS("../../clean_data/ZAT/georef_zat.rds")

georef_zat_xtr <- georef_zat %>%
  mutate(STTREESPROAD = NUMSTTREES/LONGMV,
         NUMBRIDGESAREA = NUMBRIDGES/areakm2,
         PEDLIGHTTRAFLIGHT = NUMPTFLIGH/NUMTTFLIGH,
         TRAFLIGHTINTS = NUMPTFLIGH/NUMINT,
         .before = geometry) 

# for shiny -----------------------------------------------------------

zat_indicator_list <- georef_zat_xtr %>% 
  as.data.frame() %>% 
  select(-(1:3), -geometry, -c(MUNCod, NOMMun, UTAM, Area)) %>% 
  colnames()

save(zat_indicator_list, file = "R/zat_indicator_list.rda")

save(georef_zat_xtr, file = "R/georef_zat_xtr.rda")
  


# correlation ---------------------------------------------------

zat_var <- zat_data %>% 
  select(-1, -2, -3) 

cor_matrix <- cor(zat_var) 

cor_matrix[upper.tri(cor_matrix, diag = TRUE)] <- NA
# >0.6 only those with related variables

# FMM ------------------------------------------------------------
zat_std <- zat_data %>% 
  mutate(
        road_length_log = log(LRDENS),
         st_4ln_length_log = log(LONGMV),
         tree_per_km2 = NUMTTREES/areakm2,
        bridg_per_km2 = NUMBRIDGES/areakm2,
        trlight_per_int = NUMTTFLIGH/NUMINT,
        bus_rt_length_log = case_when(LONGRBP > 0 ~ log(LONGRBP),
                                      .default = LONGRBP)
         )


  zat_data %>% mutate(
    bus_rt_length_log = case_when(
      LONGRBP > 0 ~ log(LONGRBP),
      .default = LONGRBP))
                                                                                              .default = LONGRBP)
