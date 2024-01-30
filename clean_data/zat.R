library(sf)
library(dplyr)
library(skimr)
library(readxl)

# ZAT raw data ------------------------------------------------------------

zat <- st_read("data/ZAT/ZAT_geo/ZAT.shp")

zat_data <- read_xlsx("data/ZAT/ZAT_INDICADORES.xlsx")


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

# for shinylive -----------------------------------------------------------

zat_indicator_list <- georef_zat_xtr %>% 
  as.data.frame() %>% 
  select(-(1:3), -geometry, -c(MUNCod, NOMMun, UTAM, Area)) %>% 
  colnames()

save(zat_indicator_list, file = "R/zat_indicator_list.rda")

save(georef_zat_xtr, file = "posts/shinylive-zat/georef_zat_xtra.rda")
  
zat_indicator_list
