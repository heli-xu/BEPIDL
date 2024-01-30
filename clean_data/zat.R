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



