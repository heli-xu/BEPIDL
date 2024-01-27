library(sf)
library(dplyr)
library(skimr)
library(readxl)

# ZAT raw data ------------------------------------------------------------

zat <- st_read("data/ZAT/ZAT_geo/ZAT.shp")

zat_data <- read_xlsx("data/ZAT/ZAT_INDICADORES.xlsx")


# Calle raw data ----------------------------------------------------------

calle <- st_read("data/Calles/Calles_datos/Calles_datos.shp")

## skim_calles.csv ------------------------------------------
skim_calles <- skim(calle)
write.csv(skim_calles,"clean_data/skim_calles.csv")


## calles_100.rds ------------------------------------
library(leaflet)

calles_100 = calle |> 
  slice(1:100) |>
  st_transform(crs = st_crs("+proj=longlat +datum=WGS84"))

saveRDS(calles_100, "clean_data/calles/calles_100.rds")

glimpse(sfa)

calles_100 |>
  leaflet() |>
  addTiles() |>
  leaflet::addPolylines()


# georef_zat.rds ----------------------------------------------------------

## geo-referenced ZAT indicators 

georef_zat <- zat_data %>% 
  left_join(zat, by = "ZAT")
#note it'll keep the format of the first argument (zat_data), which is not sf obejct
#but can set if needed

saveRDS(georef_zat, file = "clean_data/ZAT/georef_zat.rds")
