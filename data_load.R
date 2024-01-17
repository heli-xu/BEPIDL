library(sf)
library(dplyr)
library(skimr)
library(readxl)

zat <- st_read("data/ZAT/ZAT.shp")

calle <- st_read("data/Calles/Calles_datos/Calles_datos.shp")

skim_calles <- skim(calle)

write.csv(skim_calles,"data/skim_calles.csv")

zat_data <- read_xlsx("data/ZAT_INDICADORES.xlsx")

skim_zat <- skim(zat_data)