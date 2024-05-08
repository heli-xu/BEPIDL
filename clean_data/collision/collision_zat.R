library(tidyverse)
library(sf)

# get ped collision data --------------------
a <- st_read("../../data/siniestros2015-2019.gdb/")

a2 <- a %>% 
  filter(CLASE_ACC == "ATROPELLO")

saveRDS(a2, "../GitHub/BEPIDL/data/atropello_point_sf.rds")
