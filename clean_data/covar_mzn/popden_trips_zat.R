library(readxl)
library(tidyverse)

# population density---------

pop <- read_xlsx("../../data/pop_zat.xlsx")
zat <- read_xlsx("../../data/ZAT/ZAT_INDICADORES.xlsx")

pop_density <- pop %>% 
  left_join(zat %>% 
      dplyr::select(ZAT, areakm2), 
    by = "ZAT") %>% 
  mutate(pop_density = (POBD2021 / areakm2)*100) %>% 
  dplyr::select(ZAT, pop_density)

saveRDS(pop_density, file = "../../clean_data/ZAT/pop_density2021.rds")


# walking/transit trips (offset)---------
traffic <- readRDS("../../data/zat_denom.rds")

traffic <- traffic %>% 
  mutate(walk_pubt = total_walk + total_pubt)

saveRDS(traffic, file = "../../clean_data/ZAT/walk_pubt.rds")