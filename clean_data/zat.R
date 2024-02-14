library(sf)
library(skimr)
library(readxl)
library(tidyverse)

# ZAT raw data ------------------------------------------------------------

zat <- st_read("../data/ZAT/ZAT_geo/ZAT.shp")

zat_data <- read_xlsx("../data/ZAT/ZAT_INDICADORES.xlsx")


# georef_zat.rds ----------------------------------------------------------
## geo-referenced ZAT indicators 

georef_zat <- zat_data %>% 
  left_join(zat, by = "ZAT")
#note it'll keep the format of the first argument (zat_data), which is not sf obejct
#but can set if needed

saveRDS(georef_zat, file = "clean_data/ZAT/georef_zat.rds")

georef_zat <- readRDS("../../clean_data/ZAT/georef_zat.rds")

#Hiwot's derived variables:
georef_zat_xtr <- georef_zat %>%
  mutate(STTREESPROAD = NUMSTTREES/LONGMV,
         NUMBRIDGESAREA = NUMBRIDGES/areakm2,
         PEDLIGHTTRAFLIGHT = NUMPTFLIGH/NUMTTFLIGH,
         TRAFLIGHTINTS = NUMPTFLIGH/NUMINT,
         .before = geometry) 

# Process (zat_std2.rds) ---------------------------------------------------
# clean and standardize
## note the zeros whenever you do standardize
zat_clean <- zat_data %>% 
  filter(LRDENS > 0, 
         NUMINT >0)
## went from 919 -> 906 rows

## after standardizing, all count data will not be integers any more
zat_std <- zat_clean %>%
  mutate(
    road_length_log = log(LRDENS),
    st_4ln_length_log = log(LONGMV),
    bikelane_per_km2 = BPRDRATE * LRDENS,
    bikelane_m_log = case_when(bikelane_per_km2 > 0 ~ log(bikelane_per_km2),
                              .default = bikelane_per_km2),
    sttree_per_km2 = NUMSTTREES / areakm2,
    bridg_per_km2 = NUMBRIDGES / areakm2,
    trlight_per_km2 = NUMTTFLIGH / areakm2,
    numrbp_per_km2 = NUMRBP/areakm2,
    numrt_per_km2 = NUMRT/areakm2,
    longrbp_per_km2 = LONGRBP / areakm2,
    longrt_per_km2 = LONGRT / areakm2,
    bus_length_log = case_when(longrbp_per_km2 > 0 ~ log(longrbp_per_km2),
                               .default = longrbp_per_km2),
    brt_length_log = case_when(longrt_per_km2 > 0 ~ log(longrt_per_km2),
                               .default = longrt_per_km2)
  ) 

# select value per km2, length in log
zat_std2 <- zat_std %>% 
  select(ZAT, BUSTOPDENS, road_length_log, st_4ln_length_log, bikelane_m_log, 
    sttree_per_km2, bridg_per_km2, trlight_per_km2, numrbp_per_km2,
    numrt_per_km2, bus_length_log, brt_length_log)

saveRDS(zat_std2, file = "../clean_data/ZAT/zat_std2.rds")

# for shiny -----------------------------------------------------------
## sf object version
zat_indicator_list <- georef_zat_xtr %>% 
  as.data.frame() %>% 
  select(-(1:3), -geometry, -c(MUNCod, NOMMun, UTAM, Area)) %>% 
  colnames()

save(zat_indicator_list, file = "R/zat_indicator_list.rda")

save(georef_zat_xtr, file = "R/georef_zat_xtr.rda")
  
## df version with standardization
zat_indicator_list <- zat_std %>% 
  select(-ZAT) %>% 
  colnames()

save(zat_indicator_list, file = "../analysis/shiny-zat/R/zat_indicator_list.rda")
save(zat_std, file = "../analysis/shiny-zat/R/zat_std.rda")




# correlation ---------------------------------------------------

zat_var <- zat_data %>% 
  select(-1, -2, -3) 

cor_matrix <- cor(zat_var) 

cor_matrix[upper.tri(cor_matrix, diag = TRUE)] <- NA
# >0.6 only those with related variables




# FMM ------------------------------------------------------------
library(flexmix)
library(leaflet)
library(tmap)

## clustering (mix.rds)-----------------------------
##see ZAT_profile.qmd and .rds in clean_data/quarto_zat_profile/

## mapping (zat_cluster.rds)------------------------
zat_std2 <- readRDS("ZAT/zat_std2.rds")

# zat_fmm <- zat_std2 %>% select(-road_length_log, -numrt_per_km2)
# 
# street <- zat_fmm %>% 
#   select(st_4ln_length_log, bikelane_m_log, trlight_per_km2, sttree_per_km2, bridg_per_km2) %>% 
#   colnames()
# 
# transportation <- zat_fmm %>% 
#   select(BUSTOPDENS, bus_length_log, brt_length_log, numrbp_per_km2) %>% 
#   colnames()
# 
# all <- zat_fmm %>% 
#   select(-ZAT) %>% 
#   colnames()
# 
# source("../../../../functions/fmm_normal.R")
# 
# street_mix<- fmm_normal(zat_fmm, street, 1:7)
# transp_mix <- fmm_normal(zat_fmm,transportation, 1:7)
# all_mix <- fmm_normal(zat_fmm, all, 1:7)

all_mix <- readRDS("../clean_data/quarto_zat_profile/all_mix.rds")
street_mix <- readRDS("../clean_data/quarto_zat_profile/street_mix.rds")
transp_mix <- readRDS("../clean_data/quarto_zat_profile/transp_mix.rds")

all_mix_c <- getModel(all_mix, "BIC")
st_mix_c <- getModel(street_mix, "BIC")
tp_mix_c <- getModel(transp_mix, "BIC")

zat_cluster <- zat_std2 %>% 
  select(ZAT) %>% 
  mutate(all = clusters(all_mix_c),
         street = clusters(st_mix_c),
         transp = clusters(tp_mix_c)) %>% 
  left_join(zat, by = "ZAT") %>% 
  select(ZAT, all, street, transp, geometry) %>% 
  st_as_sf()

saveRDS(zat_cluster, file = "zat_cluster.rds")

## in ZAT_profile_map.qmd  
{street <- zat_cluster %>%
  select(ZAT, geometry, street) %>%
  drop_na() %>%
  #using NAD83 in tmap
  tm_shape(projection = sf::st_crs(26915))+
  tm_polygons("street",
    palette = "YlGn",
    style = "cat",
    title = "cluster")+
  tm_layout(panel.show = TRUE,
    panel.labels = "Street Design Profiles",
    panel.label.fontface = "bold",
    title.position = c("left", "TOP"),
    legend.position = c("RIGHT", "bottom"),
    legend.title.size = 0.9,
    legend.width = 2)

transp <- zat_cluster %>%
  select(ZAT, geometry, transp) %>%
  drop_na() %>%
  #using NAD83 in tmap
  tm_shape(projection = sf::st_crs(26915))+
  tm_polygons("transp",
    palette = "YlGn", 
    style = "cat",
    title = "cluster")+
  tm_layout(panel.show = TRUE,
    panel.labels = "Transportation Profiles",
    panel.label.fontface = "bold",
    title.position = c("left", "TOP"),
    legend.position = c("RIGHT", "bottom"),
    legend.title.size = 0.9,
    legend.width = 2)

all_ind <- zat_cluster %>%
  select(ZAT, geometry, all) %>%
  drop_na() %>%
  #using NAD83 in tmap
  tm_shape(projection = sf::st_crs(26915))+
  tm_polygons("all",
    palette = "YlGn", 
    style = "cat",
    title = "cluster")+
  tm_layout(panel.show = TRUE,
    panel.labels = "All-Indicator Profiles",
    panel.label.fontface = "bold",
    title.position = c("left", "TOP"),
    legend.position = c("RIGHT", "bottom"),
    legend.title.size = 0.9,
    legend.width = 2)

plots <- list(street, transp, all_ind)
  
current.mode <- tmap_mode("plot")

tmap_arrange(
    plots,
    nrow = 1,
    width = c(0.34, 0.33, 0.33)
  )
}

## variable impact ------------------------------
library(ggplot2)

zat_cluster <- readRDS("zat_cluster.rds")

zat_cluster_var <- zat_cluster %>%
  as.data.frame() %>% 
  select(-geometry) %>% 
  left_join(zat_std2, by = "ZAT")

street <- c("st_4ln_length_log", "bikelane_m_log", "trlight_per_km2", 
  "sttree_per_km2", "bridg_per_km2") 

transportation <- c("BUSTOPDENS", "bus_length_log", "brt_length_log", "numrbp_per_km2") 

st_var <- zat_cluster_var %>% 
  select(-ZAT, -transp, -all) %>% 
  pivot_longer(-street, names_to = "indicator") %>% 
  filter(indicator%in%c("st_4ln_length_log", "bikelane_m_log", "trlight_per_km2", 
    "sttree_per_km2", "bridg_per_km2")) %>% 
  group_by(street, indicator) %>% 
  mutate(mean = mean(value),
    sd = sd(value))

ggplot(st_var, aes(x= factor(street), y = value, fill = factor(street)))+
  geom_boxplot() +
  scale_fill_brewer(palette = "YlGn") + # Change fill colors
  theme_minimal() + # Use a minimal theme
  labs(title = "Box Plot by Category", 
    x = "Category", 
    y = "Value") +
  facet_wrap(~indicator, scales = "free", nrow = 1)
