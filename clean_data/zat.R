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
## df version with standardization
zat_indicator_list <- zat_std %>% 
  select(-ZAT) %>% 
  colnames()

save(zat_indicator_list, file = "../analysis/shiny-zat/R/zat_indicator_list.rda")
save(zat_std, file = "../analysis/shiny-zat/R/zat_std.rda")

## sf object with std with variable descriptioin
library(readr)

zat_codebook <- read_csv("../reading/Codebook Final/zat_codebook.csv")

zat_std_geo <- zat_std %>% 
  left_join(zat, by = "ZAT") %>% 
  st_as_sf()

save(zat_std_geo, file = "../analysis/shiny-zat/R/zat_std_geo.rda")

## can use gpt to generate string
xtr_var <- c('road_length_log', 'st_4ln_length_log', 'bikelane_per_km2', 'bikelane_m_log', 
             'sttree_per_km2', 'bridg_per_km2', 'trlight_per_km2', 'numrbp_per_km2',
             'numrt_per_km2', 'longrbp_per_km2', 'longrt_per_km2', 'bus_length_log', 'brt_length_log')

descript <- c(
  'Natural logarithm of road network length (LRDENS)',
  'Natural logarithm of the length of streets with 4 or more lanes (LONGMV)',
  'Bikelane length in meters per square kilometer, calculated as the product of BPRDRATE and LRDENS',
  'Natural logarithm of bikelane length per square kilometer (bikelane_per_km2) if value > 0, otherwise keep the value as 0',
  'Street tree per square kilometer (NUMSTTREES / areakm2)',
  'Pedestrian bridge per square kilometer (NUMBRIDGES / areakm2)',
  'Total traffic light per square kilometer (NUMTTFLIGH / areakm2)',
  'Number of bus routes per square kilometer (NUMRBP / areakm2)',
  'Number of rail transit routes per square kilometer (NUMRT / areakm2)',
  'Length of bus routes per square kilometer (LONGRBP / areakm2), keeping the value as 0 if length is zero ',
  'Length of rail transit stops per square kilometer, with a default value if length is zero (LONGRT / areakm2)', 
  'Natural logarithm of length of bus routes per square kilometer (logarithm of longrbp_per_km2), keeping the value as 0 if length is zero',
  'Natural logarithm of length of rail transit routes per square kilometer (calculated as the logarithm of longrt_per_km2), keeping the value as 0 if length is zero'
)

extra <- data.frame(var = xtr_var, description = descript)

zat_codebook2 <- zat_codebook %>% 
  select(var = zat_old, description) %>% 
  rbind(extra)

save(zat_codebook2, file = "../analysis/shiny-zat/R/zat_indicator_table.rda")
#incorrect, obj name gotta match the rda name!
#it's now matched in file 

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

## mapping (zat_cluster.rds)------------------------
zat_std2 <- readRDS("clean_data/ZAT/zat_std2.rds")

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

zat_cluster <- readRDS("clean_data/zat_cluster.rds")

zat_cluster_var <- zat_cluster %>%
  as.data.frame() %>% 
  select(-geometry) %>% 
  left_join(zat_std2, by = "ZAT")

# street <- c("st_4ln_length_log", "bikelane_m_log", "trlight_per_km2", 
#   "sttree_per_km2", "bridg_per_km2") 
# 
# transportation <- c("BUSTOPDENS", "bus_length_log", "brt_length_log", "numrbp_per_km2") 

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
    y = "Value",
    fill = "cluster") +
  facet_wrap(~indicator, scales = "free", nrow = 1)

transp_var <- zat_cluster_var %>% 
  select(-ZAT, -street, -all) %>% 
  pivot_longer(-transp, names_to ="indicator") %>% 
  filter(indicator%in%c("BUSTOPDENS", "bus_length_log", "brt_length_log", "numrbp_per_km2")) %>% 
  group_by(transp, indicator) %>% 
  mutate(mean = mean(value),
         sd = sd(value))

ggplot(transp_var, aes(x= factor(transp), y = value, fill = factor(transp)))+
  geom_boxplot() +
  scale_fill_brewer(palette = "YlGn") + # Change fill colors
  theme_minimal() + # Use a minimal theme
  labs(title = "Box Plot by Category", 
       x = "Category", 
       y = "Value",
       fill = "cluster") +
  facet_wrap(~indicator, scales = "free", nrow = 1)

zat_imp_geo <- zat %>% 
  select(ZAT, geometry) %>% 
  left_join(zat_std2 %>% 
      select(ZAT, st_4ln_length_log, trlight_per_km2, bus_length_log),
    by = "ZAT") %>% 
  drop_na()

saveRDS(zat_imp_geo, "../clean_data/zat_imp_geo.rds")

# Hierarchical clustering ------
## hclust ------------------------
library(leaflet)
library(glue)
dist(zat_std2)

hc <- hclust(dist(zat_std2), "ward.D2")

zat_cluster2 <- zat_cluster %>% 
  mutate(ward_D2 = cutree(hc, k =3))

#map
pal <- colorFactor(
  palette = "YlGn",
  domain = zat_cluster2$ward_D2
)

zat_label <- glue("ZAT{zat_cluster2$ZAT} FMM cluster: {zat_cluster2$all}")

zat_cluster2 %>% 
  select(ZAT, ward_D2) %>% 
  st_zm() %>%
  st_transform(crs = st_crs("+proj=longlat +datum=WGS84")) %>%
  leaflet() %>%
  addProviderTiles(providers$CartoDB.Voyager)  %>%
  addPolygons(color = "white", 
              weight = 0.5,
              smoothFactor = 0.5,
              opacity = 1,
              fillColor = ~pal(ward_D2),
              fillOpacity = 0.8,
              highlightOptions = highlightOptions(
                weight = 5,
                color = "#666",
                fillOpacity = 0.8,
                bringToFront = TRUE),
              label = zat_label,
              labelOptions = labelOptions(
                style = list(
                  "font-family" = "Fira Sans, sans-serif",
                  "font-size" = "1.2em"
                ))
  )%>% 
  addLegend("bottomleft",
            pal = pal,
            values = ~ward_D2,
            title = "Hierarchical Clustering",
            opacity = 1)

## SKATER (spatial constrained) -------------------
library(spdep)
sf_use_s2(FALSE) 

zat_std2_sf <- zat_std2 %>% 
  left_join(zat_cluster, by = "ZAT") %>% 
  select(-all, -street, -transp) %>% 
  st_as_sf() %>% 
  st_zm() #sp doesnot support polygon z dimenstion

#scale
zat_scaled <- zat_std2 %>% 
  column_to_rownames(var = "ZAT") %>% 
  select(1:11) %>% 
  scale() %>% 
  as.data.frame()

zat_nb <- poly2nb(zat_std2_sf) 

plot(as_Spatial(zat_std2_sf), main = "no snap")
plot(zat_nb, coords = coordinates(as_Spatial(zat_std2_sf)), col="blue", add = TRUE)

#summary(zat_nb)
#some zat with no links with cause error in costs
no_link <- c(24, 113, 124, 143, 184, 399, 538, 613, 754, 755, 758, 766, 767, 774, 775, 778, 820, 893)

# to find out what are the no-linked ZAT 
#--> they don't look that "isolated"! -- adjust the `snap` argument
# no_link_zat <- zat_std2_sf %>% 
#   filter(ZAT %in% no_link) %>% 
#   st_transform(crs = st_crs("+proj=longlat +datum=WGS84")) 
# 
# link_zat <- zat_std2_sf %>% 
#   filter(!ZAT %in% no_link) %>% 
#   st_transform(crs = st_crs("+proj=longlat +datum=WGS84")) 
# 
# leaflet() %>% 
#   addTiles() %>% 
#   addPolygons(data=link_zat, weight = 3, fillColor = 'purple', color = 'white', fillOpacity = 1) %>%
#   addPolygons(data=no_link_zat, weight = 3, fillColor = 'blue', color = 'blue') 


zat_nb2 <- poly2nb(zat_std2_sf, snap = 0.005) 
#tried 0.001 --> 2 disjoint connected subgraphs (not sure why)
summary(zat_nb2)

plot(as_Spatial(zat_std2_sf), main = "snap0.005")
plot(zat_nb2, coords = coordinates(as_Spatial(zat_std2_sf)), col="blue", add = TRUE)

costs <- nbcosts(zat_nb2, data = zat_scaled)

weights <- nb2listw(zat_nb2, costs, style = "B")

#minimal spanning tree
zat_mst <- mstree(weights)

clust4 <- skater(edges = zat_mst[,1:2], data = zat_scaled, ncuts=3)

plot((zat_std2_sf %>% mutate(clus = clust4$groups))['clus'], main = "4 cluster example")

# Soft spatial constrain -----------------------
library(ClustGeo)
## partition with no constraint
D0 <- dist(zat_std2)
tree <- hclustgeo(D0)
plot(tree, hang = -1, label = FALSE,
    xlab = "", sub = "",
    main = "Ward dendrogram with D0 only")
# The height corresponds to the increase in the total within-cluster variance
# after merging two clusters.

# k =4 is chosen
rect.hclust(tree, k = 4, border = 1:4)
legend("topright", legend = paste("cluster", 1:4),
  fill = 1:4, bty = "n", border = "white")
# partition
p4 <- cutree(tree, 4)
plot(zat_std2_sf$geometry, border = "grey", col = p4,
    main = "Partition p4 obtained from D0 only")

# 
# ggplot()+
#   geom_sf(data = zat_std2_sf$geometry, aes(fill= p4))+
#   scale_color_viridis_d(option = "magma")

#c("#7e549e","#c2549d","#fc8370","#fecb3e")

## consider geographic constraint
dist_zat <- st_distance(zat_cluster)
