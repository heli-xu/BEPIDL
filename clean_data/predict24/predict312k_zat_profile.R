library(ClustGeo)
library(sf)
library(tidyverse)
library(sfdep)
library(data.table)
#library(ggplot2)
library(patchwork)

sf_use_s2(FALSE)  #IMPORTANT! affect nb network

# 0. import data-----------------
#updated prediction Nov2024, year 2015-19 only
predict24_zat_312k <- readRDS("predict_312k/zat_predict312k_1519.rds")

#prediction with road network characteristics
#predict24_road_zat <- readRDS("w_road_info/predict24_road_info_zat.rds")

zat_shapefile <- readRDS("../ZAT/zat_shapefile.rds")

## with some var from calle
calle_indicators <- readRDS("../aggr_hclust_geo/calle_2_zat_rep.rds")

## with some var from zat
zat_indicators <- readRDS("../ZAT/zat_std2n.rds")

dist_link4 <- readRDS("predict_312k/dist_link4.rds")

# if loading dist_link4.rds, skip 1.
#remember-if zat dataset changes, must redo distance matrix to keep same dim.

# 1. Neighborhood Distance -------------------
## D0 D1 have to be same size, so distance matrix has to follow data dimension
zat_shape_sub <- predict24_zat_312k %>% 
  pull(ZAT)

centroid <- zat_shapefile %>% 
  st_zm() %>% 
  filter(ZAT%in%zat_shape_sub) %>% 
  st_centroid()


nb <- st_contiguity(zat_shapefile %>% 
                      filter(ZAT%in%zat_shape_sub) %>% 
                      st_zm(), snap = 0.005)

dist_nb <- st_nb_dists(centroid, nb)  

#check <- st_distance(st_geometry(centroid)[1],st_geometry(centroid)[2])

library(sp)
plot(as_Spatial(zat_shapefile %>% st_zm()), main = "Neighborhood Network")
plot(nb, coords = coordinates(as_Spatial(centroid)), col="blue", add = TRUE)

## 1.1 make a df of from-to-distance --------------
n = length(dist_nb)
res = data.table(from = integer(), to = integer(), dist = numeric())
for(i in seq_len(n)){
  res = rbind(res, data.table(from = i, to = nb[[i]], dist = dist_nb[[i]]))
}

## 1.2 create network with cppRouting package ---------------
graph  <-  cppRouting::makegraph(res, directed = F)

## 1.3 calculate distance with network topology (cppRouting package) ----------
dist_link4 <- cppRouting::get_distance_matrix(Graph=graph, 
                                 from = unique(res$from), 
                                 to = unique(res$to))

saveRDS(dist_link4, file = "predict_312k/dist_link4.rds")

#dist_link4.rds saved##

# 2. Add road characteristics---------------------
predict24_road_zat <- predict24_zat_312k %>% 
  left_join(calle_indicators %>% 
      select(ZAT, road_width, area_roadway), by = "ZAT") %>% 
  left_join(zat_indicators %>% 
      select(ZAT, road_length_log, INTDENS), by = "ZAT")

saveRDS(predict24_road_zat, file = "predict_312k/zat_predict312k_1519_road_info.rds")

# 3. Clustering ---------------
## D0: indicators ----------
D0 <- dist(predict24_road_zat)

tree <- hclustgeo(D0)

plot(tree, hang = -1, label = FALSE,
     xlab = "", sub = "",
     main = "Ward dendrogram with D0 only")

rect.hclust(tree, k = 4, border = 1:4)
legend("topright", legend = paste("cluster", 1:4),
       fill = 1:4, bty = "n", border = "white")

## D1: nb distance---------------
D1 <- as.dist(dist_link4)

range.alpha <- seq(0,1, 0.05)
k <- 4

cr <- choicealpha(D0, D1, range.alpha, k, graph = FALSE)

plot(cr)
# a = 0.3

## Mix -------
tree <- hclustgeo(D0, D1, alpha = 0.3)
p4_nbdist <- cutree(tree, 4)

# 4. Summarize zat_cluster -------------------------
pr_zat_cluster <- data.frame(
  ZAT = predict24_road_zat$ZAT,
  clus = p4_nbdist
)

saveRDS(pr_zat_cluster, file = "predict_312k/profile_w_road_info/pr312k_zat_cluster_rd.rds")

# 5. Visualize ----------------------
## 5.1 prep data ------------
source("../../functions/get_cluster.R")
source("../../functions/cluster_plot.R")

predict_cluster <- get_cluster(predict24_road_zat, p4_nbdist)

predict_cluster_geo <- zat_shapefile %>% 
  st_zm() %>% 
  left_join(pr_zat_cluster, by = "ZAT") %>% 
  drop_na(clus)

saveRDS(predict_cluster, file = "predict_312k/profile_w_road_info/predict312k_clus_scaled.rds")

saveRDS(predict_cluster_geo, file = "predict_312k/profile_w_road_info/predict312k_clus_geo.rds")
#clus_geo ignored, need rerunning for new device

## 5.2 assemble plots -----------------
pal <- c("#225ea8","#41b6c4","#a1dab4", "#ffdb00")
map <- ggplot()+
  geom_sf(data = zat_shapefile %>% st_zm(), linewidth = 0.1)+
  geom_sf(data = predict_cluster_geo, color = "grey", linewidth = 0.1, aes(fill = factor(clus)))+
  scale_fill_manual(values = pal)+
  labs(fill = "Cluster")+
  theme_minimal()+
  theme(
    panel.grid = element_blank(),
    axis.text = element_blank()
    # legend.position = "inside",
    # legend.position.inside = c(0.9,0.2),
    # legend.title.position = "top"
  )

### plot all indicators together---------
var_plot <- cluster_plot(predict_cluster)

(var_plot | map ) + 
  plot_annotation('AI-detected Street Features and Selected Road Network Characteristics, Bogotá', 
                  theme=theme(plot.title=element_text(size=14, face = "bold", hjust=0.5),
                              plot.subtitle = element_text(size = 10, face = "bold", hjust = 0.5)))+
  plot_layout(widths = c(1.5, 1), heights = unit(18, units = "cm"))

### *separate road characteristics and features-------
var_plot1 <- predict_cluster %>% 
  filter(str_detect(indicator, "width|length|INTDENS|area")) %>% 
  cluster_plot()+
  labs(
    y = "",
    title = "Road characteristics"
  )+
  theme(
    plot.title = element_text(size = 10)
  )

var_plot2 <- predict_cluster %>% 
  filter(!str_detect(indicator, "width|length|INTDENS|area")) %>% 
  cluster_plot()+
  labs(
    title = "Street features"
  )+
  theme(
    plot.title = element_text(size = 10)
  )


(var_plot2 | (var_plot1/map + plot_layout(heights = c(1, 3))))+ 
  plot_annotation('Neighborhood (ZAT) Built Environment Profiles, Bogotá',
    subtitle = "Based on AI-detected Street Features and Selected Road Network Characteristics (2015-2019)",
    theme=theme(plot.title=element_text(size=14, face = "bold", hjust=0.5),
    plot.subtitle=element_text(size = 11, face = "bold", hjust = 0.5))
    )+
  plot_layout(widths = c(1.5, 1), heights = unit(16, units = "cm"))

# 6. summary table (feature + clus)--------
predict312k_fea_clus <- pr312k_zat_cluster_rd |> 
  left_join(predict24_zat_312k, by = "ZAT")

write.csv(predict312k_fea_clus, file = "predict_312k/zat_pr312k_all.csv")
