library(ClustGeo)
library(sf)
library(tidyverse)
library(sfdep)
library(data.table)
#library(ggplot2)
library(patchwork)

sf_use_s2(FALSE)

# 0. import data-----------------
predict24_zat <- readRDS("zat_predict24.rds")

zat_shapefile <- readRDS("../ZAT/zat_shapefile.rds")


# 1. Neighborhood Distance -------------------
## D0 D1 have to be same size, so distance matrix has to follow data dimension
zat_shape_sub <- predict24_zat %>% 
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
dist_link3 <- cppRouting::get_distance_matrix(Graph=graph, 
                                 from = unique(res$from), 
                                 to = unique(res$to))

saveRDS(dist_link3, file = "dist_link3.rds")

#dist_link3.rds saved##

# 1. Clustering ---------------
## D0 ----------
D0 <- dist(predict24_zat)

tree <- hclustgeo(D0)

plot(tree, hang = -1, label = FALSE,
     xlab = "", sub = "",
     main = "Ward dendrogram with D0 only")

rect.hclust(tree, k = 5, border = 1:5)
legend("topright", legend = paste("cluster", 1:5),
       fill = 1:5, bty = "n", border = "white")

## D1 ---------------
D1 <- as.dist(dist_link3)

range.alpha <- seq(0,1, 0.05)
k <- 5

cr <- choicealpha(D0, D1, range.alpha, k, graph = FALSE)

plot(cr)
# a = 0.4

## Mix -------
tree <- hclustgeo(D0, D1, alpha = 0.4)
p5_nbdist <- cutree(tree, 5)

# 2. Summarize zat_cluster -------------------------
pr_zat_cluster <- data.frame(
  ZAT = predict24_zat$ZAT,
  clus = p5_nbdist
)

saveRDS(pr_zat_cluster, file = "pr_zat_cluster.rds")

# 3. Visualize ----------------------
## 3.1 prep data ------------
source("../../functions/get_cluster.R")
source("../../functions/cluster_plot.R")

predict_cluster <- get_cluster(predict24_zat, p5_nbdist)

predict_cluster_geo <- zat_shapefile %>% 
  st_zm() %>% 
  left_join(pr_zat_cluster, by = "ZAT") %>% 
  drop_na(clus)

## 3.2 assemble plots -----------------
pal <- c("#225ea8","#41b6c4","#a1dab4", "#ffdb00","orange")
map <- ggplot()+
  geom_sf(data = zat_shapefile %>% st_zm(), linewidth = 0.1)+
  geom_sf(data = predict_cluster_geo, color = "grey", linewidth = 0.1, aes(fill = factor(clus)))+
  scale_fill_manual(values = pal)+
  labs(fill = "Cluster")+
  theme_minimal()+
  theme(
    panel.grid = element_blank(),
    axis.text = element_blank()
  )

(cluster_plot(predict_cluster) | map ) + 
  plot_annotation('Hierarchical Clustering with Indicators and Neighborhood Constraint', 
                  subtitle = 'AI-detected Street Features, Bogotá',
                  theme=theme(plot.title=element_text(size=14, face = "bold", hjust=0.5),
                              plot.subtitle = element_text(size = 10, face = "bold", hjust = 0.5)))+
  plot_layout(widths = c(1.5, 1), heights = unit(18, units = "cm"))


