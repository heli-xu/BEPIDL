library(ClustGeo)
library(sf)
library(tidyverse)
library(readxl)
library(sfdep)
library(data.table)
library(cppRouting)
library(ggplot2)
library(patchwork)

sf_use_s2(FALSE)

# import data --------------------
calle_zat_xwalk <- readRDS("calle_zat_xwalk.rds")
# NA is removed from xwalk
# calle_zat_xwalk <- calle_zat_xwalk %>% 
#   filter(!is.na(ZAT))

#calle to zat w replicate 
calle2_zat_rep <- readRDS("aggr_hclust_geo/calle_2_zat_rep.rds")


zat_shapefile <- readRDS("ZAT/zat_shapefile.rds")

#road type
road_type <- readRDS("road_type/road_type_calle.rds") 

# 1. Neighborhood distances -------------------------
zat_shape_sub <- calle_2_zat_rep %>% 
  pull(ZAT)

centroid <- zat_shapefile %>% 
  st_zm() %>% 
  filter(ZAT%in%zat_shape_sub) %>% 
  st_centroid()

# to find out why centroid has 1 fewer row than zat_shape_sub
# na <- bind_cols(shape = sort(zat_shape_sub[1:863]), centroid = sort(centroid$ZAT))
# everything match... turns out there's an NA in zat_shape_sub!! --fixed from xwalk

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
graph  <-  makegraph(res, directed = F)

## 1.3 calculate distance with network topology (cppRouting package) ----------
dist_link <- get_distance_matrix(Graph=graph, 
                                 from = unique(res$from), 
                                 to = unique(res$to))

#dist_lin2.rds saved##

# 2. Join rd type-ZAT data-----------------
## 2.1 road type in zat--------
road_type_zat <- road_type %>% 
  left_join(calle_zat_xwalk, by = "CodigoCL") %>% 
  mutate(count = 1) %>% 
  group_by(ZAT, MVITCla, road_type) %>% 
  summarize(
    sum = sum(count), .groups = "drop") %>% 
  pivot_wider(id_cols = -MVITCla, 
    names_from = "road_type", 
    values_from = "sum")

road_type %>% filter(is.na(road_type))

road_type_zat_pct <- road_type_zat %>% 
  mutate(across(everything(), ~replace_na(.x, 0))) %>% 
  mutate(
    total = rowSums(pick(Collector:Projected)),
    across(Collector:Projected, ~(.x/total) * 100, .names = "pct_{.col}"),
    pct_other = rowSums(pick(c(pct_Rural, pct_Pedestrian, pct_Unknown, pct_Projected)))
  )

saveRDS(road_type_zat_pct, file = "road_type/road_type_zat_pct.rds")

## 2.2 add to zat data ----------------
calle2zat_rep_rd <- calle2_zat_rep %>% 
  left_join(road_type_zat_pct, by = "ZAT") %>% 
  select(-c(Collector:total, pct_Rural, pct_Pedestrian, pct_Unknown, pct_Projected, pct_other)) %>% 
  ## NOTE to leave out one pct_group to avoid collinearity
  drop_na()

saveRDS(calle2zat_rep_rd, file = "aggr_hclust_geo/rd_type/calle2zat_rep_rd.rds")

# 3. clustering --------------------
## 3.1 cluster number ------------------
D0 <- dist(calle2zat_rep_rd)

tree <- hclustgeo(D0)

plot(tree, hang = -1, label = FALSE,
     xlab = "", sub = "",
     main = "Ward dendrogram with D0 only")

rect.hclust(tree, k = 4, border = 1:4)
legend("topright", legend = paste("cluster", 1:4),
       fill = 1:4, bty = "n", border = "white")

## 3.2 spatial constrainst --------------------
dist_link2 <- readRDS("aggr_hclust_geo/dist_link2.rds")

D1 <- as.dist(dist_link2)

range.alpha <- seq(0,1, 0.05)
k <- 4

cr <- choicealpha(D0, D1, range.alpha, k, graph = FALSE)

plot(cr)
# 0.25 - no road type
# 0.35 - yes road type - with or wo pct_other


tree <- hclustgeo(D0, D1, alpha = 0.35)
p4_nbdist <- cutree(tree, 4)

# 4. visualization ---------------

source("../functions/get_cluster.R")
source("../functions/cluster_plot.R")


## df of zat-cluster, 
## NOTE cluster vector is same order as ZAT column in D0 data, NOT the names of the vector
zat_cluster <- data.frame(
  ZAT = calle2zat_rep_rd$ZAT,
  clus = p4_nbdist
) 

saveRDS(zat_cluster, file = "ZAT/zat_cluster_w_calle_rd_type.rds")

## 4.1 scaled data by cluster------
calle2zat_clust <- get_cluster(calle2zat_rep_rd, p4_nbdist)

# clust_zat <- calle2zat_rep_rd %>% 
#   left_join(zat_cluster, by = "ZAT") 
# 
# x2 <- clust_zat %>% filter(clus == 2) 

## 4.2 cluster geo-----------
calle2zat_geo <- zat_shapefile %>% 
  st_zm() %>% 
  left_join(zat_cluster, by = "ZAT") %>% 
  drop_na(clus)

saveRDS(calle2zat_clust, file = "aggr_hclust_geo/rd_type/calle2zat_clust2.rds")
saveRDS(calle2zat_geo, file = "aggr_hclust_geo/rd_type/calle2zat_geo2.rds")


## 4.3 assemble plots------------
## NOT used: note the `fill` below, tho we ended up using a different route
map <- ggplot()+
  geom_sf(data = zat_shapefile %>% st_zm())+
  geom_sf(data = calle2zat_geo, fill = pal[calle2zat_geo$clus])

# plot(calle2zat_geo$geometry, border = "grey", col  = p4_nbdist,
#      main = "Partition p4_nb obtained with
#          alpha=0.2 and neighborhood dissimilarities")


#read in files above

source("../functions/cluster_plot.R")
pal <- c("#225ea8","#41b6c4","#a1dab4","#fecb3e") 

map <- ggplot()+
  geom_sf(data = zat_shapefile %>% st_zm(), linewidth = 0.1)+
  geom_sf(data = calle2zat_geo, color = "grey", linewidth = 0.1, aes(fill = factor(clus)))+
  scale_fill_manual(values = pal)+
  labs(fill = "Cluster")+
  theme_minimal()+
  theme(
    panel.grid = element_blank(),
    axis.text = element_blank()
  )

(cluster_plot(calle2zat_clust) | map ) + 
  plot_annotation('Hierarchical Clustering with Indicators and Neighborhood Constraint', 
    subtitle = 'ZAT level and aggregated calle level, Bogotá',
    theme=theme(plot.title=element_text(size=14, face = "bold", hjust=0.5),
      plot.subtitle = element_text(size = 10, face = "bold", hjust = 0.5)))+
  plot_layout(widths = c(1.5, 1), heights = unit(10, units = "cm"))


# Extra: traffic flow (total trips) -----------
zat_shapefile <- readRDS("ZAT/zat_shapefile.rds")
total_trips <- readRDS("../data/zat_denom.rds")

walk_transit <- total_trips %>% 
  select(ZAT, total_walk, total_pubt) %>% 
  mutate(flow = total_walk+total_pubt) %>% 
  select(-total_walk, -total_pubt)

walk_transit_geo <- walk_transit %>% 
  left_join(zat_shapefile, by = "ZAT") %>% 
  filter(!st_is_empty(geometry)) %>% 
  st_as_sf() %>% 
  st_zm()

ggplot()+
  geom_sf(data = zat_shapefile %>% st_zm())+
  geom_sf(data = walk_transit_geo, aes(fill = flow))+
  scale_fill_viridis_c()+
  ggtitle("Traffic Flow: Walking + Public Transit Trips")+
  theme(
    title = element_text(face = "bold", size = 10),
    plot.title = element_text(hjust = 0.5)
  )

saveRDS(walk_transit_geo, file = "aggr_hclust_geo/walk_transit_geo.rds")

# Extra: population -----------------------------
pop <- read_excel("../data/pop_zat.xlsx")

zat_pop2021 <- pop %>% 
  select(ZAT, POBD2021) %>% 
  left_join(zat_shapefile, by = "ZAT") %>% 
  filter(!st_is_empty(geometry)) %>%
  st_as_sf() %>% 
  st_zm()


ggplot()+
  geom_sf(data = zat_shapefile %>% st_zm()) +
  geom_sf(data = zat_pop2021, aes(fill = POBD2021))+
  scale_fill_viridis_c()+
  ggtitle("Population") +
  theme(
    title = element_text(face = "bold", size = 12),
    plot.title = element_text(hjust = 0.5)
  )

