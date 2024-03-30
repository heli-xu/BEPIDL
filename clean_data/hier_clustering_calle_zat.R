library(ClustGeo)
library(sf)
library(tidyverse)
library(readxl)
library(sfdep)
library(data.table)
library(cppRouting)
sf_use_s2(FALSE)


calle_zat_xwalk <- readRDS("calle_zat_xwalk.rds")
# NA is removed from xwalk

# calle_zat_xwalk <- calle_zat_xwalk %>% 
#   filter(!is.na(ZAT))

calle_clean_df <- readRDS("calles/calle_clean_df.rds")

rep_calle_zat <- calle_clean_df %>%
  select(CodigoCL, area, A_Calzada, A_separado, A_andenes, P_Ancho_Cl,
         av_carrile) %>% 
  left_join(calle_zat_xwalk, by = "CodigoCL") %>% 
  drop_na(ZAT) %>% #still need to remove NA here
  uncount(match_n)

rep_calle_zat_agg <- rep_calle_zat %>% 
  group_by(ZAT) %>% 
  summarise(
    across(c(P_Ancho_Cl, av_carrile), ~weighted.mean(.x, w=area), .names = "{.col}"),
    across(-c(CodigoCL, P_Ancho_Cl, av_carrile), ~sum(.x), .names = "{.col}")
  )

zat_std2n <- readRDS("ZAT/zat_std2n.rds")

area <- zat_std2n %>%
  select(ZAT, areakm2)

#standardize according to zat-level fashion
#for sum value, adjusted by areakm2,
#for average, leave as is
rep_calle_zat_agg2 <- rep_calle_zat_agg %>% 
  select(-area) %>% 
  left_join(area, by = "ZAT") %>% 
  mutate(
    across(c(A_Calzada, A_separado, A_andenes), ~.x/areakm2),
    across(c(A_Calzada, A_separado, A_andenes), ~.x/1000) #to km2
  )


calle_2_zat_rep <- rep_calle_zat_agg2 %>% 
  select(-areakm2) %>% #we don't want this in clustering much
  left_join(zat_std2n %>% select(-areakm2), by = "ZAT") #areakm2 already included

zat_shape_sub <- calle_2_zat_rep %>% 
  pull(ZAT)

# traffic flow (total trips) -----------
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
  scale_fill_viridis_c()


# population -----------------------------
pop <- read_excel("../data/pop_zat.xlsx")

zat_pop2021 <- pop %>% 
  select(ZAT, POBD2021)

# neighborhood distances -------------------------
zat_shapefile <- readRDS("ZAT/zat_shapefile.rds")

centroid <- zat_shapefile %>% 
  st_zm() %>% 
  filter(ZAT%in%zat_shape_sub) %>% 
  st_centroid()

# to find out why centroid has 1 fewer row than zat_shape_sub
# na <- bind_cols(shape = sort(zat_shape_sub[1:863]), centroid = sort(centroid$ZAT))
# everything match... turns out there's an NA in zat_shape_sub!!

nb <- st_contiguity(zat_shapefile %>% 
                    filter(ZAT%in%zat_shape_sub) %>% 
                      st_zm(), snap = 0.005)

dist_nb <- st_nb_dists(centroid, nb)  

#check <- st_distance(st_geometry(centroid)[1],st_geometry(centroid)[2])

library(sp)
plot(as_Spatial(zat_shapefile %>% st_zm()), main = "Neighborhood Network")
plot(nb, coords = coordinates(as_Spatial(centroid)), col="blue", add = TRUE)

# 1. make a df of from-to-distance
n = length(dist_nb)
res = data.table(from = integer(), to = integer(), dist = numeric())
for(i in seq_len(n)){
  res = rbind(res, data.table(from = i, to = nb[[i]], dist = dist_nb[[i]]))
}

# 2. create network with cppRouting package
graph  <-  makegraph(res, directed = F)

# 3. calculate distance with network topology (cppRouting package)
dist_link <- get_distance_matrix(Graph=graph, 
                                 from = unique(res$from), 
                                 to = unique(res$to))

# clustering --------------------
## cluster number ------------------
D0 <- dist(calle_2_zat_rep)

tree <- hclustgeo(D0)

plot(tree, hang = -1, label = FALSE,
     xlab = "", sub = "",
     main = "Ward dendrogram with D0 only")

rect.hclust(tree, k = 4, border = 1:4)
legend("topright", legend = paste("cluster", 1:4),
       fill = 1:4, bty = "n", border = "white")

## spatial constrainst --------------------
D1 <- as.dist(dist_link2)

range.alpha <- seq(0,1, 0.05)
k <- 4

cr <- choicealpha(D0, D1, range.alpha, k, graph = FALSE)

plot(cr)
# 0.4

tree <- hclustgeo(D0, D1, alpha = 0.4)
p4_nbdist <- cutree(tree, 4)

# visualization ---------------
library(ggplot2)
library(patchwork)

source("../functions/get_cluster.R")
source("../functions/cluster_plot.R")
pal <- c("#225ea8","#41b6c4","#a1dab4","#fecb3e") 

calle2zat_clust <- get_cluster(calle_2_zat_rep, p4_nbdist)

calle2zat_geo <- zat_shapefile %>% 
  st_zm() %>% 
  filter(ZAT%in%zat_shape_sub) %>% 
  mutate(clus = p4_nbdist)

map <- ggplot()+
  geom_sf(data = zat_shapefile %>% st_zm())+
  geom_sf(data = calle2zat_geo, fill = pal[calle2zat_geo$clus])
  

(cluster_plot(calle2zat_clust) | map ) + 
  plot_annotation('Hierarchical Clustering with Indicators and Neighborhood Constraint', 
                  subtitle = 'ZAT level and aggregated calle level, Bogotá',
                  theme=theme(plot.title=element_text(size=14, face = "bold", hjust=0.5),
                              plot.subtitle = element_text(size = 10, face = "bold", hjust = 0.5)))+
  plot_layout(widths = c(1,1.5), heights = unit(15, units = "cm"))

# plot(calle2zat_geo$geometry, border = "grey", col  = p4_nbdist,
#      main = "Partition p4_nb obtained with
#          alpha=0.2 and neighborhood dissimilarities")
