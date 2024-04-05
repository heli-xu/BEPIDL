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

# calle to zat with replicates -----------------------------
calle_zat_xwalk <- readRDS("calle_zat_xwalk.rds")
# NA is removed from xwalk

# calle_zat_xwalk <- calle_zat_xwalk %>% 
#   filter(!is.na(ZAT))

calle_clean_df <- readRDS("calles/calle_clean_df.rds") 

calle_rename_df <- calle_clean_df %>%   
  rename_with(~ tolower(.), area:X9ceder_el) %>% 
  rename(
    area_calle = area,
    trees = arboles,
    grade = ave_pendie,
    road_width = p_ancho_cl,
    area_roadway = a_calzada,
    area_median = a_separado,
    vehicle_bridge = puente_vh,
    ped_bridge = puente_pt,
    area_sidewalk = a_andenes,
    brt_routes = rutas_trm,
    bus_routes = rutas_sitp,
    bus_stops = parad_sitp,
    bus_lanes = caril_sitp,
    bike_length = largo_cicl,
    road_marks = sen_horizo,
    warning_signs = se_hor_seg,
    road_signs = sen_vert,
    traffic_lights = semaforo,
    road_segments = segme_via,
    speed_limit = velcidad,
    num_lanes_total = sum_carril,
    num_lanes_avg = av_carrile,
   # road_signs_inv = sen_v_inv,
    stop_signs = s_pare_inv,
    lturn_sign = x1_girar_iz,
    bike_signs = x2_ciclov.,
    bus_signs = x3bus_o_tra,
    pedxwalk_signs = x4peatonale,
    speed_bump_signs = x5policiasa,
    stop_signs2 = x6pare,
    parking_signs = x7estaciona,
    school_zone_signs = x8zonas_esc,
    yield_signs = x9ceder_el
  )

rep_calle_zat <- calle_rename_df %>%
  select(CodigoCL, area_calle, area_roadway, area_median, area_sidewalk,
    road_width, road_marks, road_signs, pedxwalk_signs) %>% 
  left_join(calle_zat_xwalk, by = "CodigoCL") %>% 
  drop_na(ZAT) %>% #still need to remove NA here
  uncount(match_n)

rep_calle_zat_agg <- rep_calle_zat %>% 
  group_by(ZAT) %>% 
  summarise(
    across(road_width, ~weighted.mean(.x, w=area_calle), .names = "{.col}"),
    across(-c(CodigoCL, road_width), ~sum(.x), .names = "{.col}")
  )

zat_std2n <- readRDS("ZAT/zat_std2n.rds")

area <- zat_std2n %>%
  select(ZAT, areakm2)

#standardize according to zat-level fashion
#for sum value, adjusted by areakm2,
#for average, leave as is
rep_calle_zat_agg2 <- rep_calle_zat_agg %>% 
  select(-area_calle) %>% 
  left_join(area, by = "ZAT") %>% 
  mutate(
    across(c(area_roadway, area_median, area_sidewalk, 
      road_marks, road_signs, pedxwalk_signs), ~.x/areakm2),
    across(c(area_roadway, area_median, area_sidewalk), ~.x/1000) #to km2
  )


calle_2_zat_rep <- rep_calle_zat_agg2 %>% 
  select(-areakm2) %>% #we don't want this in clustering much
  left_join(zat_std2n %>% select(-areakm2), by = "ZAT") #areakm2 already included

saveRDS(calle_2_zat_rep, file = "aggr_hclust_geo/calle_2_zat_rep.rds")

zat_shape_sub <- calle_2_zat_rep %>% 
  pull(ZAT)

# traffic flow (total trips) -----------
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

# population -----------------------------
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

#dist_lin2.rds saved##

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
dist_link2 <- readRDS("aggr_hclust_geo/dist_link2.rds")

D1 <- as.dist(dist_link2)

range.alpha <- seq(0,1, 0.05)
k <- 4

cr <- choicealpha(D0, D1, range.alpha, k, graph = FALSE)

plot(cr)
# 0.25

tree <- hclustgeo(D0, D1, alpha = 0.25)
p4_nbdist <- cutree(tree, 4)

# visualization ---------------

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

saveRDS(calle2zat_clust, file = "aggr_hclust_geo/calle2zat_clust.rds")
saveRDS(calle2zat_geo, file = "aggr_hclust_geo/calle2zat_geo.rds")

#read in files above

source("../functions/cluster_plot.R")
# modify the ncol of facet
cluster_plot <- function(cluster_data) {
  cluster_data %>% 
    ggplot() +
    geom_col(aes(x = factor(indicator), y = scaled, fill = clus)) +
    scale_fill_manual(values = pal)+
    coord_flip() + 
    geom_hline(yintercept = 0, linetype = "dotted")+ #still set as y (although it's after flipping)
    theme_minimal() +
    labs(y = "Relative Less   Relative More",
      x = "") +
    scale_x_discrete(expand = expansion(mult = 0.002),
      labels = c(
        "mean_BUSTOPDENS"="Bus stop density" ,
        "mean_road_length_log" ="Road length",
        "mean_st_4ln_length_log" ="Street length",
        "mean_bikelane_m_log" ="Bikelane length",
        "mean_sttree_per_km2" ="Tree",
        "mean_bridg_per_km2" ="Bridge",
        "mean_trlight_per_km2" ="Traffic light" ,
        "mean_numrbp_per_km2" ="Bus route count" ,
        "mean_numrt_per_km2" ="BRT route count",
        "mean_bus_length_log" ="Bus route length",
        "mean_brt_length_log"="Bus route length",
        "mean_road_width"="Road width (st)",
        # "mean_av_carrile"="Average lane",
        "mean_area_roadway"="Roadway area (st)",
        "mean_area_median"="Median area (st)",
        "mean_area_sidewalk"="Sidewalk area (st)",
        "mean_INTDENS"="Intersection density",
        "mean_road_marks" ="Road marks (st)",
        "mean_road_signs" = "Road signs (st)",
        "mean_pedxwalk_signs"="Ped crosswalk sign (st)"
      ))+
    theme(panel.grid = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_text(size = 9),
      axis.line.x = element_line(arrow = grid::arrow(length = unit(0.3, "cm"), 
        ends = "both")),
      plot.margin=grid::unit(c(0,0,0,0), "mm"),
      strip.text = element_text(face = "bold", size = 10),
      strip.background = element_rect(fill = "grey", color = "white"),
      panel.spacing.x = unit(1, "lines")) +
    facet_wrap(~clus, ncol = 2)
}

map <- ggplot()+
  geom_sf(data = zat_shapefile %>% st_zm())+
  geom_sf(data = calle2zat_geo, aes(fill = factor(clus)))+
  scale_fill_manual(values = pal)+
  labs(fill = "Cluster")

(cluster_plot(calle2zat_clust) | map ) + 
  plot_annotation('Hierarchical Clustering with Indicators and Neighborhood Constraint', 
    subtitle = 'ZAT level and aggregated calle level, Bogotá',
    theme=theme(plot.title=element_text(size=14, face = "bold", hjust=0.5),
      plot.subtitle = element_text(size = 10, face = "bold", hjust = 0.5)))+
  plot_layout(heights = unit(12, units = "cm"))
