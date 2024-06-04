library(tidyverse)
library(sf)
library(leaflet)
library(patchwork)

# import data -------------
road_type <- readRDS("../road_type/road_type_calle.rds")
calle_clean_df <- readRDS("calles/calle_clean_df.rds")

# Join collision  
road_type_geo <- road_type %>% 
  left_join(calle_geo, by= "CodigoCL") %>% 
  st_as_sf()

  
injury <- calle_clean_df %>% 
  select(CodigoCL, sini_herid, sini_muert, si_act_pea) %>% 
  left_join(road_type_geo, by = "CodigoCL") %>% 
  drop_na(MVITCla) %>% 
  mutate(victim = sini_herid + sini_muert) %>% 
  st_as_sf()

## recode road type --------------------------
injury <- injury %>% 
  mutate(
    rd_type = case_match(
      MVITCla,
      1 ~ "Arterial",
      2 ~ "Collector",
      3 ~ "Local",
      .default = "Other"
    )) %>% 
  distinct()  # in joining to  zat --> many replicates somehow

#predict_gis_clean2 <- readRDS("MLdata_GIS/predict_gis_clean2.rds")

# 
# ped_injury <- predict_gis_clean2 %>% 
#   select(codigocl, sini_herid, sini_muert, si_act_pea, road_type) %>% 
#   mutate(victim = sini_herid + sini_muert) %>% 
#   rename(CodigoCL = codigocl) %>% 
#   left_join(calle_geo, by = "CodigoCL") %>% 
#   st_as_sf()

# viz with cluster profile ------------------------------

calle2zat_geo <- readRDS("aggr_hclust_geo/calle2zat_geo.rds")


pal_vict <- colorNumeric(
  palette = "plasma",
  domain = injury$victim
)

pal_clus <- colorFactor(
  palette = "YlGnBu",
  domain =calle2zat_geo$clus
)

injury %>% 
  st_transform(crs = st_crs("+proj=longlat +datum=WGS84")) %>% 
  leaflet() %>% 
  addProviderTiles(providers$CartoDB.Voyager) %>%
  addPolygons(
    data = calle2zat_geo %>% 
      st_transform(crs = st_crs("+proj=longlat +datum=WGS84")),
    fillColor = ~pal_clus(clus),
    fillOpacity = 0.9,
    weight = 0.5, 
    color = "grey"
  ) %>% 
  addPolygons(
    weight = 2, 
    fillColor = ~pal_vict(victim), 
    fillOpacity = 1,
    color = ~pal_vict(victim)) %>% 
  addLegend(
    "bottomright",
    pal = pal_vict,
    values = ~victim,
    title = "Accidents with <br>injured/fatal victims"
  ) %>% 
  addLegend(
    "bottomleft",
    pal = pal_clus,
    values = ~calle2zat_geo$clus,
    title = "Cluster"
  )

pal_ped <- colorNumeric(
  palette = "plasma",
  domain = injury$si_act_pea
)

injury %>% 
  st_transform(crs = st_crs("+proj=longlat +datum=WGS84")) %>% 
  leaflet() %>% 
  addProviderTiles(providers$CartoDB.Voyager) %>%
  addPolygons(
    data = calle2zat_geo %>% 
      st_transform(crs = st_crs("+proj=longlat +datum=WGS84")),
    fillColor = ~pal_clus(clus),
    fillOpacity = 0.8,
    weight = 0.5, 
    color = "grey"
  ) %>% 
  addPolygons(
    weight = 2, 
    fillColor = ~pal_ped(si_act_pea), 
    fillOpacity = 1,
    color = ~pal_ped(si_act_pea)) %>% 
  addLegend(
    "bottomright",
    pal = pal_ped,
    values = ~si_act_pea,
    title = "Pedestrian Incidents"
  ) %>% 
  addLegend(
    "bottomleft",
    pal = pal_clus,
    values = ~calle2zat_geo$clus,
    title = "Cluster"
  )

# road type plot
pal_rd <- colorFactor(
  palette = "PuOr",
  domain = injury2$rd_type
)

injury2 %>% 
  st_transform(crs = st_crs("+proj=longlat +datum=WGS84")) %>% 
  leaflet() %>% 
  addProviderTiles(providers$CartoDB.Voyager) %>%
  addPolygons(
    data = calle2zat_geo %>% 
      st_transform(crs = st_crs("+proj=longlat +datum=WGS84")),
    fillColor = ~pal_clus(clus),
    fillOpacity = 0.8,
    weight = 0.5, 
    color = "grey"
  ) %>% 
  addPolygons(
    weight = 2, 
    fillColor = ~pal_rd(rd_type), 
    fillOpacity = 1,
    color = ~pal_rd(rd_type)) %>% 
  addLegend(
    "bottomright",
    pal = pal_rd,
    values = ~rd_type,
    title = "Road Type"
  ) %>% 
  addLegend(
    "bottomleft",
    pal = pal_clus,
    values = ~calle2zat_geo$clus,
    title = "Cluster"
  )

# aggregate ----------------------------
xwalk <- readRDS("calle_zat_xwalk.rds")
zat_shapefile <- readRDS("zat/zat_shapefile.rds")

repeated_calle <- injury %>% as.data.frame() %>% count(CodigoCL) %>% 
  filter(n >1) %>% 
  pull(CodigoCL)

calle_geo %>% 
  filter(CodigoCL%in%repeated_calle) %>% 
  st_transform(crs = st_crs("+proj=longlat +datum=WGS84")) %>% 
  leaflet() %>% 
  addTiles() %>% 
  addPolygons(
    weight = 1, fillColor = 'blue', color = 'blue')

injury2 <- injury %>% 
  distinct(CodigoCL, .keep_all = TRUE)
  
zat_injury <- injury2 %>%
  st_drop_geometry() %>% 
  left_join(xwalk, by = "CodigoCL") %>% 
  drop_na(ZAT) %>% 
  uncount(match_n) %>% 
  group_by(ZAT) %>% 
  summarise(
    across(c(sini_herid,sini_muert, si_act_pea, victim),
      ~sum(.), .names = "{.col}") 
  ) %>% 
  left_join(zat_shapefile, by = "ZAT") %>% 
  st_as_sf() %>% 
  st_zm() #remember this whenever zat shapefiles come up

base_map <- ggplot()+
  geom_sf(data = zat_shapefile %>% st_zm(), color = "grey")

pal <- c("#225ea8","#41b6c4","#a1dab4","#fecb3e") 
map_clust <- base_map +
  geom_sf(data = calle2zat_geo, aes(fill = factor(clus)))+
  scale_fill_manual(values = pal) +
  labs(
    fill = "Cluster"
  )+
  theme_minimal()+  # note the order of adjusting theme
  theme(
    legend.position = "inside",
    legend.position.inside = c(0.9, 0.2),
    axis.text = element_blank(),
    panel.grid = element_blank()
  )

map_victim <- base_map+
  geom_sf(data = zat_injury, aes(fill = victim))+
  scale_fill_viridis_c(option = "plasma")+
  labs(fill = "Accidents \nwith Victims")+
  theme_minimal()+  # note the order of adjusting theme
  theme(
    legend.position = "inside",
    legend.position.inside = c(0.9, 0.2),
    axis.text = element_blank(),
    panel.grid = element_blank()
  )

map_ped <- base_map +
  geom_sf(data = zat_injury, aes(fill= si_act_pea))+
  scale_fill_viridis_c(option = "plasma")+
  labs(fill = "Pedestrian Incidents")+
  theme_minimal()+  # note the order of adjusting theme
  theme(
    legend.position = "inside",
    legend.position.inside = c(0.9, 0.2),
    axis.text = element_blank(),
    panel.grid = element_blank()
  )

# traffic flow
walk_transit_geo <- readRDS("aggr_hclust_geo/walk_transit_geo.rds")

map_flow <- base_map +
  geom_sf(data = walk_transit_geo, aes(fill = flow))+
  scale_fill_viridis_c(option = "plasma") +
  labs(fill = "Traffic Flow")+
  theme_minimal()+  # note the order of adjusting theme
  theme(
    legend.position = "inside",
    legend.position.inside = c(0.9, 0.2),
    axis.text = element_blank(),
    panel.grid = element_blank()
  )


(map_clust | map_flow | map_victim)+ 
  plot_annotation('Neighborhood Profiles, Traffic Flows and Accidents', 
    subtitle = 'ZAT level, Bogotá',
    theme=theme(plot.title=element_text(size=18, face = "bold", hjust=0.5),
      plot.subtitle = element_text(size = 14, face = "bold", hjust = 0.5))) 


### Ran

# Categorizing the 'victim' variable into 5 equal parts
zat_injury2 <- zat_injury %>%
  mutate(victim_factor = cut(victim,
    breaks = quantile(victim, probs = seq(0, 1, by = 0.25), na.rm = TRUE),
    include.lowest = TRUE,
    labels = FALSE))

zat_injury2 <- zat_injury %>%
  mutate(victim_factor = cut(victim,
    breaks = c(0, 400, 800, 1200, 1600, 2000),
    include.lowest = TRUE,
    labels = c("0-400", "401-800", "801-1200", "1201-1600", "1601-2000")))

# Convert numeric labels to factor to ensure discrete scale in ggplot
zat_injury2$victim_factor <- factor(zat_injury2$victim_factor)

# Now, plotting with ggplot2 using the categorized 'victim' variable
ggplot() +
  geom_sf(data = zat_shapefile %>% st_zm()) +
  geom_sf(data = zat_injury2, aes(fill = factor(victim_factor))) +
  scale_fill_viridis_d(option = "plasma") +
  theme_minimal()
