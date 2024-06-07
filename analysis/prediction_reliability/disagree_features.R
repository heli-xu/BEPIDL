library(tidyverse)
library(leaflet)
library(ggplot2)

# 0. import data -------
calle_geo <- readRDS("../../clean_data/calles/calle_shapefile.rds")

pr_variables <- c(
  "pr_sign_traffic_yn",
  "pr_sign_traffic_yn",
  "pr_traffic_light_yn",
  "pr_pedxwalk_yn",
  "pr_sign_stop_yn",
  "pr_sign_stop_yn",
  "pr_sign_yield_yn",
  "pr_sign_school_zone_yn",
  "pr_sidewalk_yn",
  "pr_lane_bike_yn",
  "pr_lane_bus_yn",
  "pr_median_yn",
  "pr_speed_bump_yn",
  "pr_trees_yn",
  "pr_bus_stop_yn",
  "pr_parked_vehicles_yn",
  "pr_lane_parking_yn",
  "pr_brt_station_yn"
)

# Variables starting with "gis_"
gis_variables <- c(
  "gis_road_signs_inv_yn",
  "gis_total_st_signs_yn",
  "gis_traffic_lights_yn",
  "gis_pedxwalk_signs_yn",
  "gis_stop_signs_yn",
  "gis_stop_signs2_yn",
  "gis_yield_signs_yn",
  "gis_school_zone_signs_yn",
  "gis_sw",
  "gis_bike_lane",
  "gis_any_bus",
  "gis_median",
  "gis_speed_bump_signs_yn",#noted change
  "gis_trees_yn",
  "gis_bus_stops_yn",
  "gis_parking_signs_yn",
  "gis_parking_signs_yn",
  "gis_brt_yn"
)

# 1. Diff (pr vs gis) -------------------
new_col <- paste0("pYgN_", str_sub(pr_variables, 4, -4))
pr_gis_calle <- readRDS("predict_gis_calle.rds")

prYgisN_expr <- paste(pr_variables, "-", gis_variables)

check_yn <- pr_gis_calle %>%
  dplyr::select(codigocl, all_of(pr_variables), all_of(gis_variables)) %>% 
  mutate(across(-codigocl, ~as.numeric(.x)))

check2 <- map2_dfc(new_col, prYgisN_expr, function(new_col, prYgisN_expr){
  check_yn %>% 
    transmute(
      {{new_col}} := eval(str2lang({{prYgisN_expr}}))
    )
}) %>% 
  bind_cols(check_yn) %>% 
  dplyr::select(codigocl, starts_with("pYgN_"))

colnames(check2)

# 2. street w prYES, gisNO --------------  
prYgisN <- check2 %>% 
  pivot_longer(cols = -codigocl, names_to = "variable", values_to = "diff") %>% 
  filter(diff > 0) %>% 
  group_by(codigocl) %>% 
  summarise(
    diff_all = sum(diff)
  )
#62k rows

saveRDS(prYgisN, file = "predictY_gisN.rds")

#csv deleted
#write_csv(predictY_gisN, file = "st_predictY_gisN.csv")

prYgisN_top250 <- prYgisN %>%
  arrange(desc(diff_all)) %>% 
  head(500) %>% 
  #slice_max(diff_all, n = 250) %>%  
  #12-15 feature diff
  left_join(calle_geo %>% rename(codigocl = CodigoCL),
    by = "codigocl") %>% 
  st_as_sf()

prYgisN_top250 %>% 
  st_transform(crs = st_crs("+proj=longlat +datum=WGS84")) %>% 
  leaflet() %>% 
  addTiles() %>% 
  addPolygons(
    color = "purple",
    fillColor = "purple"
  )

# 3. street w gisYes, prNO-------------------
gisYprN <- check2 %>% 
  pivot_longer(cols = -codigocl, names_to = "variable", values_to = "diff") %>% 
  filter(diff < 0) %>% 
  group_by(codigocl) %>% 
  summarise(
    diff_all = sum(diff)
  )
#9k rows

saveRDS(gisYprN, file = "gisY_predictN.rds")

#csv deleted
#write_csv(gisY_predictN, file = "st_gisY_predictN.csv")

gisYprN_top250 <- gisYprN %>% 
  arrange(diff_all) %>% 
  head(500) %>% 
  #slice_min(diff_all, n = 250) %>% 
  # 3-7 feature diff
  left_join(calle_geo %>% rename(codigocl = CodigoCL),
    by = "codigocl") %>% 
  st_as_sf()

gisYprN_top250 %>% 
  st_transform(crs = st_crs("+proj=longlat +datum=WGS84")) %>% 
  leaflet() %>% 
  addTiles() %>% 
  addPolygons(
    color = "blue",
    fillColor = "blue"
  )

# 4. diff by category-------
check_cat <- check2 %>% 
  rename_with(~str_sub(.x, start = 6), .cols = -codigocl) %>% 
  pivot_longer(cols = -codigocl, names_to = "feature", values_to = "diff") %>% 
  group_by(feature) %>% 
  summarise(match = sum(diff ==0),
            prYgisN = sum(diff == 1),
            gisYprN = sum(diff == -1)) %>% 
  mutate(
    feature = case_match(
      feature,
      "sign_stop...5"~"sign_stop1",
      "sign_stop...6" ~"sign_stop2",
      "sign_traffic...1"~"sign_traffic1",
      "sign_traffic...2"~"sign_traffic2",
      .default = feature
    )
  ) %>% 
  mutate(
    total_street = 63391,
    across(match:gisYprN, ~(.x/total_street)*100, .names = "pct_{.col}")
  )

saveRDS(check_cat, file = "check_by_cat.rds")

#csv deleted
#write_csv(check_by_cat, file = "check_by_category.csv")

df_plot <- check_cat %>% 
  select(feature, starts_with("pct_")) %>% 
  pivot_longer(cols = -feature, names_to = "compare", values_to = "pct") %>% 
  mutate(compare = factor(compare, levels = c("pct_match", "pct_prYgisN", "pct_gisYprN")))

## visualize ----------
pal = c("#146b3a","#1c2859","#f89c31")
ggplot(df_plot)+
  geom_col(aes(x = pct, y = compare, fill = compare))+
  scale_fill_manual(values = pal)+
  facet_grid(rows = vars(feature), scales = "free", switch = "y")+
  theme_bw()+
  labs(
    title = "AI prediction 2024 vs GIS data",
    subtitle = "Agreement by feature across all streets (~63k)",
    x = "Percent of Streets",
    y = "Street Features",
    caption = "match = prediction and GIS match; \nprYgisN = prediction YES, GIS NO; \ngisYprN = GIS YES, prediction NO"
  )+
  theme(
    plot.title = element_text(size = 13, face = "bold", hjust = 0),
    text = element_text(size = 11),
    axis.title = element_text(size = 12),
    # plot.title.position = "plot",
    panel.spacing.y = unit(0, "points"),
    panel.border = element_blank(),
    #axis.text.y = element_blank(),
    axis.ticks.length.y = unit(0, "points"),
    strip.text.y.left = element_text(face = "bold", angle = 0),
    strip.background.y = element_blank(),
    strip.placement = "outside",
    axis.line = element_line()
  )
      