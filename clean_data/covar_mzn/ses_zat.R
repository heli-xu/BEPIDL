library(sf)
library(tidyverse)
library(leaflet)
library(glue)
library(ggplot2)

sf_use_s2(FALSE)

ses <- st_read("../../data/SES/SHP_MGN2018_INTGRD_MANZ/MGN_ANM_MANZANA.shp")

zat_xy <- readRDS("../../clean_data/zat/zat_shapefile.rds") %>% 
  st_zm()

# 1. Link block and ZAT -----------------
ses_level <- ses %>% 
  select(COD_DANE_A, starts_with("TP19_EE_E"))

block_zat <- ses_level %>% 
  st_transform(crs = st_crs(zat_xy)) %>% 
  st_join(zat_xy, .predicate = st_intersects)

block_zat %>% 
  st_drop_geometry() %>% 
  filter(is.na(ZAT))

block_zat %>% 
  st_drop_geometry() %>% 
  drop_na(ZAT) %>% 
  count(ZAT)

# 2. Summarize, weighted mean for ZAT------------
ses_zat <- block_zat %>% 
  st_drop_geometry() %>% 
  drop_na(ZAT) %>% 
  group_by(ZAT) %>% 
  summarise(across(TP19_EE_E1:TP19_EE_E9, ~sum(.x), .names = "{.col}"), .groups = "drop") %>% 
  mutate(
    total_household = rowSums(pick(TP19_EE_E1:TP19_EE_E9)),  #faster than rowwise
    #ungroup() %>% 
    across(TP19_EE_E1:TP19_EE_E6, ~ (.x/total_household)*100, .names = "percent_{.col}")
  ) %>% 
  mutate(wt_mean = (percent_TP19_EE_E1*1 + percent_TP19_EE_E2*2 + percent_TP19_EE_E3*3 + percent_TP19_EE_E4*4 + percent_TP19_EE_E5*5 + percent_TP19_EE_E6*6)/100) %>%  #checked NAs (below) ****
  drop_na(wt_mean) %>% 
  mutate(
    ses_cat = case_when(
      wt_mean %in% c(1.5, 2.5, 3.5, 4.5, 5.5) ~ ceiling(wt_mean),
      .default = round(wt_mean, 0)
    ),
    ses_cat = factor(ses_cat)
  )

# ***
block_zat %>% 
  st_drop_geometry() %>% 
  count(ZAT) %>% 
  arrange(n)  #most of NA in single-block ZAT

block_zat %>% 
  st_drop_geometry() %>% 
  filter(ZAT == 404)  # 3 blocks with no record
# ***


levels(ses_zat$ses_cat) #make sure it's 6 levels

saveRDS(ses_zat, file = "ses_zat.rds")

# 3. Visualize --------------

ses_zat_geo <- ses_zat %>% 
  left_join(zat_xy, by = "ZAT") %>% 
  st_as_sf()

pal <- colorFactor(
  palette = c("orange","navy"),
  domain = ses_zat_geo$ses_cat
)

label <- glue("ZAT {ses_zat_geo$ZAT} Weighted average SES level: {ses_zat_geo$ses_cat}")


ses_zat_geo %>%
  st_transform(crs = st_crs("+proj=longlat +datum=WGS84")) %>%
  leaflet() %>%
  addProviderTiles(providers$CartoDB.Positron)  %>%
  addPolygons(color = "white", 
              weight = 0.5,
              #smoothFactor = 0.5,
              opacity = 1,
              fillColor = ~pal(ses_cat),
              fillOpacity = 0.9,
              highlightOptions = highlightOptions(
                weight = 3,
                color = "grey",
                fillOpacity = 0.8,
                bringToFront = TRUE),
              label = label,
              labelOptions = labelOptions(
                style = list(
                  "font-family" = "Fira Sans, sans-serif",
                  "font-size" = "1.2em"
                ))
  ) %>% 
  addLegend("bottomleft",
            pal = pal,
            values = ~ses_cat,
            title = "Average SES Level",
            opacity = 1)


# 3. Poverty Index ----------
ipm <- read_csv("../../data/ZAT/BE_vars.csv") %>% 
  dplyr::select(ZAT, MEANIPM)

saveRDS(ipm, file = "ipm_zat.rds")
