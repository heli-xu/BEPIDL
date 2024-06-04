library(tidyverse)
library(sf)
library(MASS)
library(broom)

# 0.import data ----------------------------
#collision and feature
calle_df <- readRDS("../../clean_data/calles/calle_df.rds")

# road type
road_type_calle <- readRDS("../../clean_data/road_type/road_type_calle.rds") 

# 1. collision ~ road_type -----------
## 1.1 distribution------------------
source("../../functions/distr_stat.R")

rd_dsitr <- road_type_calle %>% 
  st_drop_geometry() %>% 
  distr_stat(., CodigoCL, road_type2)

## 1.2 join data, fit model---------
collision_rd_type <- road_type_calle %>% 
  left_join(calle_df, by = "CodigoCL") %>% 
  dplyr::select(CodigoCL, road_type2, ped = si_act_pea) %>% 
  mutate(road_type2 = factor(road_type2))

levels(collision_rd_type$road_type2)

fit_rd <- glm.nb(ped ~ road_type2, data = collision_rd_type)
summary(fit_rd)

## 1.3 summarise RR -------------------
rd_type_RR <- tidy(fit_rd, conf.int = TRUE, exponentiate = TRUE) %>% 
  mutate(
    RR_95CI = paste0(round(estimate,2)," (", round(conf.low,2), ",", round(conf.high, 2), ")"),
    road_type2 = case_match(
      term,
      "(Intercept)" ~ "Arterial",
      "road_type2Collector" ~ "Collector",
      "road_type2Local" ~ "Local",
      "road_type2Other" ~ "Other"
    )
  ) %>% 
  left_join(rd_dsitr, by = "road_type2") 

saveRDS(rd_type_RR, file = "rd_type-collision_RR.rds")

rd_type_RR_csv <- rd_type_RR %>% 
  dplyr::select(
    predictor = road_type2,
    n, total,
    percent_street = percent,
    RR_95CI, 
    p.value)

write_csv(rd_type_RR, file = "rd_type-collision.csv")

## 1.4 Visualize --------------


data <- rd_type_RR %>% 
  filter(!term == "(Intercept)") 

data %>% 
  ggplot(aes(x = estimate, y = road_type2))+
  geom_errorbar(aes(xmin = conf.low, xmax = conf.high), linewidth = 0.5)+
  geom_point(aes(x = estimate), size = 2)+
  geom_vline(aes(xintercept = 1), linetype = 2) +
  theme_bw()+
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
  )+  
  #below are outside of function
  labs(
    title = "Pedestrian Collision and Road Type",
    x = "RR (95%CI)",
    y = "Road Type",
    caption = "All comparisons are relative to the 'Arterial' road type."
  )+
  #if x =1 axis label is needed
  scale_x_continuous(breaks = sort(round(c(seq(min(data$conf.low), max(data$conf.high), length.out = 3), 1), 2)))

### function for basic plot --------------
plot_RR <- function(data, group){
  data2 <- data %>% 
    filter(!term == "(Intercept)") %>% 
    rename(predictor = {{group}})
  
  data2 %>% 
    ggplot(aes(x = estimate, y = predictor))+
    geom_errorbar(aes(xmin = conf.low, xmax = conf.high), linewidth = 0.5)+
    geom_point(aes(x = estimate), size = 2)+
    geom_vline(aes(xintercept = 1), linetype = 2) +
    theme_bw()+
    theme(
      plot.title = element_text(size = 13, face = "bold", hjust = 0),
      text = element_text(size = 11),
      axis.title = element_text(size = 12),
      # plot.title.position = "plot",
      panel.spacing.y = unit(0, "points"),
      panel.border = element_blank(),
      #axis.text.y = element_blank(),
      axis.ticks.length.y = unit(0, "points"),
      #strip.text.y.left = element_text(face = "bold", angle = 0),
      #strip.background.y = element_blank(),
      #strip.placement = "outside",
      axis.line = element_line()
    )
}

plot_RR(rd_type_RR, road_type2) 

# 2. collision~rd_type + covar---------------
## 2.1 join col, rd_type, covar data -----------
### 500m buffer-------
collision_rd_covar500 <- road_type_calle %>% 
  left_join(calle_df, by = "CodigoCL") %>% 
  dplyr::select(CodigoCL, road_type2, ped = si_act_pea) %>% 
  mutate(road_type2 = factor(road_type2, levels = c("Local", "Arterial", "Collector", "Other"))) %>% 
  left_join(covar_500, by = "CodigoCL") %>% 
  mutate(
    across(c(pop_density, starts_with("pct")), ~ scale(.x)[,1])
  )

### 100m buffer -----------------
collision_rd_covar100 <- road_type_calle %>% 
  left_join(calle_df, by = "CodigoCL") %>% 
  dplyr::select(CodigoCL, road_type2, ped = si_act_pea) %>% 
  mutate(road_type2 = factor(road_type2, levels = c("Local", "Arterial", "Collector", "Other"))) %>% 
  left_join(covar_100, by = "CodigoCL") %>% 
  mutate(
    across(c(pop_density, starts_with("pct")), ~ scale(.x)[,1])
  )

## 2.2 model w covar----------------

### 500m buffer---------
fit_rd_covar500 <- glm.nb(ped ~ road_type2 + pct_apt + pct_home + pct_unoccu + pop_density + pct_male + pct_yr_0_9 + pct_yr_10_19 + pct_yr_30_39 + pct_yr_40_49 + pct_yr_50_59 + pct_yr_60_69 + pct_yr_70_79 + pct_yr_80_plus, data = collision_rd_covar500)

summary(fit_rd_covar500)

### 100m buffer-----------
fit_rd_covar100 <- glm.nb(ped ~ road_type2 + pct_apt + pct_home + pct_unoccu + pop_density + pct_male + pct_yr_0_9 + pct_yr_10_19 + pct_yr_30_39 + pct_yr_40_49 + pct_yr_50_59 + pct_yr_60_69 + pct_yr_70_79 + pct_yr_80_plus, data = collision_rd_covar100)

summary(fit_rd_covar100)


## 2.3 summarise RR ---------------
### 500m buffer------------
rd_covar500_RR <- tidy(fit_rd_covar500, conf.int = TRUE, exponentiate = TRUE) %>% 
  mutate(
    RR_95CI = paste0(round(estimate,2)," (", round(conf.low,2), ",", round(conf.high, 2), ")"),
    road_type2 = case_match(
      term,
      "(Intercept)" ~ "Local",
      "road_type2Arterial" ~ "Arterial",
      "road_type2Collector" ~ "Collector",
      "road_type2Other" ~ "Other",
      .default = "(Covariates)"
    )
  ) %>% 
  left_join(rd_dsitr, by = "road_type2") 

### 100m buffer ---------------
rd_covar100_RR <- tidy(fit_rd_covar100, conf.int = TRUE, exponentiate = TRUE) %>% 
  mutate(
    RR_95CI = paste0(round(estimate,2)," (", round(conf.low,2), ",", round(conf.high, 2), ")"),
    road_type2 = case_match(
      term,
      "(Intercept)" ~ "Local",
      "road_type2Arterial" ~ "Arterial",
      "road_type2Collector" ~ "Collector",
      "road_type2Other" ~ "Other",
      .default = "(Covariates)"
    )
  ) %>% 
  left_join(rd_dsitr, by = "road_type2") 


saveRDS(rd_covar500_RR, file = "rd_type_col_covar500_RR.rds")
saveRDS(rd_covar100_RR, file = "rd_type_col_covar100_RR.rds")

## 2.4 Visualize -------------
source("../../functions/plot_RR.R")

rd_covar500_RR %>% 
  filter(!road_type2 == "(Covariates)") %>% 
  plot_RR(., road_type2)+
  labs(
    title = "Pedestrian Collision and Road Type",
    subtitle = "Adjusted for population, age, sex, types of dwellings (500m buffer range).",
    x = "RR (95%CI)",
    y = "Road Type",
    caption = "All comparisons are relative to the 'Local' road type."
  )+
  scale_x_continuous(breaks = sort(round(c(seq(min(rd_covar500_RR$conf.low), max(rd_covar500_RR$conf.high), length.out = 4), 1), 0)))

rd_covar100_RR %>% 
  filter(!road_type2 == "(Covariates)") %>% 
  plot_RR(., road_type2)+
  labs(
    title = "Pedestrian Collision and Road Type",
    subtitle = "Adjusted for population, age, sex, types of dwellings (100m buffer range).",
    x = "RR (95%CI)",
    y = "Road Type",
    caption = "All comparisons are relative to the 'Local' road type."
  )+
  scale_x_continuous(breaks = sort(round(c(seq(min(rd_covar500_RR$conf.low), max(rd_covar500_RR$conf.high), length.out = 4), 1), 0)))
