library(tidyverse)
library(sf)
library(MASS)
library(broom)

# 0.import data ----------------------------
#collision and feature
calle_rename_df <- readRDS("../../clean_data/calles/calle_rename_df.rds")
#covariates
covar_500 <- readRDS("../../clean_data/SES/covar_calle500m.rds")
covar_100 <- readRDS("../../clean_data/SES/covar_calle100m.rds")

# 1. collision ~ feature ---------------------------

## 1.1 feature descrp stat ------------------------------
feature_stat <- calle_rename_df %>% 
  pivot_longer(cols = -codigocl, names_to = "feature", values_to = "value") %>% 
  #count(codigocl)
  group_by(feature) %>% 
  summarise(
    mean = mean(value),
    sd = sd(value),
    min = min(value),
    max = max(value),
    median = median(value),
    IQR = IQR(value, na.rm = TRUE)
  )

write_csv(feature_stat, file = "st_feature_descrpt_stat.csv")

## 1.2 Prepare features --------------
### features to tertile
features <-  c(
  "trees",
  "grade",
  "area_median",
  "area_sidewalk",
  "road_width",
  "road_marks",
  "warning_signs",
  "road_signs",
  "traffic_lights",
  #"st_dir",  only 1,2 directions
  "num_lanes_total",
  "pedxwalk_signs",
  "school_zone_signs",
  "stop_signs_v",
  "stop_signs",
  "yield_signs",
  "total_st_signs",
  "bus_routes",
  "brt_routes",
  "bike_length",
  "traffic_fines_tot"
)

#features to iterate
all_features <-  c(
  "trees",
  "grade",
  "area_median",
  "area_sidewalk",
  "road_width",
  "road_marks",
  "warning_signs",
  "road_signs",
  "traffic_lights",
  "st_dir",
  "num_lanes_total",
  "pedxwalk_signs",
  "school_zone_signs",
  "stop_signs_v",
  "stop_signs",
  "yield_signs",
  "total_st_signs",
  "bus_routes",
  "brt_routes",
  "bike_length",
  "traffic_fines_tot"
)

### Zeroes, nonzero Tertiles, factored--------------
#interesting how NAs are useful here
calle_tertile <- calle_rename_df %>% 
  dplyr::select(codigocl, st_dir, all_of(features), ped_collision) %>% 
  mutate(
    across(all_of(features), ~na_if(., 0)), #turn to NA so that it doesn't get computed
    across(all_of(features), ~ntile(., 3), .names = "{.col}"),
    across(all_of(features), ~replace_na(., 0)), #turn it back 0
    across(-c(codigocl, ped_collision), ~factor(.))
  )

### zeros in feature count --------
count_zero <- calle_tertile %>% 
  summarise(across(everything(), ~ sum(. == 0))) %>% 
  pivot_longer(cols = -codigocl, names_to = "feature", values_to = "zero_count") %>% 
  mutate(total = 100819,
         percent = (zero_count/total)*100) %>% 
  select(-codigocl)

calle_rename_df %>% filter(grade == 0)

## 1.3 modeling -----------------
### try one
fit_feature <- glm.nb(ped_collision ~ area_median, data = calle_tertile)
summary(fit_feature)

### function for iterate -------
fit_features <- function(predictor){
  formula <- as.formula(paste("ped_collision ~", predictor))
  
  model <- glm.nb(formula,  data = calle_tertile)
  return(model)
}

test <- fit_features("area_median")
summary(test)

### iterate ---------
fit_allfeatures <- map(all_features, fit_features)

## 1.4 summarise RR ------------

feature_df <- map_df(fit_allfeatures,
  \(x) tidy(x, conf.int = TRUE, exponentiate = TRUE))

feature_RR <- feature_df %>%
  mutate(
    RR_95CI = paste0(round(estimate,2)," (", round(conf.low,2), ",", round(conf.high, 2), ")"),
    tertile = str_sub(term, -1),
    predictor = str_sub(term, end = -2),
    category = case_match(
        tertile,
        ")" ~ "Low (ref)",
        "2" ~ "Medium",
        "3" ~ "High"
      )
  ) 

saveRDS(feature_RR, file = "feature-collision_RR.rds")

feature_RR_csv <- feature_RR %>% 
   # note category for `st_dir` needs manual changing in csv, only 1, 2 directions
  dplyr::select(
    predictor = term,
    category,
    RR_95CI, 
    p.value
  )

write_csv(feature_RR_csv, file = "st_feature-collision.csv")

## 1.5 xtra: all features --------------
### tertile ------------
calle_tertile2 <- calle_rename_df %>% 
  dplyr::select(-c(sini_total:si_act_tot,si_act_cic:si_veh_bic)) %>% 
  mutate(
    across(-c(codigocl, ped_collision, st_dir), ~ntile(., 3), .names = "{.col}"),
    across(-c(codigocl, ped_collision), ~factor(.))
  )

xtra_features <- calle_tertile2 %>% 
  dplyr::select(-c(codigocl, ped_collision)) %>% 
  colnames()

### model --------------
xtraFeatures <- map(xtra_features, fit_features) #change data in fit_features()

feature_xtra <- map_df(xtraFeatures,
  \(x) tidy(x, conf.int = TRUE, exponentiate = TRUE))

saveRDS(feature_xtra, file = "all_features-collision.rds")

sig <- feature_xtra %>% 
  filter(p.value < 0.05)

## 1.6 Visualize ------------
feature_RR <- readRDS("feature-collision_RR.rds") %>% 
  mutate(category = case_match(
    predictor,
    "st_dir" ~ "double",
    .default = category
  ))

#below has become a function plot_facet_RR()
feature_RR %>% 
  filter(!term == "(Intercept)") %>% 
  #mutate(category = factor(predictor)) %>% 
  ggplot(aes(x = estimate, y = category))+
  geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf, fill = predictor), alpha = 1) +
  scale_fill_manual(values = rep(c("#ffffff00", "#f0f0f090"), 20), guide = "none")+ 
  geom_errorbar(aes(xmax = conf.high, xmin = conf.low), linewidth = 0.5)+
  geom_point(aes(x = estimate), size = 2)+
  geom_vline(aes(xintercept = 1), linetype = 2) +
  facet_grid(rows = vars(predictor), scales = "free", switch = "y")+
  #scale_x_continuous(breaks = c(1, 4, 8, 12), labels = c("1","4", "8", "12"))+
  scale_x_continuous(breaks = sort(round(c(seq(min(feature_RR$conf.low), max(feature_RR$conf.high), length.out = 4), 1), 0)))+
  theme_bw()+
 # scale_fill_manual(values = c("Medium" = "#ffffff00", "High" = "#f0f0f090"), guide = "none") +
  labs(
    title = "Pedestrian Collision and Street Features",
    x = "RR (95%CI)",
    y = "Street Features",
    caption = "Tertiles of street feature values are used in analysis.\n All comparisons are relative to the 'Low' category."
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

feature_RR %>% 
  filter(!term == "(Intercept)") %>% 
  plot_facet_RR()+
  labs(
    subtitle = "Unadjusted"
  )+
  theme(
    plot.title.position = "plot"
  )

# 2. collision~feature+covar -----------------

## 2.1 Prepare features---------------------
### Zeros, nonzero Tertiles, factored---------
calle_tertile <- calle_rename_df %>% 
  dplyr::select(codigocl, st_dir, all_of(features), ped_collision) %>% 
  mutate(
    across(all_of(features), ~na_if(., 0)), #turn to NA so that it doesn't get computed
    across(all_of(features), ~ntile(., 3), .names = "{.col}"),
    across(all_of(features), ~replace_na(., 0)), #turn it back 0
    across(-c(codigocl, ped_collision), ~factor(., levels = c("1", "0", "2", "3")))
  )

levels(calle_tertile$trees)

### *scaled--------------
collision_covar_500 <- covar_500 %>%
  mutate(
    across(c(pop_density, starts_with("pct")), ~ scale(.x)[,1])
  ) %>% 
  rename(codigocl = CodigoCL) %>% 
  left_join(calle_tertile, by = "codigocl") 


## 2.2 500m buffer-------------
fit_feature <- glm.nb(ped_collision ~ area_median + pct_apt + pct_home + pct_unoccu + pop_density + pct_male + pct_yr_0_9 + pct_yr_10_19 + pct_yr_30_39 + pct_yr_40_49 + pct_yr_50_59 + pct_yr_60_69 + pct_yr_70_79 + pct_yr_80_plus, data = collision_covar_500)

summary(fit_feature)

### interate----------------
fit_features_x <- function(predictor, data){
  formula <- as.formula(paste("ped_collision ~", predictor, "+ pct_apt + pct_home + pct_unoccu + pop_density + pct_male + pct_yr_0_9 + pct_yr_10_19 + pct_yr_30_39 + pct_yr_40_49 + pct_yr_50_59 + pct_yr_60_69 + pct_yr_70_79 + pct_yr_80_plus"))
  
  model <- glm.nb(formula,  data = data)
  return(model)
}

test <- fit_features_x("area_median", collision_covar_500)
summary(test)


fit_allfeatures <- map(all_features, \(x) fit_features_x(x, data = collision_covar_500))

feature_df <- map_df(fit_allfeatures,
  \(x) tidy(x, conf.int = TRUE, exponentiate = TRUE))
#took 20min or something

### summarise RR-----------------
feature_RR <- feature_df %>%
  mutate(
    RR_95CI = paste0(round(estimate,2)," (", round(conf.low,2), ",", round(conf.high, 2), ")"),
    tertile = str_sub(term, -1),
    predictor = str_sub(term, end = -2),
    category = case_match(
      tertile,
      ")" ~ "Low (ref)",
      "0" ~ "Zero",
      "2" ~ "Medium",
      "3" ~ "High",
      .default = "(Covariates)"
    ),
    category = case_match(
      term,
      "st_dir2" ~ "Double",
      .default = category
    )
  ) 

saveRDS(feature_RR, file = "st_feature_covar500_RR.rds")

### *visualize -------------------
feature_covar500 <- readRDS("st_feature_covar500_RR.rds")
source("../../functions/plot_facet_RR.R")

feature_covar500 %>% 
  filter(!term == "(Intercept)",
  !category == "(Covariates)") %>% 
  plot_facet_RR()+  
  labs(
    subtitle = "Adjusted for types of dwellings, population density, age groups and sex composition (500m)"
  )+
  theme(
    plot.title.position = "plot"
  )

## 2.3 100m buffer ---------------------
collision_covar_100 <- covar_100 %>%
  mutate(
    across(c(pop_density, starts_with("pct")), ~ scale(.x)[,1])
  ) %>% 
  rename(codigocl = CodigoCL) %>% 
  left_join(calle_tertile, by = "codigocl") 

### iterate --------------------
fit_allfeatures <- map(all_features, \(x) fit_features_x(x, data = collision_covar_100))

feature100_df <- map_df(fit_allfeatures,
  \(x) tidy(x, conf.int = TRUE, exponentiate = TRUE))
#took 20min or something

### summarise RR---------------
feature100_RR <- feature100_df %>%
  mutate(
    RR_95CI = paste0(round(estimate,2)," (", round(conf.low,2), ",", round(conf.high, 2), ")"),
    tertile = str_sub(term, -1),
    predictor = str_sub(term, end = -2),
    category = case_match(
      tertile,
      ")" ~ "Low (ref)",
      "0" ~ "Zero",
      "2" ~ "Medium",
      "3" ~ "High",
      .default = "(Covariates)"
    ),
    category = case_match(
      term,
      "st_dir2" ~ "Double",
      .default = category
    )
  ) 

saveRDS(feature100_RR, file = "st_feature_covar100_RR.rds")
# a <- feature100_RR %>% 
#   filter(!term == "(Intercept)",
#     !category == "(Covariates)") %>% 
#   filter(p.value < 0.05)
  

### visualize-------------
feature_covar100 <- readRDS("st_feature_covar100_RR.rds")

feature_covar100 %>% 
  filter(!term == "(Intercept)",
    !category == "(Covariates)") %>% 
plot_facet_RR()+
  labs(
    subtitle = "Adjusted for types of dwellings, population density, age groups and sex composition (100m)"
  )+
  theme(
    plot.title.position = "plot"
  )




















