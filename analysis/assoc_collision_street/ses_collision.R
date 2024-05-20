library(tidyverse)
library(sf)
library(MASS)
library(broom)

# 0.import data ----------------------------
#collision
calle_df <- readRDS("../../clean_data/calles/calle_df.rds")
calle_df %>% filter(is.na(si_act_pea))

#ses
ses_calle100m <- readRDS("../../clean_data/SES/ses_calle100m.rds")
ses_calle500m <- readRDS("../../clean_data/SES/ses_calle500m.rds")

# 1.SES distribution ----------------------------
ses_100 <- ses_calle100m %>% 
  dplyr::select(CodigoCL, ses_cat) %>% 
  count(ses_cat) %>% 
  ungroup() %>% 
  pivot_wider(names_from = ses_cat, values_from = n) %>% 
  mutate(total = rowSums(.)) %>% 
  pivot_longer(cols = -total_hs, names_to = "ses_cat", values_to = "count") %>% 
  mutate(percent = (count/total_hs)*100)

distr_stat <- function(data, id, group){
  data %>% 
    dplyr::select({{id}},{{group}}) %>% 
    count({{group}}) %>% 
    ungroup() %>% 
    mutate(total = nrow(data), #as long as no NA rows
      percent = (n/total)*100)
    # pivot_wider(names_from = {{group}}, values_from = n) %>% 
    # mutate(total = rowSums(.)) %>% 
    # pivot_longer(cols = -total, names_to = "group", values_to = "n") %>% 
    # mutate(percent = (n/total)*100)
}

# check it works
ses_100 <- distr_stat(ses_calle100m, CodigoCL, ses_cat)

ses_500 <- distr_stat(ses_calle500m, CodigoCL, ses_cat)
  
# 2. SES~ped collision -------------------------------
## 100m buffer------------------------------------
calle_ped100 <- calle_df %>% 
  dplyr::select(CodigoCL, ped_inc = si_act_pea) %>% 
  left_join(
    ses_calle100m %>%
      dplyr::select(CodigoCL, ses_cat),
    by = "CodigoCL"
      ) %>% 
 # filter(is.na(ses_cat)) 150 NAs
  drop_na(ses_cat) %>% 
  #use ses6 as reference
  mutate(ses_cat_r = factor(ses_cat, levels = rev(levels(ses_cat)))) 

levels(calle_ped100$ses_cat_r)

fit100 <- glm.nb(ped_inc ~ ses_cat_r, data = calle_ped100)
summary(fit100)

fit100_df <- tidy(fit100, conf.int = TRUE, exponentiate = TRUE) %>% 
  mutate(buffer_m = 100, 
    ses_cat = case_match(
      term,
      "(Intercept)" ~ "6",
      "ses_cat_r5" ~ "5",
      "ses_cat_r4" ~ "4",
      "ses_cat_r3" ~ "3",
      "ses_cat_r2" ~ "2",
      "ses_cat_r1" ~ "1"
    )) %>% 
  left_join(ses_100, by = "ses_cat") 
  

## 500m buffer--------------------------------------------
calle_ped500 <- calle_df %>% 
  dplyr::select(CodigoCL, ped_inc = si_act_pea) %>%  #select() masked by MASS
  left_join(
    ses_calle500m %>%
      dplyr::select(CodigoCL, ses_cat),
    by = "CodigoCL"
  ) %>% 
  #filter(is.na(ses_cat))
  drop_na(ses_cat) %>% 
  #use ses6 as reference
  mutate(ses_cat_r = factor(ses_cat, levels = rev(levels(ses_cat))))

fit500 <- glm.nb(ped_inc ~ ses_cat_r, data = calle_ped500)
summary(fit500)

fit500_df <- tidy(fit500, conf.int = TRUE, exponentiate = TRUE) %>% 
  mutate(buffer_m = 500,
    ses_cat = case_match(
      term,
      "(Intercept)" ~ "6",
      "ses_cat_r5" ~ "5",
      "ses_cat_r4" ~ "4",
      "ses_cat_r3" ~ "3",
      "ses_cat_r2" ~ "2",
      "ses_cat_r1" ~ "1"
    )) %>% 
  left_join(ses_500, by = "ses_cat")

## summarise RR-----------------
ses_collision_RR <- bind_rows(fit100_df, fit500_df) %>% 
  mutate(
    RR_95CI = paste0(round(estimate,2)," (", round(conf.low,2), ",", round(conf.high, 2), ")"),
    predictor = paste0("ses_", ses_cat)
  ) 

saveRDS(ses_collision_RR, file = "ses_collision_RR.rds")

ses_collision_csv <- ses_collision_RR %>% 
  dplyr::select(
    predictor, n, total, 
    percent_street = percent, 
    RR_95CI, p.value, buffer_m)


write_csv(total_df, file = "ses_collision_street.csv")

## Visualize ----------------------
plot_RR(ses_collision_RR, predictor)+
  facet_grid(vars(buffer_m), switch = "y")+
  theme(
    strip.text.y.left = element_text(face = "bold", angle = 90),
    strip.background.y = element_rect(fill = "white"),
    strip.placement = "outside",
  )+
  labs(
    title = "Pedestrian Collision and Socioeconomic Status (SES)",
    subtitle = "Street level, Bogotá, Colombia",
    x = "RR (95%CI)",
    y = "SES Level",
    caption = "All comparisons are relative to the 'ses_6' (highest) level."
  )
