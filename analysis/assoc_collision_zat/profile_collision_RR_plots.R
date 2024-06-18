#plots for profiles and collision

source("../../functions/plot_RR.R")
#modified from quarto post zat_hclust_geo.qmd
RR_plots <- function(data1, data2, data3){
  
  unadjust <- plot_RR(data1, predictor)+
    facet_grid(vars(outcome), switch = "y")+
    labs(
      #title = "Pedestrian Collision and Neighborhood Profiles",
      title = "Unadjusted, ZAT level",
      x = "RR (95%CI)",
      y = "ZAT Profile"
      #caption = "All comparisons are relative to the profile 1."
    )+
    theme(
      plot.title = element_text(size = 8, face = "plain"),
      plot.title.position = "plot")
  
  adjusted_ses <- data2 %>%
    filter(!predictor == "(Covariates)") %>%
    plot_RR(., predictor) +
    facet_grid(vars(outcome), switch = "y") +
    labs(
      title = "Adjusted for ZAT-level SES",
      x = "RR (95%CI)",
      y = "ZAT Profile"
    ) +
    theme(plot.title = element_text(size = 8, face = "plain"),
      plot.title.position = "plot")
  
  adjusted_all <- data3 %>%
    filter(!predictor == "(Covariates)") %>%
    plot_RR(., predictor) +
    facet_grid(vars(outcome), switch = "y") +
    labs(
      title = "Adjusted for ZAT-level walking/public transit \ntrips, SES, road types, and population density",
      x = "RR (95%CI)",
      y = "ZAT Profile",
      caption = "All comparisons are relative to the profile 4."
    ) +
    theme(plot.title = element_text(size = 8, face = "plain"),
      plot.title.position = "plot")
  
  (unadjust | adjusted_ses | adjusted_all)+
    plot_annotation('Pedestrian Collision and Neighborhood (ZAT) Profiles in Bogotá',
      theme=theme(plot.title=element_text(size=14, face = "bold", hjust=0.5)))
}

# 1. predict24+road info - derived profiles--------
profile_RR <- readRDS("predict24_profile/profile_rd_RR.rds")
profile_ses_RR <- readRDS("predict24_profile/profile_rd_ses_RR.rds")
profile_covar_RR <- readRDS("predict24_profile/profile_rd_covar2_RR.rds")

RR_plots(profile_RR, profile_ses_RR, profile_covar_RR)

# 2. calle+ZAT GIS-drived profiles ----------------
# a = 0.35, ref = profile4
profile2_RR <- readRDS("profile_wo_rd_type/profile_a35_RR_ref4.rds")
profile2_ses_RR <- readRDS("profile_wo_rd_type/profile_a35_ses_RR_ref4.rds")
profile2_covar_RR <- readRDS("profile_wo_rd_type/profile_a35_cov_rd_RR_ref4.rds")

RR_plots(profile2_RR, profile2_ses_RR, profile2_covar_RR)
