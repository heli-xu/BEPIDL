#' Create Clustered Bar Plot
#'
#' Generates a bar plot for cluster analysis, displaying scaled values for various indicators within each cluster. The plot is flipped for better visualization of indicators, with bars colored by cluster. This function is tailored for visualizing standardized indicator values across different clusters.
#'
#' @param cluster_data a ClustGeo object from `get_cluster()`, containing the variables: `indicator`, `scaled`, and `clus`. 
#'   \describe{
#'     \item{indicator}{a factor indicating the type of metric}
#'     \item{scaled}{the scaled value of that metric (averaged value by cluster)}
#'     \item{clus}{the cluster assignment}
#'   }
#'
#' @return A ggplot object representing the clustered bar plot. This plot includes a horizontal line at y=0 (indicating no change or baseline), with bars for each indicator scaled around this line. The plot is facetted by cluster, with each cluster's indicators displayed in its own panel.
#'
#' @examples
#' # Assuming you have a dataframe `df` with columns `indicator`, `scaled`, and `clus`
#' cluster_plot(df)
#'
#' @import ggplot2
#' @importFrom dplyr %>%
#' @importFrom ggplot2 ggplot geom_col coord_flip geom_hline theme_minimal labs scale_x_discrete theme facet_wrap
#' @export
#'

cluster_plot <- function(cluster_data) {
  
  cluster_data %>% 
    ggplot() +
    geom_col(aes(x = factor(indicator), y = scaled, fill = factor(clus))) +  ##modified `fill`
    scale_fill_manual(values = pal)+
    coord_flip() + 
    geom_hline(yintercept = 0, linetype = "dotted")+ #still set as y (although it's after flipping)
    theme_minimal() +
    labs(y = "Less     More                   Less      More",
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
        "mean_pedxwalk_signs"="Ped crosswalk sign (st)",
        "mean_pct_Collector" = "Collector rd (st)",
        "mean_pct_Arterial" = "Arterial rd (st)",
        "mean_pct_Local" = "Local rd (st)",
        "mean_pct_other" = "Other rd (st)",
        "mean_pcta_Collector" = "Collector rd area (st)",
        "mean_pcta_Arterial" = "Arterial rd area (st)",
        "mean_pcta_Local" = "Local rd area (st)",
        "mean_pcta_other" = "Other rd area (st)",
        #for prediction data
        "mean_sign_traffic" = "Traffic Sign",
        "mean_traffic_light" = "Traffic Light",
        "mean_sign_crossing" = "Crossing Sign",
        "mean_pedestrian_light" = "Pedestrian Light",
        "mean_sign_stop" = "Stop Sign",
        "mean_sign_yield" = "Yield Sign",
        "mean_sign_school_zone" = "School Zone Sign",
        "mean_sidewalk" = "Sidewalk",
        "mean_crosswalk" = "Crosswalk",
        "mean_lane_marking" = "Lane Marking",
        "mean_lane_bike" = "Bike Lane",
        "mean_lane_bus" = "Bus Lane",
        "mean_roundabout" = "Roundabout",
        "mean_curb" = "Curb",
        "mean_bollards" = "Bollards",
        "mean_median" = "Median",
        "mean_median_barrier" = "Median Barrier",
        "mean_speed_bump" = "Speed Bump",
        "mean_trees" = "Trees",
        "mean_bus_stop" = "Bus Stop",
        "mean_street_lights" = "Street Lights",
        "mean_kiosks" = "Kiosks",
        "mean_parked_vehicles" = "Parked Vehicles",
        "mean_sidewalk_obstruction" = "Sidewalk Obstruction",
        "mean_lane_parking" = "Parking Lane",
        "mean_brt_station" = "BRT Station",
        "mean_potholes" = "Potholes"        
      ))+
    theme(panel.grid = element_blank(),
      legend.position = "none", ## added
      axis.text.x = element_blank(),
      axis.text.y = element_text(size = 9),
      axis.line.x = element_line(arrow = grid::arrow(length = unit(0.3, "cm"), 
        ends = "both")),
      plot.margin=grid::unit(c(0,0,0,0), "mm"),
      strip.text = element_text(face = "bold", size = 10),
      strip.background = element_rect(fill = "grey", color = "white"),
      panel.spacing.x = unit(1, "lines")) +
    facet_wrap(~clus, ncol = 2) # changed ncol
}
