## cluster_data is a ClustGeo object from get_clus
cluster_plot <- function(cluster_data) {
  cluster_data %>% 
    ggplot() +
    geom_col(aes(x = factor(indicator), y = scaled), fill = pal[cluster_data$clus]) +
    coord_flip() + 
    geom_hline(yintercept = 0, linetype = "dotted")+ #still set as y (although it's after flipping)
    theme_minimal() +
    labs(y = "Relative Less          Relative More",
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
        "mean_brt_length_log"="Bus route length" 
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
    facet_wrap(~clus, ncol = 1)
}
