#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(dplyr)
library(sf)
library(ggplot2)
library(leaflet)
library(glue)


load("R/zat_indicator_table.rda")
  
ui <- fluidPage(

tags$head(includeCSS("CSS/Header.css")),
tags$head(includeCSS("CSS/styles.css")),
  
##Header
  includeHTML("HTML/Header.html"),
  
      # Application title
  div( titlePanel("Built Environment Features in Bogotá: ZAT-level Indicators"), id = "navbar"),

      # Sidebar 
      sidebarLayout(
          sidebarPanel(
            width = 3,
            selectInput("indicator",
                        "ZAT-level indicator:",
                        choices = zat_indicator_table$var,
                        selected = "BUSTOPDENS"),
              
            textOutput("description_title"),
            textOutput("description")
          ),
          

          # Show a plot of the generated distribution
          mainPanel(
            width = 9,
            column(6, plotOutput("distPlot")),
            column(6, leafletOutput("map"))
          )
      ),
  
  ## Footer
  includeHTML("HTML/Footer.html")
)

server <- function(input, output, session) {
  load("R/zat_std_geo.rda")
  
  output$description_title <- renderText({print("Indicator Description:")})
  
  output$description <- renderText({
    zat_indicator_table %>% 
      filter(var == input$indicator) %>% 
      pull(description)
  })
  
  output$distPlot <- renderPlot({
    print(input$indicator)
    
    
    # df_to_plot_old<- zat_std %>% 
    #   as.data.frame() |>
    #   select(input$indicator) 
    # df_to_plot = tibble(column_selected = vector_selected)
    
    vector_selected <- zat_std_geo |> pull(input$indicator)
    
    mean_value <- mean(vector_selected)
    sd_value <- sd(vector_selected)
    
    
    
    zat_std_geo|>
      ggplot(aes(.data[[input$indicator]])) +
        geom_histogram(fill = "skyblue", color = "blue") +
        geom_vline(xintercept = mean_value, color = "#002f6c", #drexel blue
          size = 2) +
        geom_vline( xintercept = mean_value + sd_value, color = "#f2ca00",
          linetype = "dotted",size = 2) +
        geom_vline(xintercept = mean_value - sd_value, color = "#f2ca00",
          linetype = "dotted",size = 2) +
        annotate("text", x = mean_value, y = -5, label = "Mean",
               size = 4, color = "#808080") +
        annotate("text", x = mean_value + sd_value, y =-5,label = "+1 SD",
                size = 4,  color = "#808080") +
        annotate("text", x = mean_value - sd_value, y = -5, label = "-1 SD",
                size = 4, color = "#808080")+
        theme_minimal() +
        labs(
          title = "Histogram of the Distribution",
            y = "Number of ZAT Units")+
        theme(
          panel.grid.minor = element_blank(),
          title = element_text(size = 15, face = "bold"),
          axis.text.x = element_text(size = 14),
          axis.text.y = element_text(size = 14, face = "bold"),
          axis.title.x = element_text(size = 15, face = "bold"),
          axis.title.y = element_text(size = 15, face = "bold")
          )
  })

  #leaflet map color palette
  
  output$map <- renderLeaflet({
    zat_df <- as.data.frame(zat_std_geo)

    domain_v <- zat_df %>%
      select(input$indicator) %>%
      pull()

    pal <- colorNumeric(
      palette = c("orange","navy"),
      domain = domain_v
    )

    zat_label <- glue("ZAT{zat_std_geo$ZAT} {input$indicator}: {round(domain_v,2)}/km2")

    map_title <- glue("{input$indicator} </br>in Bogotá")

    zat_std_geo %>%
      st_zm() %>%
      st_transform(crs = st_crs("+proj=longlat +datum=WGS84")) %>%
      leaflet() %>%
      addProviderTiles(providers$CartoDB.Voyager)  %>%
      addPolygons(color = "white",
        weight = 0.5,
        smoothFactor = 0.5,
        opacity = 1,
        fillColor = ~pal(domain_v),
        fillOpacity = 0.8,
        highlightOptions = highlightOptions(
          weight = 5,
          color = "#666",
          fillOpacity = 0.8,
          bringToFront = TRUE),
        label = zat_label,
        labelOptions = labelOptions(
          style = list(
            "font-family" = "Fira Sans, sans-serif",
            "font-size" = "1.2em"
          ))
      ) %>%
      addLegend("bottomright",
        pal = pal,
        values = ~domain_v,
        title = map_title,
        opacity = 1)
  })
}


# Define UI for application that draws a histogram
# ui <- fluidPage(
# 
#     # Application title
#     titlePanel("Old Faithful Geyser Data"),
# 
#     # Sidebar with a slider input for number of bins 
#     sidebarLayout(
#         sidebarPanel(
#             sliderInput("bins",
#                         "Number of bins:",
#                         min = 1,
#                         max = 50,
#                         value = 30)
#         ),
# 
#         # Show a plot of the generated distribution
#         mainPanel(
#            plotOutput("distPlot")
#         )
#     )
# )
# 
# # Define server logic required to draw a histogram
# server <- function(input, output) {
# 
#     output$distPlot <- renderPlot({
#         # generate bins based on input$bins from ui.R
#         x    <- faithful[, 2]
#         bins <- seq(min(x), max(x), length.out = input$bins + 1)
# 
#         # draw the histogram with the specified number of bins
#         hist(x, breaks = bins, col = 'darkgray', border = 'white',
#              xlab = 'Waiting time to next eruption (in mins)',
#              main = 'Histogram of waiting times')
#     })
# }

# Run the application 
shinyApp(ui = ui, server = server)
