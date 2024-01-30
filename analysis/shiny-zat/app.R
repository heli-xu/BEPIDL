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
library(ggplot2)
library(sf)


load("R/zat_indicator_list.rda")

ui <- fluidPage(

tags$head(includeCSS("CSS/Header.css")),
tags$head(includeCSS("CSS/styles.css")),
  
##Header
  includeHTML("HTML/Header.html"),
  
      # Application title
      titlePanel("Distribution of ZAT-level Indicators"),

      # Sidebar with a slider input for number of bins
      sidebarLayout(
          sidebarPanel(
              selectInput("indicator",
                          "ZAT-level indicator:",
                          choices = zat_indicator_list,
                          selected = "BUSTOPDENS")
          ),

          # Show a plot of the generated distribution
          mainPanel(
             plotOutput("distPlot")
          )
      )
)

server <- function(input, output, session) {
  load("R/georef_zat_xtr.rda")
  
  output$distPlot <- renderPlot({
    df_to_plot<- georef_zat_xtr %>% 
                 as.data.frame() %>% 
                 select(input$indicator)
    
    
      ggplot(df_to_plot, aes_string(input$indicator[1])) +
      geom_histogram(fill = "skyblue", color = "blue")+
      theme_minimal() +
      theme(
        panel.grid.minor = element_blank(),
        plot.title = element_text(size = 15, face = "bold")
      )
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
