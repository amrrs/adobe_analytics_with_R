#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(plotly)
library(dplyr)

# Define UI for application that draws a histogram
ui <- fluidPage(
   
  # Application title
  titlePanel("Advanced Attribution Example"),
   
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
       checkboxGroupInput(
         "model_selection",
         "Select Attribution Models:",
         choices = c(
          "Last Touch"= 2, 
          "First Touch" = 3,
          "Linear" = 4,
          "Half Life" = 5,
          "U-Shaped" = 6,
          "Algorithmic" = 7
        ),
        selected = c(2,3,4)
      )
    ),
      
      # Show a plot of the generated distribution
      mainPanel(
         plotlyOutput("modelPlot"),
         tableOutput("modelTable")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$modelPlot = renderPlotly({
    selected_cols = as.numeric(input$model_selection)
    model_table = model_comparison2
    
    p = plot_ly(model_table, x = ~ model_table$Marketing.Channel, y = ~model_table$Last.Touch, type = 'bar', name = 'Last Touch')
    
    if(3 %in% selected_cols){
      p = p %>% add_trace(y = ~model_table$First.Touch, name = 'First Touch')
    }
    if(4 %in% selected_cols){
      p = p %>% add_trace(y = ~model_table$Linear, name = 'Linear')
    }
    if(5 %in% selected_cols){
      p = p %>% add_trace(y = ~model_table$Half.Life, name = 'Half Life')
    }
    if(6 %in% selected_cols){
      p = p %>% add_trace(y = ~model_table$U.Shaped, name = 'U-Shaped')
    }
    if(7 %in% selected_cols){
      p = p %>% add_trace(y = ~model_table$Shapley.Value, name = 'Algorithmic')
    }
    
    p = p %>%
      layout(yaxis = list(title = 'Orders'), barmode = 'group') %>%
      layout(xaxis = list(title = 'Marketing Channel'))
    p
  })
  
  output$modelTable = renderTable({
    model_comparison2
    #model_comparison2[,c(1,as.numeric(input$modelselection))]
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)

