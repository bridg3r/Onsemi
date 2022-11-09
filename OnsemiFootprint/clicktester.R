library(shiny)
library(plotly)

shinyUI <- fluidPage(
  mainPanel(
    plotlyOutput("trendPlot"),
    tags$head(tags$script(src="./www/clickhandler.js"))
  ) )

library(shiny)
library(plotly)

x = c(1, 2, 3)
y = c(4, 2, 4)
links = c("https://plot.ly/r/", 
          "https://plot.ly/r/shiny-tutorial", 
          "https://plot.ly/r/click-events")

df = data.frame(x, y, links)

shinyServer <- function(input, output) {
  
  output$trendPlot <- renderPlotly({
    # Create a ggplot
    plot_ly(df, x=x,y=y,links=links)
    
    # Alternatively, use Plotly's native syntax. More here: https://plot.ly/r
    # plot_ly(df, x=x,y=y,links=links)
  })
}

shinyApp(shinyUI, shinyServer)