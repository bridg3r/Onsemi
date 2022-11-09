# install.packages(c( "gapminder", "ggforce", "gh", "globals", "openintro", "profvis", "RSQLite", "shiny", "shinycssloaders", "shinyFeedback",  "shinythemes", "testthat", "thematic", "tidyverse", "vroom", "waiter", "xml2", "zeallot" ))

# TO DO:
# Move everything to quarto in VS code
# add reactive events to filter to only selected location
# add in Onsemi logo title
# add in an annotion to highlight the location
# better looking markers on the map
# add dropdown for different groups
# add dropdown for differnt site functions
# add in clickalble links to sharepoint site
# Spice up the page more
# fix city names, use code from colab

library(shiny)
source("./map.R")

master <- ggplot(data = world) +
  geom_sf(color = "black", fill = rgb(144, 238, 144, maxColorValue = 255)) +
  labs(shape="Group", color = "Site Function") +
  theme(panel.background = element_rect(fill = rgb(137, 207, 240, maxColorValue = 255)),
        panel.grid.major = element_line(linetype = 'dashed', size = .3),
        plot.title = element_text(hjust = 0.5), axis.title.x=element_blank(), axis.title.y=element_blank(), 
        legend.position = "none") + 
  scale_fill_manual(values = c('ATO' = rgb(255, 172, 28, maxColorValue = 255), 'other'=rgb(238, 75, 43, maxColorValue = 255), 
                                'Wafer Fab Site' = rgb(34, 139, 34, maxColorValue = 255))) +
  scale_shape_manual(values = c('ASG' = 21, 'non-ASG'= 24)) + 
  geom_point(data = onsemi, aes(Longitude, Latitude, fill = site_type, shape=cgroup), color = outline, size = 5)


ui <- fluidPage(
  titlePanel(imageOutput("logo", height = '50px')),
  fluidRow(
    column(9,
           plotOutput("plot2", width = "900px", height = '450px')
    ), 
    column(2,
           selectInput("city2", label = "Locations", selected = 'All', choices = append(c('All'), sort(onsemi$city))),
           imageOutput("legend")
    )
  )
)


server <- function(input, output, session) {
  output$plot2 <- renderPlot(
    if (input$city2 != 'All') {
      junior + geom_label(aes(Longitude, Latitude+7, label = city), data = onsemi %>% filter(city==input$city2)) +
        scale_shape_manual(values = c('ASG' = 21, 'non-ASG'= 24)) +
        geom_point(data = onsemi %>% filter(city==input$city2), aes(Longitude, Latitude, fill = site_type, shape=cgroup), color=outline, size = 5)
    } else {
      master
    }
    
  )
  
  output$legend <- renderImage({
    list(
      src = file.path("./legend.png"),
      contentType = "image/png",
      width = 130,
      height = 200
    )
  }, deleteFile = FALSE)
  
  output$logo <- renderImage({
    list(
      src = file.path("./worldwidelogo.png"),
      contentType = "image/png",
      width = 500,
      height = 50
    )
  }, deleteFile = FALSE)
  
  output$title <- renderText("Worldwide Footprint")
  
}


shinyApp(ui, server)
