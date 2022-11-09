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

junior <- ggplot(data = world) +
  geom_sf(color = "black", fill = rgb(144, 238, 144, maxColorValue = 255)) +
  theme(panel.background = element_rect(fill = rgb(137, 207, 240, maxColorValue = 255)),
        panel.grid.major = element_line(linetype = 'dashed', size = .3),
        plot.title = element_text(hjust = 0.5), axis.title.x=element_blank(), axis.title.y=element_blank(), 
        legend.position = "none") + 
  scale_color_manual(values = c('ATO' = rgb(255, 172, 28, maxColorValue = 255), 'other'=rgb(238, 75, 43, maxColorValue = 255), 
                                'Wafer Fab Site' = rgb(34, 139, 34, maxColorValue = 255))) +
  scale_shape_manual(values = c('ASG' = 16, 'non-ASG'= 17))

ui <- fluidPage(
  titlePanel(imageOutput("logo", height = '50px')),
  tabsetPanel(
    tabPanel('Interactive',
             fluidRow(
               column(9,
                      plotlyOutput("plot", width = "900px", height = '450px')
               ), 
               column(2,
                      selectInput("group", label = "Groups", selected = 'All', choices = append(c('All'), sort(onsemi$cgroup))),
                      selectInput("type", label = "Site Function", selected = 'All', choices = append(c('All'), sort(onsemi$site_type))),
                      selectInput("city", label = "Find One Location", selected = 'All', choices = append(c('All'), sort(onsemi$city))),
                      imageOutput("legend")
               )
             )
    ),
    tabPanel('Static',
      
    )
  )
)

server <- function(input, output, session) {
  output$plot <- renderPlotly( 
    if (input$city != 'All') {
      ggplotly(junior + geom_point(data = onsemi %>% filter(city == input$city), 
                                   aes(Longitude, Latitude, color = site_type, shape=cgroup, 
                                       text = paste0(city, ', ', country, '\n', site_type, '\n', cgroup)), 
                                   fill = 'black', size = 3), tooltip = 'text')
    } else if (input$type != 'All' & input$group == 'All' & input$city == 'All') {
      ggplotly(junior + geom_point(data = onsemi %>% filter(site_type == input$type), 
                                   aes(Longitude, Latitude, color = site_type, shape=cgroup, 
                                       text = paste0(city, ', ', country, '\n', site_type, '\n', cgroup)), 
                                   fill = 'black', size = 3), tooltip = 'text')
    } else if (input$group != 'All' & input$type == 'All' & input$city == 'All') {
      ggplotly(junior + geom_point(data = onsemi %>% filter(cgroup == input$group), 
                                   aes(Longitude, Latitude, color = site_type, shape=cgroup, 
                                       text = paste0(city, ', ', country, '\n', site_type, '\n', cgroup)), 
                                   fill = 'black', size = 3), tooltip = 'text')
    } else if (input$group != 'All' & input$type != 'All' & input$city == 'All') {
      tryCatch({
        ggplotly(junior + geom_point(data = onsemi %>% filter(cgroup == input$group & site_type == input$type), 
                                     aes(Longitude, Latitude, color = site_type, shape=cgroup, 
                                         text = paste0(city, ', ', country, '\n', site_type, '\n', cgroup)), 
                                     fill = 'black', size = 3), tooltip = 'text')
      },error = function(e) {
        junior
      }
      )
    } else {
      ggplotly(junior + geom_point(data = onsemi, 
                                   aes(Longitude, Latitude, color = site_type, shape=cgroup, 
                                       text = paste0(city, ', ', country, '\n', site_type, '\n', cgroup)), 
                                   fill = 'black', size = 3), tooltip = 'text')
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


