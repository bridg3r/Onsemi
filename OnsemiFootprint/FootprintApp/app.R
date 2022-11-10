# install.packages(c( "gapminder", "ggforce", "gh", "globals", "openintro", "profvis", "RSQLite", "shiny", "shinycssloaders", "shinyFeedback",  "shinythemes", "testthat", "thematic", "tidyverse", "vroom", "waiter", "xml2", "zeallot" ))

# TO DO:
# Move everything to quarto in VS code
# add reactive events to filter to only selected location
# add in Onsemi logo title
# add in an annotion to highlight the location
# better looking markers on the map
# add dropdown for different groups
# add dropdown for different site functions
# add in clickalble links to sharepoint site
# Spice up the page more
# fix city names, use code from colab
library(tidyverse)
library(plotly)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(shiny)
library(ggrepel)

onsemi <- read_csv('./www/onsemi3.csv')

groupLinks <- tibble(cgroup = c('Advanced Solutions Group', 'Power Solutions Group', 'Intelligent Solutions Group')) %>% 
  mutate(link = case_when(cgroup == 'Advanced Solutions Group'~'https://onsemi.sharepoint.com/sites/asg', 
                          cgroup == 'Power Solutions Group'~'https://onsemi.sharepoint.com/sites/psg', cgroup =='Intelligent Solutions Group'~'https://onsemi.sharepoint.com/sites/isg')
  )



world <- ne_countries(scale = "medium", returnclass = "sf")

outline = 'white'

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

junior <- ggplot(data = world) +
  geom_sf(color = "black", fill = rgb(144, 238, 144, maxColorValue = 255)) +
  labs(shape="Group", color = "Site Function") +
  theme(panel.background = element_rect(fill = rgb(137, 207, 240, maxColorValue = 255)),
        panel.grid.major = element_line(linetype = 'dashed', size = .3),
        plot.title = element_text(hjust = 0.5), axis.title.x=element_blank(), axis.title.y=element_blank(), 
        legend.position = "none") + 
  scale_fill_manual(values = c('ATO' = rgb(255, 172, 28, maxColorValue = 255), 'other'=rgb(238, 75, 43, maxColorValue = 255), 
                               'Wafer Fab Site' = rgb(34, 139, 34, maxColorValue = 255))) +
  scale_shape_manual(values = c('ASG' = 21, 'non-ASG'= 24))

outline = 'white'

ui <- fluidPage(
  #includeScript("clickhandler.js"),
  titlePanel(imageOutput("logo", height = '50px')),
  tabsetPanel(
    tabPanel('Interactive',
             fluidRow(
               column(9,
                      #plotlyOutput("plot", width = "900px", height = '450px'), 
                      #tags$head(tags$script(src="clickhandler.js")) # https://stackoverflow.com/questions/32057164/adding-hyperlinks-to-shiny-plots
                      plotlyOutput("plot", width = "900px", height = '450px'), # make this reactive to city or group
                      #imageOutput("homepage"),
                      #tags$a(imageOutput("homepage"),href="https://www.google.com")
                      uiOutput("sharepoint")
               ), 
               column(2,
                      selectInput("group", label = "Groups", selected = 'All', choices = append(c('All'), sort(groupLinks$cgroup))),
                      selectInput("type", label = "Site Function", selected = 'All', choices = append(c('All'), sort(onsemi$site_type))),
                      selectInput("city", label = "Find One Location", selected = 'All', choices = append(c('All'), sort(onsemi$city))),
                      imageOutput("legend")
               )
             )
    ), tabPanel('Static',
                fluidRow(
                  column(9,
                         plotOutput("plot2", width = "900px", height = '450px'), 
                         uiOutput("sharepoint2")
                  ), 
                  column(2,
                         selectInput("group2", label = "Groups", selected = 'All', choices = append(c('All'), sort(groupLinks$cgroup))),
                         selectInput("type2", label = "Site Function", selected = 'All', choices = append(c('All'), sort(onsemi$site_type))),
                         selectInput("city2", label = "Find One Location", selected = 'All', choices = append(c('All'), sort(onsemi$city))),
                         imageOutput("legend2")
                  )
                )
    )
  )
)

server <- function(input, output, session) {
  output$plot <- renderPlotly( 
    if (input$city != 'All') {
      ggplotly(junior + geom_point(data = onsemi %>% filter(city == input$city), 
                                   aes(Longitude, Latitude, fill = site_type, shape=cgroup, 
                                       text = paste0(city, ', ', country, '\n', site_type, '\n', cgroup)), 
                                   color = outline, size = 3), tooltip = 'text')
    } else if (input$type != 'All' & input$group == 'All' & input$city == 'All') {
      ggplotly(junior + geom_point(data = onsemi %>% filter(site_type == input$type), 
                                   aes(Longitude, Latitude, fill = site_type, shape=cgroup, 
                                       text = paste0(city, ', ', country, '\n', site_type, '\n', cgroup)), 
                                   color = outline, size = 3), tooltip = 'text')
    } else if (input$group != 'All' & input$type == 'All' & input$city == 'All') {
      tryCatch({
      ggplotly(junior + geom_point(data = onsemi %>% filter(get(input$group) == 1), 
                                   aes(Longitude, Latitude, fill = site_type, shape=cgroup, 
                                       text = paste0(city, ', ', country, '\n', site_type, '\n', cgroup)), 
                                   color = outline, size = 3), tooltip = 'text')
      },error = function(e) {junior})
    } else if (input$group != 'All' & input$type != 'All' & input$city == 'All') {
      tryCatch({
        ggplotly(junior + geom_point(data = onsemi %>% filter(get(input$group) == 1 & site_type == input$type), 
                                     aes(Longitude, Latitude, fill = site_type, shape=cgroup, 
                                         text = paste0(city, ', ', country, '\n', site_type, '\n', cgroup)), 
                                     color = outline, size = 3), tooltip = 'text')
      },error = function(e) {junior})
    } else {
      ggplotly(junior + geom_point(data = onsemi, 
                                   aes(Longitude, Latitude, fill = site_type, shape=cgroup, 
                                       text = paste0(city, ', ', country, '\n', site_type, '\n', cgroup)), 
                                   color = outline, size = 3), tooltip = 'text')
    }
    
  )
  
  output$legend <- renderImage({
    list(
      src = file.path("./www/legend.png"),
      contentType = "image/png",
      width = 130,
      height = 200
    )
  }, deleteFile = FALSE)
  
  output$plot2 <- renderPlot( 
    if (input$city2 != 'All') {
      junior + geom_point(data = onsemi %>% filter(city == input$city2), 
                          aes(Longitude, Latitude, fill = site_type, shape=cgroup), 
                          color = outline, size = 5) +
        geom_label_repel(aes(Longitude, Latitude, label = paste0(city, ', ', country, '\n', site_type, '\n', cgroup)),
                         data = onsemi %>% filter(city==input$city2))
    } else if (input$type2 != 'All' & input$group2 == 'All' & input$city2 == 'All') {
      junior + geom_point(data = onsemi %>% filter(site_type == input$type2), 
                          aes(Longitude, Latitude, fill = site_type, shape=cgroup), 
                          color = outline, size = 5)
    } else if (input$group2 != 'All' & input$type2 == 'All' & input$city2 == 'All') {
      junior + geom_point(data = onsemi %>% filter(get(input$group2) == 1), 
                          aes(Longitude, Latitude, fill = site_type, shape=cgroup), 
                          color = outline, size = 5)
    } else if (input$group2 != 'All' & input$type2 != 'All' & input$city2 == 'All') {
      tryCatch({ # https://www.r-bloggers.com/2020/10/basic-error-handing-in-r-with-trycatch/
        junior + geom_point(data = onsemi %>% filter(get(input$group2) == 1 & site_type == input$type2), 
                            aes(Longitude, Latitude, fill = site_type, shape=cgroup), 
                            color = outline, size = 5)
      },error = function(e) {
        junior
      }
      )
    } else {
      master
    }
    
  )
  
  output$legend2 <- renderImage({
    list(
      src = file.path("./www/legend.png"),
      contentType = "image/png",
      width = 130,
      height = 200
    )
  }, deleteFile = FALSE)
  
  output$logo <- renderImage({
    list(
      src = file.path("./www/worldwidelogo.png"),
      contentType = "image/png",
      width = 500,
      height = 50
    )
  }, deleteFile = FALSE)
  
  output$homepage <- renderImage({
    list(
      src = file.path("./www/homepage.png"),
      contentType = "image/png",
      width = 130,
      height = 50
    )
  }, deleteFile = FALSE)

  
  output$sharepoint <- renderUI({
    if (input$city != 'All' & !is.na(filter(onsemi, city==input$city)[['link']][1])) {
      url <- a(paste0(input$city, " SharePoint Homepage"), href=filter(onsemi, city==input$city)[['link']][1])
    } else if (input$city == 'All' & input$group != 'All') {
      url <- a(paste0(input$group, " SharePoint Homepage"), href=filter(groupLinks, cgroup==input$group)[['link']][1])
    } else {
      url <- a("Onsemi SharePoint Homepage", href="https://onsemi.sharepoint.com/")
      tagList(url)
    }
  })
  
  output$sharepoint2 <- renderUI({
    if (input$city != 'All' & !is.na(filter(onsemi, city==input$city2)[['link']][1])) {
      url <- a(paste0(input$city2, " SharePoint Homepage"), href=filter(onsemi, city==input$city2)[['link']][1])
    } else if (input$city2 == 'All' & input$group2 != 'All') {
      url <- a(paste0(input$group2, " SharePoint Homepage"), href=filter(groupLinks, cgroup==input$group2)[['link']][1])
    } else {
      url <- a("Onsemi SharePoint Homepage", href="https://onsemi.sharepoint.com/")
      tagList(url)
    }
  })
  
}

shinyApp(ui, server)




